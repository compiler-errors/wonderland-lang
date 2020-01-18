use self::decorate::*;
use self::type_helpers::*;
use crate::inst::{
    InstEnumRepresentation, InstEnumSignature, InstObjectSignature, InstantiatedProgram,
};
use crate::parser::ast::*;
use crate::util::{IntoError, PError, PResult, ZipExact};
use inkwell::attributes::{Attribute, AttributeLoc};
use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::*;
use inkwell::values::*;
use inkwell::IntPredicate;
use std::collections::{HashMap, HashSet};
use std::ffi::{OsStr, OsString};
use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::sync::RwLock;
use tempfile::TempDir;

mod decorate;
mod type_helpers;

lazy_static! {
    static ref STDLIB_PATH: &'static OsStr = OsStr::new("std/clib/clib.c");
    static ref LIBC_PATH: &'static OsStr = if cfg!(target_os = "linux") {
        OsStr::new("/usr/lib/libc.a")
    } else if cfg!(target_os = "macos") {
        OsStr::new("/lib/libc.dylib")
    } else {
        panic!("Unsupported OS")
    };
}

struct Translator {
    context: Context,
    module: Module,
    builtin_functions: HashMap<String, FunctionValue>,

    break_continue: HashMap<LoopId, TrLoopInfo>,

    variables: HashMap<VariableId, Vec<PointerValue>>,
    globals: HashMap<ModuleRef, Vec<GlobalValue>>,
    instruction_values: HashMap<String, BasicValueEnum>,

    type_ids: HashMap<AstType, usize>,
    closure_object_ids: HashMap<usize, TrClosureCaptureEnvironment>,

    enums: HashMap<InstEnumSignature, InstEnumRepresentation>,
}

struct TrLoopInfo {
    loop_ptrs: Vec<PointerValue>,
    break_block: BasicBlock,
    continue_block: BasicBlock,
}

pub fn translate(
    file: InstantiatedProgram,
    llvm_ir: bool,
    output_file: &str,
    included_files: Vec<OsString>,
    permanent_temp_dir: Option<&str>,
) -> PResult<()> {
    let mut tr = Translator::new();
    tr.translate(file)?;

    let module = tr.module;

    if let Result::Err(why) = module.verify() {
        if output_file != "-" {
            error!("{}", module.print_to_string().to_string());
            std::io::stdout().flush().unwrap();
        }

        return PResult::error(format!("LLVM: {}", why.to_string()));
    }

    if output_file != "-" {
        emit_module(
            module,
            llvm_ir,
            output_file,
            included_files,
            permanent_temp_dir,
        )?;
    } else {
        println!("{}", module.print_to_string().to_string());
    }

    Ok(())
}

fn emit_module(
    module: Module,
    llvm_ir: bool,
    output_file: &str,
    included_files: Vec<OsString>,
    permanent_temp_dir: Option<&str>,
) -> PResult<()> {
    if llvm_ir {
        if let Err(msg) = module.print_to_file(&PathBuf::from(output_file)) {
            return PResult::error(format!(
                "There was a problem writing the assembly file to disk: {}",
                msg.to_string()
            ));
        }
    } else {
        let dir;
        let dir_path;

        if let Some(permanent_temp_dir) = permanent_temp_dir {
            dir_path = Path::new(permanent_temp_dir);

            if dir_path.exists() && !dir_path.is_dir() {
                return PResult::error(format!(
                    "The specified path {} exists and is not a directory!",
                    dir_path.display()
                ));
            }

            std::fs::create_dir_all(dir_path)
                .map_err(|e| PError::new(format!("Temp dir error: {}", e)))?;
        } else {
            dir = Some(TempDir::new().map_err(|e| PError::new(format!("Temp dir error: {}", e)))?);
            dir_path = dir.as_ref().unwrap().path();
        }

        let ll = dir_path.join("file.ll").into_os_string();
        let ll_gc = dir_path.join("file.gc.ll").into_os_string();
        let ll_o = dir_path.join("file.o").into_os_string();
        module.write_bitcode_to_path(Path::new(&ll));

        let mut opt_command = Command::new("opt".to_string());
        opt_command.args(&[
            &ll,
            // Turn all the alloca's into real phi nodes.
            OsStr::new("--mem2reg"),
            // Optimize quite aggresively. I'm afraid this will fuck up the statepoint shit
            // later, but let's hope not...
            OsStr::new("-O3"),
            // Emit a bunch of statepoint bullshit so we can walk the stack later.
            OsStr::new("--rewrite-statepoints-for-gc"),
            OsStr::new("-o"),
            &ll_gc,
        ]);

        let mut llc_command = Command::new("llc".to_string());
        llc_command.args(&[
            &ll_gc,
            OsStr::new("-o"),
            &ll_o,
            OsStr::new("-relocation-model=pic"),
            OsStr::new("-O3"),
            OsStr::new("-filetype=obj"),
        ]);

        // This poor command, unfortunately, is the reason that cheshire currently only works
        // on Linux. For some reasons LLVM exports the stackmap in some hidden symbol mode,
        // so we can't build with it unless we re-export it as globally accessible.
        let mut objcopy_command = Command::new("objcopy".to_string());
        objcopy_command.args(&[
            &ll_o,
            &ll_o,
            OsStr::new("--globalize-symbol=__LLVM_StackMaps"),
        ]);

        let mut clang_command = Command::new("clang".to_string());
        clang_command
            .args(&[
                *STDLIB_PATH,
                &ll_o,
                OsStr::new("-g"),
                OsStr::new("-fPIC"),
                OsStr::new("-flto"),
                OsStr::new("-O3"),
                OsStr::new("-o"),
                OsStr::new(output_file),
            ])
            .args(&included_files);

        execute_command(opt_command)?;
        execute_command(llc_command)?;
        execute_command(objcopy_command)?;
        execute_command(clang_command)?;
    }

    Ok(())
}

fn execute_command(mut command: Command) -> PResult<()> {
    let command = command.stdin(Stdio::null()).stdout(Stdio::null());
    info!("Executing command: {:?}", command);
    let clang_status = command
        .status()
        .map_err(|e| PError::new(format!("Command Error: {}", e)))?;
    if clang_status.success() {
        Ok(())
    } else {
        PResult::error(format!(
            "Command exited with code: {}",
            clang_status.code().unwrap_or(-1),
        ))
    }
}

impl Translator {
    fn new() -> Translator {
        let context = Context::create();
        let module = context.create_module("main");

        // Declare string type
        let string_struct = context.opaque_struct_type("string");
        string_struct.set_body(
            &[
                context.i64_type().into(),
                context.i8_type().array_type(0).into(),
            ],
            false,
        );

        let string_ty = string_struct.ptr_type(GC).into();
        let i8_ptr_global_ty = context.i8_type().ptr_type(GLOBAL).into();
        let i8_ptr_ptr_global_ty = context.i8_type().ptr_type(GLOBAL).ptr_type(GLOBAL).into();
        let i8_ptr_gc_ty = context.i8_type().ptr_type(GC).into();
        let i64_ty = context.i64_type().into();
        let i8_ty = context.i8_type().into();
        let i1_ty = context.bool_type().into();
        let i16_ty = context.i16_type().into();
        let noreturn =
            context.create_enum_attribute(Attribute::get_named_enum_kind_id("noreturn"), 1);

        let mut tr = Translator {
            module,
            context,
            builtin_functions: HashMap::new(),

            break_continue: HashMap::new(),
            variables: HashMap::new(),
            globals: HashMap::new(),
            instruction_values: HashMap::new(),

            type_ids: HashMap::new(),
            closure_object_ids: HashMap::new(),
            enums: HashMap::new(),
        };

        tr.add_function(
            "gc_alloc_string",
            string_ty,
            &[i8_ptr_global_ty, i64_ty],
            true,
        );

        tr.add_function("gc_alloc_object", i8_ptr_gc_ty, &[i64_ty, i16_ty], true);

        tr.add_function(
            "gc_alloc_array",
            i8_ptr_gc_ty,
            &[i64_ty, i64_ty, i16_ty],
            true,
        );

        tr.add_function(
            "gc_array_idx_at",
            i8_ptr_global_ty,
            &[i8_ptr_gc_ty, i64_ty],
            true,
        );

        tr.add_void_function("gc_register_slot", &[i8_ptr_ptr_global_ty, i16_ty], true);

        tr.add_function("match_panic", i8_ty, &[], true)
            .add_attribute(AttributeLoc::Function, noreturn);

        tr.add_function(
            "string_eq_literal",
            i1_ty,
            &[string_ty, i8_ptr_global_ty, i64_ty],
            true,
        );

        tr
    }

    fn add_function(
        &mut self,
        name: &str,
        return_value: BasicTypeEnum,
        parameters: &[BasicTypeEnum],
        apply_attributes: bool,
    ) -> FunctionValue {
        let f = self
            .module
            .add_function(name, fun_type(return_value, parameters), None);

        if apply_attributes {
            set_all_fn_attributes(&self.context, f);
        }

        self.builtin_functions.insert(name.to_string(), f);
        f
    }

    fn add_void_function(
        &mut self,
        name: &str,
        parameters: &[BasicTypeEnum],
        apply_attributes: bool,
    ) -> FunctionValue {
        let f = self.module.add_function(
            name,
            self.context.void_type().fn_type(parameters, false),
            None,
        );

        if apply_attributes {
            set_all_fn_attributes(&self.context, f);
        }

        self.builtin_functions.insert(name.to_string(), f);
        f
    }

    fn get_function(&self, name: &str) -> FunctionValue {
        self.builtin_functions[name]
    }

    fn translate(&mut self, mut file: InstantiatedProgram) -> PResult<()> {
        // Forward declare the type ids. Explicitly declare string as 0.
        file.instantiated_types.remove(&AstType::String);
        self.type_ids.insert(AstType::String, 0);
        for ty in file.instantiated_types.iter() {
            self.type_ids.insert(ty.clone(), new_type_id());
        }
        file.instantiated_types.insert(AstType::String);
        self.enums.extend(file.instantiated_enums);

        // Forward declaration.
        for (sig, _) in &file.instantiated_objects {
            let name = decorate_object(&sig.0, &sig.1)?;
            self.context.opaque_struct_type(&name);
        }

        for (sig, obj) in &file.instantiated_objects {
            self.translate_object(&sig, &obj)?;
        }

        for (sig, fun) in &file.instantiated_fns {
            let name = decorate_fn(&sig.0, &sig.1)?;
            self.forward_declare_function(
                &name,
                &fun.parameter_list,
                &fun.return_type,
                fun.definition.is_none(),
            )?;
        }

        for (sig, fun) in &file.instantiated_object_fns {
            let name = decorate_object_fn(&sig.0, &sig.1, &sig.2, &sig.3)?;
            self.forward_declare_function(
                &name,
                &fun.parameter_list,
                &fun.return_type,
                fun.definition.is_none(),
            )?;
        }

        for (name, global) in &file.instantiated_globals {
            self.forward_declare_global(&name, &global.ty)?;
        }

        for (sig, fun) in file.instantiated_fns {
            let name = decorate_fn(&sig.0, &sig.1)?;

            self.translate_function(
                &name,
                &fun.parameter_list,
                &fun.return_type,
                &fun.variables,
                fun.definition.as_ref(),
            )?;
        }

        for (sig, fun) in file.instantiated_object_fns {
            let name = decorate_object_fn(&sig.0, &sig.1, &sig.2, &sig.3)?;

            self.translate_function(
                &name,
                &fun.parameter_list,
                &fun.return_type,
                &fun.variables,
                fun.definition.as_ref(),
            )?;
        }

        self.translate_main(&file.main_fn, &file.instantiated_globals)?;

        // This must happen last, so we can make sure that all of the closure capture objects have been created already.
        self.translate_gc_visit(&file.instantiated_types, &file.instantiated_objects)?;

        Ok(())
    }

    fn translate_main(
        &mut self,
        main_fn: &ModuleRef,
        globals: &HashMap<ModuleRef, AstGlobalVariable>,
    ) -> PResult<()> {
        let main_fn_name = decorate_fn(main_fn, &[])?;
        let cheshire_main_fn = self.module.get_function(&main_fn_name).unwrap();

        let real_main_fn = self.forward_declare_function("main", &[], &AstType::Int, false)?;
        set_cheshire_fn_attributes(&self.context, real_main_fn);
        let block = real_main_fn.append_basic_block("pre");
        let first_builder = Builder::create();
        first_builder.position_at_end(&block);

        // Initialize all of our globals here.
        for (name, g) in globals {
            let values = self.translate_expression(&first_builder, &g.init)?;

            for (ptr, v) in ZipExact::zip_exact(&self.globals[name], values, "globals")? {
                first_builder.build_store(ptr.as_pointer_value(), v);
            }

            for (ptr, ty) in ZipExact::zip_exact(
                &self.globals[name],
                self.flatten_ty(&g.ty),
                "flat global types",
            )? {
                if ptr
                    .as_pointer_value()
                    .get_type()
                    .get_element_type()
                    .is_pointer_type()
                {
                    let lval = first_builder.build_pointer_cast(
                        ptr.as_pointer_value(),
                        self.context.i8_type().ptr_type(GLOBAL).ptr_type(GLOBAL),
                        &temp_name(),
                    );
                    let type_id = self
                        .context
                        .i16_type()
                        .const_int(self.type_ids[&ty] as u64, false);

                    first_builder.build_call(
                        self.get_function("gc_register_slot"),
                        &[lval.into(), type_id.into()],
                        &temp_name(),
                    );
                }
            }
        }

        let ret = first_builder.build_call(cheshire_main_fn, &[], &temp_name());
        first_builder.build_return(Some(&unwrap_callsite(ret)));

        Ok(())
    }

    fn forward_declare_function(
        &mut self,
        name: &str,
        parameter_list: &[AstNamedVariable],
        return_type: &AstType,
        _exported: bool,
    ) -> PResult<FunctionValue> {
        let param_tys = parameter_list
            .iter()
            .map(|p| self.get_llvm_ty(&p.ty))
            .collect::<PResult<Vec<_>>>()?;
        let ret = self.get_llvm_ty(return_type)?;
        let fun_ty = fun_type(ret, &param_tys);

        let llvm_fun = self.module.add_function(&name, fun_ty, None);

        set_all_fn_attributes(&self.context, llvm_fun);

        Ok(llvm_fun)
    }

    fn forward_declare_global(&mut self, name: &ModuleRef, ty: &AstType) -> PResult<()> {
        self.globals.insert(name.clone(), self.flatten_globals(ty)?);
        Ok(())
    }

    fn translate_object(&mut self, sig: &InstObjectSignature, obj: &AstObject) -> PResult<()> {
        let name = decorate_object(&sig.0, &sig.1)?;
        let ty = self.module.get_type(&name).unwrap().into_struct_type();

        let tys = obj
            .members
            .iter()
            .map(|m| self.get_llvm_ty(&m.member_type))
            .collect::<PResult<Vec<BasicTypeEnum>>>()?;
        ty.set_body(&tys, false);

        Ok(())
    }

    fn translate_function(
        &mut self,
        fun_name: &str,
        parameter_list: &[AstNamedVariable],
        return_ty: &AstType,
        variables: &HashMap<VariableId, AstNamedVariable>,
        definition: Option<&AstBlock>,
    ) -> PResult<()> {
        let llvm_fun = self.module.get_function(&fun_name).unwrap();

        if let Some(definition) = definition {
            set_cheshire_fn_attributes(&self.context, llvm_fun);

            let param_values: HashMap<_, _> =
                ZipExact::zip_exact(parameter_list, &llvm_fun.get_params(), "params")?
                    .map(|(p, v)| (p.id, v.clone()))
                    .collect();

            let block = llvm_fun.append_basic_block("pre");
            let first_builder = Builder::create();
            first_builder.position_at_end(&block);

            for (id, var) in variables {
                let ty = self.get_llvm_ty(&var.ty)?;
                let val = param_values
                    .get(&id)
                    .map(|f| Ok(f.clone()))
                    .unwrap_or_else(|| type_zero(ty))?;

                let ptrs = self.flatten_alloca(&first_builder, val, &var.ty)?;
                self.variables.insert(*id, ptrs);
            }

            let start_block = llvm_fun.append_basic_block("start");
            first_builder.build_unconditional_branch(&start_block);

            let start_builder = Builder::create();
            start_builder.position_at_end(&start_block);

            let ret = self.translate_block(&start_builder, &definition)?;
            let ret = self.bundle_vals(&start_builder, &ret, return_ty)?;

            start_builder.build_return(Some(&ret));
        }

        Ok(())
    }

    fn translate_block(
        &mut self,
        builder: &Builder,
        block: &AstBlock,
    ) -> PResult<Vec<BasicValueEnum>> {
        for s in &block.statements {
            self.translate_statement(&builder, s)?;
        }

        self.translate_expression(&builder, &block.expression)
    }

    fn translate_statement(&mut self, builder: &Builder, statement: &AstStatement) -> PResult<()> {
        match statement {
            // Removed in earlier stages
            AstStatement::Assert { .. } => unimplemented!(),

            AstStatement::Let { pattern, value } => {
                let values = &self.translate_expression(builder, value)?;
                self.translate_pattern(builder, pattern, values, None)?
            }

            AstStatement::Expression { expression } => {
                self.translate_expression(builder, expression)?;
            }

            AstStatement::Break { id, value, .. } => {
                let value = self.translate_expression(builder, value)?;
                let info = &self.break_continue[&id.unwrap()];

                for (ptr, val) in ZipExact::zip_exact(&info.loop_ptrs, value, "loop values")? {
                    builder.build_store(*ptr, val);
                }

                builder.build_unconditional_branch(&info.break_block);

                let (fake_block, _) = self.get_new_block(builder)?;
                builder.position_at_end(&fake_block);
            }
            AstStatement::Continue { id, .. } => {
                let info = &self.break_continue[&id.unwrap()];
                builder.build_unconditional_branch(&info.continue_block);

                let (fake_block, _) = self.get_new_block(builder)?;
                builder.position_at_end(&fake_block);
            }
            AstStatement::Return { value } => {
                let val = self.translate_expression(builder, value)?;
                let ret = self.bundle_vals(builder, &val, &value.ty)?;

                builder.build_return(Some(&ret));

                let (fake_block, _) = self.get_new_block(builder)?;
                builder.position_at_end(&fake_block);
            }
        }

        Ok(())
    }

    fn translate_expression(
        &mut self,
        builder: &Builder,
        expression: &AstExpression,
    ) -> PResult<Vec<BasicValueEnum>> {
        let value: Vec<BasicValueEnum> = match &expression.data {
            AstExpressionData::SelfRef
            | AstExpressionData::Unimplemented
            | AstExpressionData::ExprCall { .. }
            | AstExpressionData::NamedEnum { .. }
            | AstExpressionData::PlainEnum { .. }
            | AstExpressionData::AllocateArray { .. }
            | AstExpressionData::As { .. }
            | AstExpressionData::For { .. } => unreachable!(),

            c @ AstExpressionData::Closure { .. } => {
                let env = self.translate_closure_capture_environment(c)?;
                let fun = self.translate_closure_body(&env, c)?;
                let c = self.translate_closure_construction(builder, fun, &env, c)?;

                // Cast this to the expected |...| type.
                let opaque_closure = builder.build_pointer_cast(
                    c,
                    self.get_llvm_ty(&expression.ty)?.into_pointer_type(),
                    &temp_name(),
                );
                vec![opaque_closure.into()]
            }

            AstExpressionData::Match {
                expression: match_expression,
                branches,
            } => {
                let match_values = self.translate_expression(builder, &match_expression)?;
                let (after_block, _) = self.get_new_block(builder)?;
                let (mut fallthrough_block, _) = self.get_new_block(builder)?;

                let result_ptrs = self.flatten_alloca(
                    builder,
                    type_undefined(self.get_llvm_ty(&expression.ty)?)?,
                    &expression.ty,
                )?;

                for AstMatchBranch {
                    pattern,
                    expression,
                } in branches
                {
                    self.translate_pattern(
                        builder,
                        pattern,
                        &match_values,
                        Some(&fallthrough_block),
                    )?;
                    let (success_block, _) = self.get_new_block(builder)?;
                    builder.build_unconditional_branch(&success_block);
                    builder.position_at_end(&success_block);

                    let vals = self.translate_expression(builder, expression)?;
                    for (ptr, val) in ZipExact::zip_exact(&result_ptrs, vals, "match value ptrs")? {
                        builder.build_store(*ptr, val);
                    }

                    // We may have branched from the original success since we emitted the branch
                    // to it, so let's capture the new block at the tip of the builder.
                    builder.build_unconditional_branch(&after_block);

                    builder.position_at_end(&fallthrough_block);
                    let (new_fallthrough_block, _) = self.get_new_block(builder)?;
                    fallthrough_block = new_fallthrough_block;
                }

                // Fallthrough block -> panic
                builder.build_unconditional_branch(&fallthrough_block);
                builder.position_at_end(&fallthrough_block);
                builder.build_call(self.get_function("match_panic"), &[], &temp_name());
                builder.build_unconditional_branch(&after_block);

                // Otherwise, we successfully matched, let's go unify all the returns.
                builder.position_at_end(&after_block);

                result_ptrs
                    .into_iter()
                    .map(|ptr| builder.build_load(ptr, &temp_name()))
                    .collect()
            }
            AstExpressionData::PositionalEnum {
                enumerable,
                generics,
                variant,
                children,
            } => {
                let mut flat_children = Vec::new();
                for child in children {
                    flat_children.extend(self.translate_expression(builder, child)?);
                }

                let en_info = &self.enums[&InstEnumSignature(enumerable.clone(), generics.clone())];
                let mut fields: Vec<_> = (0..en_info.fields.len()).map(|_| None).collect();

                // Set the discriminant, always the first value.
                let disc = en_info.discriminants[variant];
                fields[0] = Some(self.context.i64_type().const_int(disc, false).into());

                for (idx, child) in ZipExact::zip_exact(
                    &en_info.variants[variant],
                    flat_children,
                    "flat enum children",
                )? {
                    fields[*idx] = Some(child);
                }

                for i in 0..fields.len() {
                    if fields[i].is_none() {
                        let field_ty = self.get_llvm_ty(&en_info.fields[i])?;
                        let default_val = type_zero(field_ty)?;
                        fields[i] = Some(default_val);
                    }
                }

                fields.into_iter().map(Option::unwrap).collect()
            }

            AstExpressionData::GlobalFn { name } => {
                let name = decorate_fn(name, &[])?;

                vec![self
                    .module
                    .get_function(&name)
                    .unwrap()
                    .as_global_value()
                    .as_pointer_value()
                    .into()]
            }

            AstExpressionData::Literal(lit) => self.translate_literal(builder, lit)?,

            // Lval-always expressions. Can be calculated by getting the lval then deref'ing.
            AstExpressionData::Identifier { .. }
            | AstExpressionData::ObjectAccess { .. }
            | AstExpressionData::GlobalVariable { .. } => self
                .translate_expression_lval(builder, expression)?
                .iter()
                .map(|e| builder.build_load(*e, &temp_name()))
                .collect(),

            AstExpressionData::ArrayAccess { .. } => unreachable!(),

            AstExpressionData::Tuple { values } => {
                let mut tup = Vec::new();
                for v in values {
                    tup.extend(self.translate_expression(builder, v)?);
                }

                tup
            }

            AstExpressionData::ArrayLiteral { elements } => {
                let element_ast_ty = AstType::get_element(&expression.ty)?;
                let elements: Vec<Vec<BasicValueEnum>> = elements
                    .iter()
                    .map(|e| self.translate_expression(builder, e))
                    .collect::<PResult<_>>()?;

                // Allocating the array.
                let element_ty = self.get_llvm_ty(&element_ast_ty)?;
                let array_ty = self.get_llvm_ty(&expression.ty)?;
                let ty_size = type_size(element_ty)?;
                let num_elements = self
                    .context
                    .i64_type()
                    .const_int(elements.len() as u64, false);

                let ty_id = self.type_ids[&expression.ty];
                let ptr = builder.build_call(
                    self.get_function("gc_alloc_array"),
                    &[
                        ty_size.into(),
                        num_elements.into(),
                        self.context
                            .i16_type()
                            .const_int(ty_id as u64, false)
                            .into(),
                    ],
                    &temp_name(),
                );

                let ptr = builder.build_pointer_cast(
                    unwrap_callsite(ptr).into_pointer_value(),
                    array_ty.into_pointer_type(),
                    &temp_name(),
                );

                // Used in GEP
                let zero = self.context.i32_type().const_int(0 as u64, false);
                let two = self.context.i32_type().const_int(2 as u64, false);

                for (idx, vals) in elements.iter().enumerate() {
                    let idx = self.context.i64_type().const_int(idx as u64, false);
                    let elem_ptr =
                        unsafe { builder.build_gep(ptr, &[zero, two, idx], &temp_name()) };
                    let val = self.bundle_vals(builder, vals, &element_ast_ty)?;
                    builder.build_store(elem_ptr, val);
                }

                vec![ptr.into()]
            }

            AstExpressionData::FnCall {
                fn_name,
                generics,
                args,
            } => {
                let name = decorate_fn(fn_name, generics)?;
                let fun = self.module.get_function(&name).unwrap();

                let arg_tys: Vec<_> = args.iter().map(|e| e.ty.clone()).collect();
                let args: Vec<Vec<BasicValueEnum>> = args
                    .iter()
                    .map(|e| self.translate_expression(builder, e))
                    .collect::<PResult<_>>()?;

                // I need to bundle AFTER all of the expressions have been evaluated.
                // We never want to be holding a bundled value through a possibly GC'able call.
                let mut bundled_args = Vec::new();
                for (e, ty) in ZipExact::zip_exact(args, arg_tys, "arguments")? {
                    bundled_args.push(self.bundle_vals(builder, &e, &ty)?);
                }

                let fn_ret = builder.build_call(fun, &bundled_args, &temp_name());
                self.flatten_val(builder, unwrap_callsite(fn_ret), &expression.ty)?
            }

            AstExpressionData::ObjectCall { .. } => unreachable!(),

            AstExpressionData::StaticCall {
                call_type,
                fn_name,
                fn_generics,
                args,
                associated_trait,
                ..
            } => {
                let name = decorate_object_fn(
                    call_type,
                    &associated_trait.as_ref().unwrap().trt,
                    fn_name,
                    fn_generics,
                )?;
                let fun = self.module.get_function(&name).unwrap();

                let arg_tys: Vec<_> = args.iter().map(|e| e.ty.clone()).collect();
                let args: Vec<Vec<BasicValueEnum>> = args
                    .iter()
                    .map(|e| self.translate_expression(builder, e))
                    .collect::<PResult<_>>()?;

                // I need to bundle AFTER all of the expressions have been evaluated.
                // We never want to be holding a bundled value through a possibly GC'able call.
                let mut bundled_args = Vec::new();
                for (e, ty) in ZipExact::zip_exact(args, arg_tys, "arguments")? {
                    bundled_args.push(self.bundle_vals(builder, &e, &ty)?);
                }

                let fn_ret = builder.build_call(fun, &bundled_args, &temp_name());
                self.flatten_val(builder, unwrap_callsite(fn_ret), &expression.ty)?
            }

            AstExpressionData::TupleAccess { accessible, idx } => {
                let tup = self.translate_expression(builder, &accessible)?;

                if let AstType::Tuple { types } = &accessible.ty {
                    let begin = types[0..*idx].iter().map(|t| self.num_subvals(t)).sum();
                    let end = begin + self.num_subvals(&types[*idx]);

                    tup[begin..end].iter().cloned().collect()
                } else {
                    unreachable!()
                }
            }

            AstExpressionData::AllocateObject {
                object,
                generics,
                children,
                children_idxes,
            } => {
                // I don't like this.
                let ptr_type = self
                    .get_llvm_ty(&AstType::object(object.clone(), generics.clone()))?
                    .into_pointer_type();
                let children_idxes = children_idxes.as_ref().unwrap();

                let mut children_values = HashMap::new();
                for (name, child) in children {
                    children_values
                        .insert(name.clone(), self.translate_expression(builder, child)?);
                }

                let size = type_size(ptr_type.get_element_type().into_struct_type().into())?;
                let ty_id = self.type_ids[&expression.ty];

                let val = builder.build_call(
                    self.get_function("gc_alloc_object"),
                    &[
                        size.into(),
                        self.context
                            .i16_type()
                            .const_int(ty_id as u64, false)
                            .into(),
                    ],
                    &temp_name(),
                );
                let ptr = unwrap_callsite(val);

                let object =
                    builder.build_pointer_cast(ptr.into_pointer_value(), ptr_type, &temp_name());

                for (name, child_values) in children_values {
                    let idx = children_idxes[&name];
                    let member_ptr =
                        unsafe { builder.build_struct_gep(object, idx as u32, &temp_name()) };
                    let flat_member_ptrs =
                        self.flatten_ptr(builder, member_ptr, &children[&name].ty)?;

                    for (ptr, value) in
                        ZipExact::zip_exact(flat_member_ptrs, child_values, "flat object members")?
                    {
                        builder.build_store(ptr, value);
                    }
                }

                vec![object.into()]
            }

            AstExpressionData::Not(_expr) => unreachable!(),
            AstExpressionData::Negate(_expr) => unreachable!(),

            AstExpressionData::Assign { lhs, rhs } => {
                let rvals = self.translate_expression(builder, &rhs)?;
                let lvals = self.translate_expression_lval(builder, &lhs)?;

                for (lval, rval) in ZipExact::zip_exact(lvals, &rvals, "values")? {
                    builder.build_store(lval, *rval);
                }

                return Ok(rvals);
            }

            AstExpressionData::BinOp { .. } => unreachable!(),

            AstExpressionData::Block { block } => {
                return self.translate_block(builder, block);
            }

            AstExpressionData::If {
                condition,
                block: then_ast_block,
                else_block: else_ast_block,
            } => {
                let (then_block, then_builder) = self.get_new_block(builder)?;
                let (else_block, else_builder) = self.get_new_block(builder)?;
                let (after_block, _) = self.get_new_block(builder)?;

                let condition = self.translate_expression(builder, &condition)?;
                assert_eq!(condition.len(), 1); // This is a boolean.
                let condition = condition[0];

                builder.build_conditional_branch(
                    condition.into_int_value(),
                    &then_block,
                    &else_block,
                );

                let then_values = self.translate_block(&then_builder, &then_ast_block)?;
                // We may have branched from the original block since we emitted the branch
                // to it, so let's capture the new block at the tip of the builder.
                let then_block = then_builder.get_insert_block().unwrap();
                then_builder.build_unconditional_branch(&after_block);

                let else_values = self.translate_block(&else_builder, &else_ast_block)?;
                // DITTO the note above about branch divergence.
                let else_block = else_builder.get_insert_block().unwrap();
                else_builder.build_unconditional_branch(&after_block);

                builder.position_at_end(&after_block);

                // Collect all of the flattened values in a series of phis.
                ZipExact::zip_exact(then_values, else_values, "subvalues")?
                    .map(|(then_subval, else_subval)| {
                        let subphi = builder.build_phi(then_subval.get_type(), &temp_name());

                        subphi.add_incoming(&[
                            (&then_subval, &then_block),
                            (&else_subval, &else_block),
                        ]);
                        subphi.as_basic_value()
                    })
                    .collect()
            }

            AstExpressionData::While {
                id,
                condition,
                block,
                else_block,
                ..
            } => {
                let (condition_block, condition_builder) = self.get_new_block(builder)?;
                let (loop_block, loop_builder) = self.get_new_block(builder)?;
                let (fail_block, fail_builder) = self.get_new_block(builder)?;
                let (end_block, _) = self.get_new_block(builder)?;

                let ptrs = self.flatten_alloca(
                    builder,
                    type_undefined(self.get_llvm_ty(&expression.ty)?)?,
                    &expression.ty,
                )?;

                self.break_continue.insert(
                    *id,
                    TrLoopInfo {
                        loop_ptrs: ptrs.clone(),
                        break_block: end_block,
                        continue_block: condition_block,
                    },
                );

                builder.build_unconditional_branch(&condition_block);

                let condition = self.translate_expression(&condition_builder, condition)?;
                assert_eq!(condition.len(), 1); // This is a boolean.
                let condition = condition[0];
                condition_builder.build_conditional_branch(
                    condition.into_int_value(),
                    &loop_block,
                    &fail_block,
                );

                self.translate_block(&loop_builder, block)?;
                loop_builder.build_unconditional_branch(&condition_block);

                let fail_values = self.translate_block(&fail_builder, else_block)?;
                for (ptr, val) in ZipExact::zip_exact(&ptrs, fail_values, "fail values")? {
                    fail_builder.build_store(*ptr, val);
                }

                fail_builder.build_unconditional_branch(&end_block);

                builder.position_at_end(&end_block);

                ptrs.into_iter()
                    .map(|ptr| builder.build_load(ptr, &temp_name()))
                    .collect()
            }
            AstExpressionData::Instruction {
                instruction,
                arguments,
                output,
            } => self.translate_instruction(builder, &instruction, &arguments, output)?,
        };

        Ok(value)
    }

    fn translate_instruction(
        &mut self,
        builder: &Builder,
        instruction: &str,
        arguments: &[InstructionArgument],
        output: &InstructionOutput,
    ) -> PResult<Vec<BasicValueEnum>> {
        let output_value: BasicValueEnum = match (instruction, arguments) {
            ("neg", [a]) => {
                let a = self
                    .translate_instruction_argument(builder, a)?
                    .into_int_value();
                builder.build_int_neg(a, &temp_name()).into()
            }
            ("sext", [a, InstructionArgument::Type(ty)]) => {
                let a = self
                    .translate_instruction_argument(builder, a)?
                    .into_int_value();
                let ty = self.get_llvm_ty(ty)?.into_int_type();
                builder.build_int_s_extend(a, ty, &temp_name()).into()
            }
            ("add", [a, b]) => {
                let a = self
                    .translate_instruction_argument(builder, a)?
                    .into_int_value();
                let b = self
                    .translate_instruction_argument(builder, b)?
                    .into_int_value();
                builder.build_int_add(a, b, &temp_name()).into()
            }
            ("sub", [a, b]) => {
                let a = self
                    .translate_instruction_argument(builder, a)?
                    .into_int_value();
                let b = self
                    .translate_instruction_argument(builder, b)?
                    .into_int_value();
                builder.build_int_add(a, b, &temp_name()).into()
            }
            ("mul", [a, b]) => {
                let a = self
                    .translate_instruction_argument(builder, a)?
                    .into_int_value();
                let b = self
                    .translate_instruction_argument(builder, b)?
                    .into_int_value();
                builder.build_int_mul(a, b, &temp_name()).into()
            }
            ("sdiv", [a, b]) => {
                let a = self
                    .translate_instruction_argument(builder, a)?
                    .into_int_value();
                let b = self
                    .translate_instruction_argument(builder, b)?
                    .into_int_value();
                builder.build_int_signed_div(a, b, &temp_name()).into()
            }
            ("srem", [a, b]) => {
                let a = self
                    .translate_instruction_argument(builder, a)?
                    .into_int_value();
                let b = self
                    .translate_instruction_argument(builder, b)?
                    .into_int_value();
                builder.build_int_signed_rem(a, b, &temp_name()).into()
            }
            ("icmp eq", [a, b]) => {
                let a = self
                    .translate_instruction_argument(builder, a)?
                    .into_int_value();
                let b = self
                    .translate_instruction_argument(builder, b)?
                    .into_int_value();
                builder
                    .build_int_compare(IntPredicate::EQ, a, b, &temp_name())
                    .into()
            }
            ("icmp sgt", [a, b]) => {
                let a = self
                    .translate_instruction_argument(builder, a)?
                    .into_int_value();
                let b = self
                    .translate_instruction_argument(builder, b)?
                    .into_int_value();
                builder
                    .build_int_compare(IntPredicate::SGT, a, b, &temp_name())
                    .into()
            }
            ("load", [a]) => {
                let a = self
                    .translate_instruction_argument(builder, a)?
                    .into_pointer_value();
                builder.build_load(a, &temp_name()).into()
            }
            ("store", [ptr, value]) => {
                let ptr = self
                    .translate_instruction_argument(builder, ptr)?
                    .into_pointer_value();
                let value = self.translate_instruction_argument(builder, value)?;
                builder.build_store(ptr, value);
                type_undefined(self.context.struct_type(&[], false).into())?
            }
            ("call", _) if arguments.len() > 1 => {
                let callsite = if let InstructionArgument::Expression(AstExpression {
                    data: AstExpressionData::Literal(AstLiteral::String(fn_name)),
                    ..
                }) = &arguments[0]
                {
                    let arguments =
                        self.translate_instruction_arguments(builder, &arguments[1..])?;
                    let fun = self.module.get_function(&fn_name).unwrap();
                    builder.build_call(fun, &arguments, &temp_name()).into()
                } else {
                    let fn_and_arguments =
                        self.translate_instruction_arguments(builder, arguments)?;
                    builder.build_call(
                        fn_and_arguments[0].into_pointer_value(),
                        &fn_and_arguments[1..],
                        &temp_name(),
                    )
                };

                self.translate_instruction_callsite(output, callsite)?
                    .into()
            }
            ("getelementptr", _) if arguments.len() > 1 => {
                let ptr = self
                    .translate_instruction_argument(builder, &arguments[0])?
                    .into_pointer_value();
                let idxes =
                    self.translate_instruction_arguments_to_ints(builder, &arguments[1..])?;
                unsafe { builder.build_gep(ptr, &idxes, &temp_name()) }.into()
            }
            ("pointercast", [arg, InstructionArgument::Type(ty)]) => {
                let ptr = self
                    .translate_instruction_argument(builder, arg)?
                    .into_pointer_value();
                let ptr_ty = self.get_llvm_ty(ty)?.into_pointer_type();
                builder.build_pointer_cast(ptr, ptr_ty, &temp_name()).into()
            }
            ("ch_typesize", [InstructionArgument::Type(ty)]) => {
                type_size(self.get_llvm_ty(ty)?)?.into()
            }
            ("ch_typeid", [InstructionArgument::Type(ty)]) => {
                let id = self.type_ids[&ty];
                self.context.i16_type().const_int(id as u64, false).into()
            }
            _ => {
                return PResult::error(format!(
                    "Unknown instruction `{}`, {:?} -> {:?}",
                    instruction, arguments, output
                ));
            }
        };

        match output {
            InstructionOutput::Type(output_ty) => {
                self.flatten_val(builder, output_value, output_ty)
            }
            InstructionOutput::Anonymous(output_name) => {
                self.instruction_values
                    .insert(output_name.clone(), output_value);
                Ok(vec![type_undefined(
                    self.context.struct_type(&[], false).into(),
                )?])
            }
        }
    }

    fn translate_instruction_argument(
        &mut self,
        builder: &Builder,
        arg: &InstructionArgument,
    ) -> PResult<BasicValueEnum> {
        match arg {
            InstructionArgument::Anonymous(input_name) => Ok(self.instruction_values[input_name]),
            InstructionArgument::Expression(e) => {
                let vals = self.translate_expression(builder, e)?;
                self.bundle_vals(builder, &vals, &e.ty)
            }
            InstructionArgument::Type(t) => PResult::error(format!(
                "Can't translate a type `{}` into an instruction value",
                t
            )),
        }
    }

    fn translate_instruction_arguments(
        &mut self,
        builder: &Builder,
        arg: &[InstructionArgument],
    ) -> PResult<Vec<BasicValueEnum>> {
        arg.iter()
            .map(|arg| self.translate_instruction_argument(builder, arg))
            .collect::<PResult<Vec<_>>>()
    }

    fn translate_instruction_arguments_to_ints(
        &mut self,
        builder: &Builder,
        args: &[InstructionArgument],
    ) -> PResult<Vec<IntValue>> {
        let mut output = Vec::new();

        for arg in args {
            if let InstructionArgument::Expression(AstExpression {
                data: AstExpressionData::Literal(AstLiteral::Int(i)),
                ..
            }) = arg
            {
                output.push(
                    self.context
                        .i32_type()
                        .const_int_from_string(&i, StringRadix::Decimal)
                        .unwrap(),
                );
            } else {
                let expr = self
                    .translate_instruction_argument(builder, arg)?
                    .into_int_value();
                output.push(builder.build_int_truncate_or_bit_cast(
                    expr,
                    self.context.i32_type(),
                    &temp_name(),
                ));
            }
        }

        Ok(output)
    }

    fn translate_instruction_callsite(
        &self,
        output: &InstructionOutput,
        callsite: CallSiteValue,
    ) -> PResult<BasicValueEnum> {
        match output {
            InstructionOutput::Type(AstType::Tuple { types }) if types.is_empty() => {
                type_undefined(self.context.struct_type(&[], false).into())
            }
            _ => Ok(unwrap_callsite(callsite)),
        }
    }

    fn translate_literal(
        &mut self,
        builder: &Builder,
        lit: &AstLiteral,
    ) -> PResult<Vec<BasicValueEnum>> {
        let vals = match lit {
            AstLiteral::True => vec![self.context.bool_type().const_int(1, false).into()],
            AstLiteral::False => vec![self.context.bool_type().const_int(0, false).into()],

            AstLiteral::String(string) => {
                let string_const = self.context.const_string(&string, false);
                let string_global =
                    self.module
                        .add_global(string_const.get_type(), None, &temp_name());
                string_global.set_initializer(&string_const);

                // Access the [N x i8]* as a i8*
                let zero = self.context.i64_type().const_int(0, false);
                let string_ptr = unsafe {
                    builder.build_gep(
                        string_global.as_pointer_value(),
                        &[zero, zero],
                        &temp_name(),
                    )
                };

                let len = self
                    .context
                    .i64_type()
                    .const_int(string.len() as u64, false);
                let fn_ret = builder.build_call(
                    self.get_function("gc_alloc_string"),
                    &[string_ptr.into(), len.into()],
                    &temp_name(),
                );

                vec![unwrap_callsite(fn_ret)]
            }

            AstLiteral::Int(i) => vec![self
                .context
                .i64_type()
                .const_int_from_string(&i, StringRadix::Decimal)
                .unwrap()
                .into()],
            AstLiteral::Char(c) => {
                let mut k = [0u8];
                c.encode_utf8(&mut k);
                vec![self.context.i8_type().const_int(k[0] as u64, false).into()]
            }
        };

        Ok(vals)
    }

    fn translate_pattern(
        &mut self,
        builder: &Builder,
        pattern: &AstMatchPattern,
        values: &[BasicValueEnum],
        bail_block: Option<&BasicBlock>,
    ) -> PResult<()> {
        match &pattern.data {
            AstMatchPatternData::Underscore => {}
            AstMatchPatternData::NamedEnum { .. } | AstMatchPatternData::PlainEnum { .. } => {
                unreachable!()
            }
            AstMatchPatternData::PositionalEnum {
                enumerable,
                generics,
                variant,
                children,
                ..
            } => {
                let en_info = &self.enums[&InstEnumSignature(enumerable.clone(), generics.clone())];

                let (success_block, _) = self.get_new_block(builder)?;
                let discriminant = en_info.discriminants[variant];
                let variant_remapping = &en_info.variants[variant];

                if en_info.variants.len() == 1 {
                    // Infallible, no need to test anything at all.
                    builder.build_unconditional_branch(&success_block);
                } else {
                    let cmp = builder.build_int_compare(
                        IntPredicate::EQ,
                        values[0].into_int_value(),
                        self.context.i64_type().const_int(discriminant, false),
                        &temp_name(),
                    );
                    builder.build_conditional_branch(cmp, &success_block, bail_block.unwrap());
                }

                builder.position_at_end(&success_block);

                let mut consumed = 0;
                let remapped_values: Vec<_> =
                    variant_remapping.iter().map(|i| values[*i]).collect();

                for child in children {
                    let child_size = self.num_subvals(&child.ty);
                    self.translate_pattern(
                        builder,
                        child,
                        &remapped_values[consumed..consumed + child_size],
                        bail_block,
                    )?;
                    consumed += child_size;
                }
            }
            AstMatchPatternData::Tuple(children) => {
                let mut consumed = 0; // Skip the discriminant

                for child in children {
                    let child_size = self.num_subvals(&child.ty);
                    self.translate_pattern(
                        builder,
                        child,
                        &values[consumed..consumed + child_size],
                        bail_block,
                    )?;
                    consumed += child_size;
                }
            }
            AstMatchPatternData::Identifier(var) => {
                for (ptr, val) in
                    ZipExact::zip_exact(&self.variables[&var.id], values, "flattened values")?
                {
                    builder.build_store(*ptr, *val);
                }
            }
            AstMatchPatternData::Literal(lit) => {
                assert_eq!(values.len(), 1);
                let value = values[0];

                let (success_block, _) = self.get_new_block(builder)?;

                let predicate = match lit {
                    AstLiteral::True => builder.build_int_compare(
                        IntPredicate::EQ,
                        value.into_int_value(),
                        self.context.bool_type().const_int(1, false),
                        &temp_name(),
                    ),
                    AstLiteral::False => builder.build_int_compare(
                        IntPredicate::EQ,
                        value.into_int_value(),
                        self.context.bool_type().const_int(0, false),
                        &temp_name(),
                    ),
                    AstLiteral::Int(i) => builder.build_int_compare(
                        IntPredicate::EQ,
                        value.into_int_value(),
                        self.context
                            .bool_type()
                            .const_int_from_string(&i, StringRadix::Decimal)
                            .unwrap(),
                        &temp_name(),
                    ),
                    AstLiteral::Char(c) => {
                        let mut k = [0u8];
                        c.encode_utf8(&mut k);
                        builder.build_int_compare(
                            IntPredicate::EQ,
                            value.into_int_value(),
                            self.context.i8_type().const_int(k[0] as u64, false),
                            &temp_name(),
                        )
                    }
                    AstLiteral::String(string) => {
                        let string_const = self.context.const_string(&string, false);
                        let string_global =
                            self.module
                                .add_global(string_const.get_type(), None, &temp_name());
                        string_global.set_initializer(&string_const);

                        // Access the [N x i8]* as a i8*
                        let zero = self.context.i64_type().const_int(0, false);
                        let string_ptr = unsafe {
                            builder.build_gep(
                                string_global.as_pointer_value(),
                                &[zero, zero],
                                &temp_name(),
                            )
                        };

                        let len = self
                            .context
                            .i64_type()
                            .const_int(string.len() as u64, false);
                        let fn_ret = builder.build_call(
                            self.get_function("string_eq_literal"),
                            &[value, string_ptr.into(), len.into()],
                            &temp_name(),
                        );
                        unwrap_callsite(fn_ret).into_int_value()
                    }
                };

                builder.build_conditional_branch(predicate, &success_block, bail_block.unwrap());
                builder.position_at_end(&success_block);
            }
        }

        Ok(())
    }

    fn translate_expression_lval(
        &mut self,
        builder: &Builder,
        expression: &AstExpression,
    ) -> PResult<Vec<PointerValue>> {
        match &expression.data {
            AstExpressionData::Identifier { variable_id, .. } => {
                let variable_id = &variable_id.unwrap();
                Ok(self.variables[variable_id].clone())
            }
            AstExpressionData::ObjectAccess {
                object, mem_idx, ..
            } => {
                let object = self.translate_expression(builder, &object)?;
                assert_eq!(object.len(), 1);
                let object = object[0].into_pointer_value();

                let member = unsafe {
                    builder.build_struct_gep(object, mem_idx.unwrap() as u32, &temp_name())
                };

                self.flatten_ptr(builder, member, &expression.ty)
            }
            AstExpressionData::TupleAccess { accessible, idx } => {
                let object = self.translate_expression_lval(builder, &accessible)?;

                if let AstType::Tuple { types } = &accessible.ty {
                    let begin = types[0..*idx].iter().map(|t| self.num_subvals(t)).sum();
                    let end = begin + self.num_subvals(&types[*idx]);

                    Ok(object[begin..end].iter().cloned().collect())
                } else {
                    unreachable!()
                }
            }
            AstExpressionData::GlobalVariable { name } => Ok(self.globals[name]
                .iter()
                .map(|e| e.as_pointer_value())
                .collect()),
            _ => unreachable!(),
        }
    }

    fn translate_closure_capture_environment(
        &mut self,
        c: &AstExpressionData,
    ) -> PResult<TrClosureCaptureEnvironment> {
        if let AstExpressionData::Closure { captured, .. } = c {
            let id = new_type_id();
            let mut captured_ids = Vec::new();
            let mut indices = HashMap::new();
            let mut ast_tys = HashMap::new();
            let mut tys = HashMap::new();

            for (idx, (_, c)) in captured.as_ref().unwrap().iter().enumerate() {
                captured_ids.push(c.id);
                indices.insert(c.id, idx + 1); // First argument is saved for the fn itself.
                ast_tys.insert(c.id, c.ty.clone());
                tys.insert(c.id, self.get_llvm_ty(&c.ty)?);
            }

            let env = TrClosureCaptureEnvironment {
                id,
                captured: captured_ids,
                indices,
                ast_tys,
                tys,
            };

            self.closure_object_ids.insert(id, env.clone());
            Ok(env)
        } else {
            unreachable!()
        }
    }

    fn translate_closure_body(
        &mut self,
        env: &TrClosureCaptureEnvironment,
        c: &AstExpressionData,
    ) -> PResult<FunctionValue> {
        if let AstExpressionData::Closure {
            params,
            expr,
            variables,
            ..
        } = c
        {
            let variables = variables.as_ref().unwrap();

            let ret_ty = self.get_llvm_ty(&expr.ty)?;
            let mut param_tys: Vec<_> = params
                .iter()
                .map(|p| self.get_llvm_ty(&p.ty))
                .collect::<PResult<_>>()?;
            param_tys.insert(0, opaque_env_type(&self.context).into());

            let llvm_fun =
                self.module
                    .add_function(&temp_name(), fun_type(ret_ty, &param_tys), None);
            set_cheshire_fn_attributes(&self.context, llvm_fun);

            let param_values: HashMap<_, _> =
                ZipExact::zip_exact(params, &llvm_fun.get_params()[1..], "params")?
                    .map(|(p, v)| (p.id, v.clone()))
                    .collect();

            let block = llvm_fun.append_basic_block("pre");
            let first_builder = Builder::create();
            first_builder.position_at_end(&block);

            // Cast that 0th parameter to the right, actual capture struct type.
            let opaque_env_ptr = llvm_fun.get_params()[0].into_pointer_value();
            let env_ptr = first_builder.build_pointer_cast(
                opaque_env_ptr,
                env.into_struct_type(&self.context),
                &temp_name(),
            );
            let env_struct = first_builder
                .build_load(env_ptr, &temp_name())
                .into_struct_value();
            let capture_values = unpack_capture_values(env, &first_builder, env_struct)?;

            for (id, var) in variables {
                let ty = self.get_llvm_ty(&var.ty)?;
                let val = if let Some(..) = param_values.get(id) {
                    param_values[id]
                } else if let Some(..) = capture_values.get(id) {
                    capture_values[id]
                } else {
                    type_zero(ty)?
                };

                let ptrs = self.flatten_alloca(&first_builder, val, &var.ty)?;
                self.variables.insert(*id, ptrs);
            }

            let start_block = llvm_fun.append_basic_block("start");
            first_builder.build_unconditional_branch(&start_block);

            let start_builder = Builder::create();
            start_builder.position_at_end(&start_block);

            let ret = self.translate_expression(&start_builder, &expr)?;
            let ret = self.bundle_vals(&start_builder, &ret, &expr.ty)?;

            start_builder.build_return(Some(&ret));

            Ok(llvm_fun)
        } else {
            unreachable!()
        }
    }

    fn translate_closure_construction(
        &self,
        builder: &Builder,
        fun: FunctionValue,
        env: &TrClosureCaptureEnvironment,
        c: &AstExpressionData,
    ) -> PResult<PointerValue> {
        if let AstExpressionData::Closure {
            captured: Some(captured),
            ..
        } = c
        {
            let env_ty = env.into_struct_type(&self.context);
            let size = env_ty
                .get_element_type()
                .into_struct_type()
                .size_of()
                .unwrap();
            let ty_id = env.id;

            let ptr = unwrap_callsite(
                builder.build_call(
                    self.get_function("gc_alloc_object"),
                    &[
                        size.into(),
                        self.context
                            .i16_type()
                            .const_int(ty_id as u64, false)
                            .into(),
                    ],
                    &temp_name(),
                ),
            )
            .into_pointer_value();

            let env_ptr = builder.build_pointer_cast(ptr, env_ty, &temp_name());

            // Store the fn (casted to the opaque fn type) in the 0th member.
            let fn_dest = unsafe { builder.build_struct_gep(env_ptr, 0, &temp_name()) };
            let opaque_fn = builder.build_pointer_cast(
                fun.as_global_value().as_pointer_value(),
                opaque_fn_type(&self.context),
                &temp_name(),
            );
            builder.build_store(fn_dest, opaque_fn);

            // Then, for each capture, bundle, then store where it needs to go.
            for (c, new) in captured {
                let loaded: Vec<_> = self.variables[&c.id]
                    .iter()
                    .map(|p| builder.build_load(*p, &temp_name()))
                    .collect();
                let bundle = self.bundle_vals(builder, &loaded, &c.ty)?;

                let idx = env.indices[&new.id];
                let dest = unsafe { builder.build_struct_gep(env_ptr, idx as u32, &temp_name()) };
                builder.build_store(dest, bundle);
            }

            Ok(env_ptr)
        } else {
            unreachable!()
        }
    }

    fn translate_gc_visit(
        &mut self,
        tys: &HashSet<AstType>,
        instantiated_objects: &HashMap<InstObjectSignature, AstObject>,
    ) -> PResult<()> {
        let callback_value_type = self.context.i8_type().ptr_type(GC).ptr_type(GLOBAL);
        let gc_callback_type = self
            .context
            .bool_type()
            .fn_type(&[callback_value_type.into()], false)
            .ptr_type(GLOBAL);

        let gc_visit = self.add_void_function(
            "gc_visit",
            &[
                self.context.i8_type().ptr_type(GLOBAL).into(),
                self.context.i16_type().into(),
                gc_callback_type.into(),
            ],
            false,
        );

        let gc_visit_array = self.add_void_function(
            "gc_visit_array",
            &[
                callback_value_type.into(),
                self.context.i16_type().into(), // Child type
                gc_callback_type.into(),
            ],
            false,
        );

        let gc_visit_closure = self.add_void_function(
            "gc_visit_closure",
            &[callback_value_type.into(), gc_callback_type.into()],
            false,
        );

        let ptr_param = gc_visit.get_params()[0].into_pointer_value();
        let id_param = gc_visit.get_params()[1].into_int_value();
        let callback_param = gc_visit.get_params()[2].into_pointer_value();

        let block = gc_visit.append_basic_block("first");
        let builder = Builder::create();
        builder.position_at_end(&block);

        let (end_block, end_builder) = self.get_new_block(&builder)?;

        let mut switch = Vec::new();
        for t in tys {
            let id = self.type_ids[&t];
            debug!("GC_VISIT: {} => {}", t, id);
            let (block, builder) = self.get_new_block(&builder)?;

            match t {
                AstType::Int | AstType::Char | AstType::Bool | AstType::FnPointerType { .. } => {
                    // Do nothing.
                }

                AstType::String => {
                    // Change the i8* into an i8 addrspace(1)**
                    let callback_value =
                        builder.build_pointer_cast(ptr_param, callback_value_type, &temp_name());
                    builder.build_call(callback_param, &[callback_value.into()], &temp_name());
                }

                AstType::Object(name, generics) => {
                    // Change the i8* into an i8 addrspace(1)**
                    let callback_value =
                        builder.build_pointer_cast(ptr_param, callback_value_type, &temp_name());
                    let marked = unwrap_callsite(builder.build_call(
                        callback_param,
                        &[callback_value.into()],
                        &temp_name(),
                    ));
                    let (next_block, _) = self.get_new_block(&builder)?;
                    builder.build_conditional_branch(
                        marked.into_int_value(),
                        &next_block,
                        &end_block,
                    );
                    builder.position_at_end(&next_block);

                    // _MUST_ read the object after calling the callback. We might remap it in the callback!
                    // Change the i8* into an object addrspace(1)**
                    let object_ptr = builder.build_pointer_cast(
                        ptr_param,
                        ptr_type(self.get_llvm_ty(t)?, GLOBAL).into_pointer_type(),
                        &temp_name(),
                    );
                    let object = builder.build_load(object_ptr, &temp_name());

                    for (idx, member) in instantiated_objects
                        [&InstObjectSignature(name.clone(), generics.clone())]
                        .members
                        .iter()
                        .enumerate()
                    {
                        let mem_type_id = self.type_ids[&member.member_type];

                        let mem_ptr = unsafe {
                            builder.build_struct_gep(
                                object.into_pointer_value(),
                                idx as u32,
                                &temp_name(),
                            )
                        };
                        let mem_ptr = builder.build_pointer_cast(
                            mem_ptr,
                            self.context.i8_type().ptr_type(GLOBAL),
                            &temp_name(),
                        );
                        builder.build_call(
                            gc_visit,
                            &[
                                mem_ptr.into(),
                                self.context
                                    .i16_type()
                                    .const_int(mem_type_id as u64, false)
                                    .into(),
                                callback_param.into(),
                            ],
                            &temp_name(),
                        );
                    }
                }

                AstType::Array { ty } => {
                    // Change the i8* into an array addrspace(1)**
                    let array_ptr =
                        builder.build_pointer_cast(ptr_param, callback_value_type, &temp_name());

                    let elem_type_id = self.type_ids[&ty];
                    builder.build_call(
                        gc_visit_array,
                        &[
                            array_ptr.into(),
                            self.context
                                .i16_type()
                                .const_int(elem_type_id as u64, false)
                                .into(),
                            callback_param.into(),
                        ],
                        &temp_name(),
                    );
                }

                AstType::Tuple { types } => {
                    let tuple_ptr = builder.build_pointer_cast(
                        ptr_param,
                        ptr_type(self.get_llvm_ty(t)?, GLOBAL).into_pointer_type(),
                        &temp_name(),
                    );

                    for (idx, subty) in types.iter().enumerate() {
                        let subty_id = self.type_ids[&subty];
                        let subty_ptr = unsafe {
                            builder.build_struct_gep(tuple_ptr, idx as u32, &temp_name())
                        };
                        let subty_ptr = builder.build_pointer_cast(
                            subty_ptr,
                            self.context.i8_type().ptr_type(GLOBAL),
                            &temp_name(),
                        );
                        builder.build_call(
                            gc_visit,
                            &[
                                subty_ptr.into(),
                                self.context
                                    .i16_type()
                                    .const_int(subty_id as u64, false)
                                    .into(),
                                callback_param.into(),
                            ],
                            &temp_name(),
                        );
                    }
                }

                AstType::Enum(name, generics) => {
                    let enum_ptr = builder.build_pointer_cast(
                        ptr_param,
                        ptr_type(self.get_llvm_ty(t)?, GLOBAL).into_pointer_type(),
                        &temp_name(),
                    );

                    let en = InstEnumSignature(name.clone(), generics.clone());

                    for (i, subty) in self.enums[&en].fields.iter().enumerate() {
                        let subty_id = self.type_ids[&subty];
                        let subty_ptr =
                            unsafe { builder.build_struct_gep(enum_ptr, i as u32, &temp_name()) };
                        let subty_ptr = builder.build_pointer_cast(
                            subty_ptr,
                            self.context.i8_type().ptr_type(GLOBAL),
                            &temp_name(),
                        );
                        builder.build_call(
                            gc_visit,
                            &[
                                subty_ptr.into(),
                                self.context
                                    .i16_type()
                                    .const_int(subty_id as u64, false)
                                    .into(),
                                callback_param.into(),
                            ],
                            &temp_name(),
                        );
                    }
                }

                AstType::ClosureType { .. } => {
                    // Change the i8* into an closure addrspace(1)**
                    let closure_ptr =
                        builder.build_pointer_cast(ptr_param, callback_value_type, &temp_name());

                    builder.build_call(
                        gc_visit_closure,
                        &[closure_ptr.into(), callback_param.into()],
                        &temp_name(),
                    );
                }

                _ => unreachable!(),
            }

            builder.build_unconditional_branch(&end_block);
            switch.push((id, block));
        }

        for (id, env) in &self.closure_object_ids {
            let (block, builder) = self.get_new_block(&builder)?;

            // Change the i8* into an i8 addrspace(1)**
            let callback_value =
                builder.build_pointer_cast(ptr_param, callback_value_type, &temp_name());
            let marked = unwrap_callsite(builder.build_call(
                callback_param,
                &[callback_value.into()],
                &temp_name(),
            ));
            let (next_block, _) = self.get_new_block(&builder)?;
            builder.build_conditional_branch(marked.into_int_value(), &next_block, &end_block);
            builder.position_at_end(&next_block);

            // _MUST_ read the object after calling the callback. We might remap it in the callback!
            // Change the i8* into an object addrspace(1)**
            let object_ptr = builder.build_pointer_cast(
                ptr_param,
                ptr_type(env.into_struct_type(&self.context).into(), GLOBAL).into_pointer_type(),
                &temp_name(),
            );
            let object = builder.build_load(object_ptr, &temp_name());

            for (member, idx) in &env.indices {
                let mem_type_id = self.type_ids[&env.ast_tys[member]];

                let mem_ptr = unsafe {
                    builder.build_struct_gep(object.into_pointer_value(), *idx as u32, &temp_name())
                };
                let mem_ptr = builder.build_pointer_cast(
                    mem_ptr,
                    self.context.i8_type().ptr_type(GLOBAL),
                    &temp_name(),
                );
                builder.build_call(
                    gc_visit,
                    &[
                        mem_ptr.into(),
                        self.context
                            .i16_type()
                            .const_int(mem_type_id as u64, false)
                            .into(),
                        callback_param.into(),
                    ],
                    &temp_name(),
                );
            }

            builder.build_unconditional_branch(&end_block);
            switch.push((*id, block));
        }

        let switch: Vec<_> = switch
            .iter()
            .map(|(id, block)| (self.context.i16_type().const_int(*id as u64, false), block))
            .collect();
        builder.build_switch(id_param, &end_block, &switch);
        end_builder.build_return(None);

        Ok(())
    }

    fn get_new_block(&self, builder: &Builder) -> PResult<(BasicBlock, Builder)> {
        let b = builder.get_insert_block().unwrap();
        let p = b.get_parent().unwrap();

        let block = p.append_basic_block(&temp_name());
        let builder = Builder::create();
        builder.position_at_end(&block);
        Ok((block, builder))
    }

    fn get_llvm_ty(&self, t: &AstType) -> PResult<BasicTypeEnum> {
        let ty = match t {
            AstType::Int => self.context.i64_type().into(),
            AstType::Char => self.context.i8_type().into(),
            AstType::Bool => self.context.bool_type().into(),

            AstType::String => ptr_type(self.module.get_type("string").unwrap(), GC).into(),

            AstType::Object(name, generics) => {
                let name = decorate_object(name, generics)?;
                let ty = self.module.get_type(&name).unwrap();
                ptr_type(ty, GC)
            }

            AstType::Array { ty } => {
                let ty = self.get_llvm_ty(ty)?;
                let ctx = self
                    .context
                    .struct_type(
                        &[
                            self.context.i64_type().into(),
                            self.context.i64_type().into(),
                            array_type(ty),
                        ],
                        false,
                    )
                    .into();
                ptr_type(ctx, GC)
            }

            AstType::Tuple { types } => {
                let types = types
                    .iter()
                    .map(|t| self.get_llvm_ty(t))
                    .collect::<PResult<Vec<BasicTypeEnum>>>()?;
                self.context.struct_type(&types, false).into()
            }

            AstType::Enum(name, generics) => {
                let types = self.enums[&InstEnumSignature(name.clone(), generics.clone())]
                    .fields
                    .iter()
                    .map(|t| self.get_llvm_ty(t))
                    .collect::<PResult<Vec<BasicTypeEnum>>>()?;
                self.context.struct_type(&types, false).into()
            }

            AstType::FnPointerType { args, ret_ty } => {
                let types = args
                    .iter()
                    .map(|t| self.get_llvm_ty(t))
                    .collect::<PResult<Vec<BasicTypeEnum>>>()?;

                fun_type(self.get_llvm_ty(ret_ty)?, &types)
                    .ptr_type(GLOBAL)
                    .into()
            }

            AstType::ClosureType { .. } => self
                .context
                .struct_type(&[opaque_fn_type(&self.context).into()], false)
                .ptr_type(GC)
                .into(),

            _ => unreachable!(),
        };

        Ok(ty)
    }

    fn bundle_vals(
        &self,
        builder: &Builder,
        vals: &[BasicValueEnum],
        t: &AstType,
    ) -> PResult<BasicValueEnum> {
        if let AstType::Tuple { types } = t {
            let mut consumed = 0;
            let mut bundled_value: AggregateValueEnum =
                self.get_llvm_ty(t)?.into_struct_type().get_undef().into();

            for (i, subty) in types.iter().enumerate() {
                let num_subvals = self.num_subvals(subty);
                let subvals = &vals[consumed..consumed + num_subvals];

                // Insert the bundled set of values...
                let bundled_subval = self.bundle_vals(builder, subvals, subty)?;
                bundled_value = builder
                    .build_insert_value(bundled_value, bundled_subval, i as u32, &temp_name())
                    .unwrap();

                consumed += num_subvals;
            }

            Ok(bundled_value.into_struct_value().into())
        } else if let AstType::Enum(..) = t {
            let mut bundled_value: AggregateValueEnum =
                self.get_llvm_ty(t)?.into_struct_type().get_undef().into();

            for (i, val) in vals.iter().enumerate() {
                bundled_value = builder
                    .build_insert_value(bundled_value, *val, i as u32, &temp_name())
                    .unwrap();
            }

            Ok(bundled_value.into_struct_value().into())
        } else {
            assert_eq!(vals.len(), 1);

            Ok(vals[0].clone())
        }
    }

    fn flatten_val(
        &self,
        builder: &Builder,
        v: BasicValueEnum,
        t: &AstType,
    ) -> PResult<Vec<BasicValueEnum>> {
        if let AstType::Tuple { types } = t {
            let mut ret = Vec::new();

            for (i, subty) in types.into_iter().enumerate() {
                let subval = builder
                    .build_extract_value(v.into_struct_value(), i as u32, &temp_name())
                    .unwrap();
                ret.extend(self.flatten_val(builder, subval, subty)?);
            }

            Ok(ret)
        } else if let AstType::Enum(name, generics) = t {
            let en = InstEnumSignature(name.clone(), generics.clone());
            let mut ret = Vec::new();

            for (i, subty) in self.enums[&en].fields.iter().enumerate() {
                let subval = builder
                    .build_extract_value(v.into_struct_value(), i as u32, &temp_name())
                    .unwrap();
                let subvals = self.flatten_val(builder, subval, subty)?;
                assert_eq!(subvals.len(), 1);
                ret.push(subvals[0]);
            }

            Ok(ret)
        } else {
            Ok(vec![v])
        }
    }

    fn flatten_ptr(
        &self,
        builder: &Builder,
        v: PointerValue,
        t: &AstType,
    ) -> PResult<Vec<PointerValue>> {
        if let AstType::Tuple { types } = t {
            let mut ret = Vec::new();

            for (i, subty) in types.into_iter().enumerate() {
                let subval = unsafe { builder.build_struct_gep(v, i as u32, &temp_name()) };
                ret.extend(self.flatten_ptr(builder, subval, subty)?);
            }

            Ok(ret)
        } else if let AstType::Enum(name, generics) = t {
            let en = InstEnumSignature(name.clone(), generics.clone());
            let mut ret = Vec::new();

            for (i, subty) in self.enums[&en].fields.iter().enumerate() {
                let subval = unsafe { builder.build_struct_gep(v, i as u32, &temp_name()) };
                let subvals = self.flatten_ptr(builder, subval, subty)?;
                assert_eq!(subvals.len(), 1);
                ret.push(subvals[0]);
            }

            Ok(ret)
        } else {
            Ok(vec![v])
        }
    }

    fn flatten_alloca(
        &self,
        builder: &Builder,
        v: BasicValueEnum,
        t: &AstType,
    ) -> PResult<Vec<PointerValue>> {
        if let AstType::Tuple { types } = t {
            let mut ret = Vec::new();

            for (i, subty) in types.into_iter().enumerate() {
                let subval = builder
                    .build_extract_value(v.into_struct_value(), i as u32, &temp_name())
                    .unwrap();
                ret.extend(self.flatten_alloca(builder, subval, subty)?);
            }

            Ok(ret)
        } else if let AstType::Enum(name, generics) = t {
            let en = InstEnumSignature(name.clone(), generics.clone());
            let mut ret = Vec::new();

            for (i, subty) in self.enums[&en].fields.iter().enumerate() {
                let subval = builder
                    .build_extract_value(v.into_struct_value(), i as u32, &temp_name())
                    .unwrap();
                let subvals = self.flatten_alloca(builder, subval, subty)?;
                assert_eq!(subvals.len(), 1);
                ret.push(subvals[0]);
            }

            Ok(ret)
        } else {
            let ptr = builder.build_alloca(self.get_llvm_ty(t)?, &temp_name());
            builder.build_store(ptr, v);

            Ok(vec![ptr])
        }
    }

    fn flatten_globals(&self, t: &AstType) -> PResult<Vec<GlobalValue>> {
        if let AstType::Tuple { types } = t {
            let mut ret = Vec::new();

            for subty in types.into_iter() {
                ret.extend(self.flatten_globals(subty)?);
            }

            Ok(ret)
        } else if let AstType::Enum(name, generics) = t {
            let en = InstEnumSignature(name.clone(), generics.clone());
            let mut ret = Vec::new();

            for subty in &self.enums[&en].fields {
                let subtys = self.flatten_globals(subty)?;
                assert_eq!(subtys.len(), 1);
                ret.push(subtys[0]);
            }

            Ok(ret)
        } else {
            let ty = self.get_llvm_ty(t)?;
            let ptr = self.module.add_global(ty, None, &temp_name());
            ptr.set_initializer(&type_undefined(ty)?);
            Ok(vec![ptr])
        }
    }

    fn flatten_ty(&self, t: &AstType) -> Vec<AstType> {
        match t {
            AstType::Tuple { types } => types.iter().flat_map(|t| self.flatten_ty(t)).collect(),
            AstType::Enum(name, generics) => {
                let sig = InstEnumSignature(name.clone(), generics.clone());
                self.enums[&sig].fields.clone()
            }
            t => vec![t.clone()],
        }
    }

    pub fn num_subvals(&self, t: &AstType) -> usize {
        if let AstType::Tuple { types } = t {
            types.iter().map(|t| self.num_subvals(t)).sum()
        } else if let AstType::Enum(name, generics) = t {
            let en = InstEnumSignature(name.clone(), generics.clone());
            self.enums[&en].fields.len()
        } else {
            1usize
        }
    }
}

fn set_all_fn_attributes(context: &Context, fun: FunctionValue) {
    fun.add_attribute(
        AttributeLoc::Function,
        context.create_enum_attribute(Attribute::get_named_enum_kind_id("nounwind"), 1),
    );
}

fn set_cheshire_fn_attributes(context: &Context, fun: FunctionValue) {
    fun.set_gc("statepoint-example");
    fun.add_attribute(
        AttributeLoc::Function,
        context.create_enum_attribute(Attribute::get_named_enum_kind_id("nounwind"), 1),
    );
    fun.add_attribute(
        AttributeLoc::Function,
        context.create_enum_attribute(Attribute::get_named_enum_kind_id("inlinehint"), 1),
    );
}

pub fn unpack_capture_values(
    env: &TrClosureCaptureEnvironment,
    builder: &Builder,
    env_struct: StructValue,
) -> PResult<HashMap<VariableId, BasicValueEnum>> {
    let mut values = HashMap::new();

    for (id, idx) in &env.indices {
        let v = builder
            .build_extract_value(env_struct, *idx as u32, &temp_name())
            .unwrap();
        values.insert(*id, v);
    }

    Ok(values)
}

lazy_static! {
    static ref TEMP_NAME_COUNTER: RwLock<usize> = RwLock::new(1);
    static ref TYPE_ID_COUNTER: RwLock<usize> = RwLock::new(0);
}

fn temp_name() -> String {
    let mut id_ref = TEMP_NAME_COUNTER.write().unwrap();
    *id_ref += 1;
    let id: usize = *id_ref;

    format!("temp_{}", id)
}

fn new_type_id() -> usize {
    let mut id_ref = TYPE_ID_COUNTER.write().unwrap();
    *id_ref += 1;
    *id_ref
}
