use self::decorate::*;
use self::type_helpers::*;
use crate::inst::{InstFunctionSignature, InstObjectSignature, InstantiatedProgram};
use crate::parser::ast::*;
use crate::util::{IntoError, PError, PResult, ZipExact};
use inkwell::attributes::{Attribute, AttributeLoc};
use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::*;
use inkwell::values::*;
use inkwell::AddressSpace;
use std::collections::{HashMap, HashSet};
use std::ffi::{OsStr, OsString};
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::sync::RwLock;
use tempfile::TempDir;

mod decorate;
mod type_helpers;

const GLOBAL: AddressSpace = AddressSpace::Generic; /* Wot. */
const GC: AddressSpace = AddressSpace::Global; /* addrspace(1) is Managed, i swear */

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

enum Definition {
    Block(AstBlock),
    None,
    ArrayAccess,
    ArrayAssign,
    ArrayLen,
}

struct Translator {
    context: Context,
    module: Module,

    alloc_string: FunctionValue,
    alloc_object: FunctionValue,
    alloc_array: FunctionValue,
    array_idx_at: FunctionValue,

    break_continue: Option<(BasicBlock, BasicBlock)>,

    variables: HashMap<VariableId, Vec<PointerValue>>,
    type_ids: HashMap<AstType, usize>,
}

pub fn translate(
    file: InstantiatedProgram,
    llvm_ir: bool,
    output_file: &str,
    included_files: Vec<OsString>,
) -> PResult<()> {
    let mut tr = Translator::new();
    tr.translate(file)?;

    let module = tr.module;

    if let Result::Err(why) = module.verify() {
        println!("{}", module.print_to_string().to_string());

        PError::new(format!("LLVM: {}", why.to_string()));
    }

    if output_file != "-" {
        emit_module(module, llvm_ir, output_file, included_files)?;
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
) -> PResult<()> {
    if llvm_ir {
        if let Err(msg) = module.print_to_file(&PathBuf::from(output_file)) {
            return PResult::error(format!(
                "There was a problem writing the assembly file to disk: {}",
                msg.to_string()
            ));
        }
    } else {
        let dir = TempDir::new().map_err(|e| PError::new(format!("Temp dir error: {}", e)))?;
        let ll = dir.path().join("file.ll").into_os_string();
        let ll_gc = dir.path().join("file.gc.ll").into_os_string();
        let ll_o = dir.path().join("file.o").into_os_string();
        module.write_bitcode_to_path(Path::new(&ll));

        let mut opt_command = Command::new("opt-5.0".to_string());
        opt_command.args(&[
            &ll,
            //OsStr::new("-O3"),
            OsStr::new("--mem2reg"),
            OsStr::new("--rewrite-statepoints-for-gc"),
            OsStr::new("-o"),
            &ll_gc,
        ]);

        let mut llc_command = Command::new("llc-5.0".to_string());
        llc_command.args(&[
            &ll_gc,
            OsStr::new("-o"),
            &ll_o,
            OsStr::new("-relocation-model=pic"),
            OsStr::new("-O3"),
            OsStr::new("-filetype=obj"),
        ]);

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
                OsStr::new("-o"),
                OsStr::new(output_file),
                OsStr::new("-O3"),
                OsStr::new("-g"),
                OsStr::new("-fPIC"),
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
    println!("Executing command: {:?}", command);
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
        context.opaque_struct_type("string").set_body(
            &[
                context.i64_type().into(),
                context.i8_type().ptr_type(GLOBAL).into(),
            ],
            false,
        );

        let alloc_string = module.add_function(
            "gc_alloc_string",
            module
                .get_type("string")
                .unwrap()
                .into_struct_type()
                .ptr_type(GC)
                .fn_type(
                    &[
                        context.i8_type().ptr_type(GLOBAL).into(),
                        context.i64_type().into(),
                    ],
                    false,
                ),
            None,
        );
        set_fn_attrs(&context, alloc_string);

        let alloc_object = module.add_function(
            "gc_alloc_object",
            context.i8_type().ptr_type(GC).fn_type(
                &[context.i64_type().into(), context.i16_type().into()],
                false,
            ),
            None,
        );
        set_fn_attrs(&context, alloc_object);

        let alloc_array = module.add_function(
            "gc_alloc_array",
            context.i8_type().ptr_type(GC).fn_type(
                &[
                    context.i64_type().into(),
                    context.i64_type().into(),
                    context.i16_type().into(),
                ],
                false,
            ),
            None,
        );
        set_fn_attrs(&context, alloc_array);

        let array_idx_at = module.add_function(
            "gc_array_idx_at",
            context.i8_type().ptr_type(GLOBAL).fn_type(
                &[
                    context.i8_type().ptr_type(GC).into(),
                    context.i64_type().into(),
                ],
                false,
            ),
            None,
        );
        set_fn_attrs(&context, alloc_array);

        Translator {
            module,
            context,

            alloc_string,
            alloc_object,
            alloc_array,
            array_idx_at,

            break_continue: None,

            variables: HashMap::new(),
            type_ids: HashMap::new(),
        }
    }

    fn translate(&mut self, mut file: InstantiatedProgram) -> PResult<()> {
        // Forward declare the type ids.
        file.instantiated_types.remove(&AstType::String);
        self.type_ids.insert(AstType::String, 0);
        for (i, ty) in file.instantiated_types.iter().enumerate() {
            self.type_ids.insert(ty.clone(), i + 1); // 0 is left for for string...
        }

        // Forward declaration.
        for (sig, _) in &file.instantiated_objects {
            let name = decorate_object(&sig.0, &sig.1)?;
            self.context.opaque_struct_type(&name);
        }

        for (sig, obj) in &file.instantiated_objects {
            self.translate_object(sig, obj.as_ref().unwrap())?;
        }

        for (sig, fun) in &file.instantiated_fns {
            let fun = fun.as_ref().unwrap();
            let name = decorate_fn(&sig.0, &sig.1)?;
            self.forward_declare_function(
                &name,
                &fun.parameter_list,
                &fun.return_type,
                fun.definition.is_none(),
            )?;
        }

        for (sig, fun) in &file.instantiated_object_fns {
            let fun = fun.as_ref().unwrap();
            let name = decorate_object_fn(&sig.0, &sig.1, &sig.2, &sig.3)?;
            self.forward_declare_function(
                &name,
                &fun.parameter_list,
                &fun.return_type,
                fun.definition.is_none(),
            )?;
        }

        for (sig, fun) in file.instantiated_fns {
            let fun = fun.unwrap();
            let name = decorate_fn(&sig.0, &sig.1)?;

            let definition = if is_array_deref(&sig)? {
                Definition::ArrayAccess
            } else if is_array_len(&sig)? {
                Definition::ArrayLen
            } else if is_array_deref_assign(&sig)? {
                Definition::ArrayAssign
            } else if fun.definition.is_some() {
                Definition::Block(fun.definition.unwrap())
            } else {
                Definition::None
            };

            self.translate_function(
                &name,
                &fun.parameter_list,
                &fun.return_type,
                &fun.variables,
                definition,
            )?;
        }

        for (sig, fun) in file.instantiated_object_fns {
            let fun = fun.unwrap();
            let name = decorate_object_fn(&sig.0, &sig.1, &sig.2, &sig.3)?;

            let definition = if fun.definition.is_some() {
                Definition::Block(fun.definition.unwrap())
            } else {
                Definition::None
            };

            self.translate_function(
                &name,
                &fun.parameter_list,
                &fun.return_type,
                &fun.variables,
                definition,
            )?;
        }

        self.translate_gc_visit(&file.instantiated_types, &file.instantiated_objects)?;
        self.translate_main(&file.main_fn)?;

        Ok(())
    }

    fn translate_main(&mut self, main_fn: &ModuleRef) -> PResult<()> {
        let main_fn_name = decorate_fn(main_fn, &[])?;
        let cheshire_main_fn = self.module.get_function(&main_fn_name).unwrap();

        let real_main_fn = self.forward_declare_function("main", &[], &AstType::Int, false)?;
        let block = real_main_fn.append_basic_block("pre");
        let first_builder = Builder::create();
        first_builder.position_at_end(&block);
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
            .map(|p| self.get_type(&p.ty))
            .collect::<PResult<Vec<_>>>()?;
        let ret = self.get_type(return_type)?;
        let fun_ty = fun_type(ret, &param_tys);

        let llvm_fun = self.module.add_function(&name, fun_ty, None);

        llvm_fun.set_gc("statepoint-example");
        set_fn_attrs(&self.context, llvm_fun);

        Ok(llvm_fun)
    }

    fn translate_object(&mut self, sig: &InstObjectSignature, obj: &AstObject) -> PResult<()> {
        let name = decorate_object(&sig.0, &sig.1)?;
        let ty = self.module.get_type(&name).unwrap().into_struct_type();

        let tys = obj
            .members
            .iter()
            .map(|m| self.get_type(&m.member_type))
            .collect::<PResult<Vec<BasicTypeEnum>>>()?;
        ty.set_body(&tys, false);

        Ok(())
    }

    fn translate_function(
        &mut self,
        fun_name: &str,
        parameter_list: &[AstNamedVariable],
        _return_type: &AstType,
        variables: &HashMap<VariableId, AstNamedVariable>,
        definition: Definition,
    ) -> PResult<()> {
        let llvm_fun = self.module.get_function(&fun_name).unwrap();

        if let Definition::Block(definition) = definition {
            let param_values: HashMap<_, _> =
                ZipExact::zip_exact(parameter_list, &llvm_fun.get_params(), "params")?
                    .map(|(p, v)| (p.id, v.clone()))
                    .collect();

            let block = llvm_fun.append_basic_block("pre");
            let first_builder = Builder::create();
            first_builder.position_at_end(&block);

            for (id, var) in variables {
                let ty = self.get_type(&var.ty)?;
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
            let ret = self.bundle_vals(&start_builder, &ret, &definition.expression.ty)?;

            start_builder.build_return(Some(&ret));
        } else if let Definition::ArrayAccess = definition {
            // TODO: Don't decorate me? I think??

            // Emit array access logic for `FpP8internalP5deref11deref_array1...`
            let return_type = llvm_fun.get_type().get_return_type().unwrap();

            let block = llvm_fun.append_basic_block("pre");
            let first_builder = Builder::create();
            first_builder.position_at_end(&block);

            // Alloca and store the array parameter.
            let array = llvm_fun.get_params()[0];
            let alloca = first_builder.build_alloca(array.get_type(), &temp_name());
            first_builder.build_store(alloca, array);

            // Take parameter zero, which is an array, and cast it to i8*
            let ptr = first_builder.build_pointer_cast(
                array.into_pointer_value(),
                self.context.i8_type().ptr_type(GC),
                &temp_name(),
            );

            // Call array_idx_at with array, elem size, and idx.
            let idx = llvm_fun.get_params()[1];
            let elem_ptr = unwrap_callsite(first_builder.build_call(
                self.array_idx_at,
                &[ptr.into(), idx],
                &temp_name(),
            ));

            // Cast that pointer to T*
            let elem_ptr_typed = first_builder.build_pointer_cast(
                elem_ptr.into_pointer_value(),
                ptr_type(return_type, GLOBAL).into_pointer_type(),
                &temp_name(),
            );
            let elem = first_builder.build_load(elem_ptr_typed, &temp_name());

            first_builder.build_return(Some(&elem));
        } else if let Definition::ArrayAssign = definition {
            // TODO: Don't decorate me? I think??

            // Emit array access logic for `FpP8internalP5deref11deref_array1...`
            let return_type = llvm_fun.get_type().get_return_type().unwrap();

            let block = llvm_fun.append_basic_block("pre");
            let first_builder = Builder::create();
            first_builder.position_at_end(&block);

            // Alloca and store the array parameter.
            let array = llvm_fun.get_params()[0];
            let alloca = first_builder.build_alloca(array.get_type(), &temp_name());
            first_builder.build_store(alloca, array);

            // Take parameter zero, which is an array, and cast it to i8*
            let ptr = first_builder.build_pointer_cast(
                array.into_pointer_value(),
                self.context.i8_type().ptr_type(GC),
                &temp_name(),
            );

            // Call array_idx_at with array, elem size, and idx.
            let idx = llvm_fun.get_params()[1];
            let elem_ptr = unwrap_callsite(first_builder.build_call(
                self.array_idx_at,
                &[ptr.into(), idx],
                &temp_name(),
            ));

            // Cast that pointer to T*
            let elem_ptr_typed = first_builder.build_pointer_cast(
                elem_ptr.into_pointer_value(),
                ptr_type(return_type, GLOBAL).into_pointer_type(),
                &temp_name(),
            );
            let value = llvm_fun.get_params()[2];
            first_builder.build_store(elem_ptr_typed, value);
            first_builder.build_return(Some(&value));
        } else if let Definition::ArrayLen = definition {
            let block = llvm_fun.append_basic_block("pre");
            let first_builder = Builder::create();
            first_builder.position_at_end(&block);

            // Alloca and store the array parameter.
            let array = llvm_fun.get_params()[0];
            let alloca = first_builder.build_alloca(array.get_type(), &temp_name());
            first_builder.build_store(alloca, array);

            let array_deref = first_builder.build_load(array.into_pointer_value(), &temp_name());
            let size = first_builder
                .build_extract_value(array_deref.into_struct_value(), 0, &temp_name())
                .unwrap();

            first_builder.build_return(Some(&size));
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
            AstStatement::Let { .. } => unreachable!(),
            AstStatement::Assert { .. } => unimplemented!(),

            AstStatement::Expression { expression } => {
                self.translate_expression(builder, expression)?;
            }

            AstStatement::While { condition, block } => {
                let (condition_block, condition_builder) = self.get_new_block(builder)?;
                let (loop_block, loop_builder) = self.get_new_block(builder)?;
                let (end_block, _) = self.get_new_block(builder)?;

                builder.build_unconditional_branch(&condition_block);
                builder.position_at_end(&end_block);

                let condition = self.translate_expression(&condition_builder, condition)?;
                assert_eq!(condition.len(), 1); // This is a boolean.
                let condition = condition[0];
                condition_builder.build_conditional_branch(
                    condition.into_int_value(),
                    &loop_block,
                    &end_block,
                );

                let old_break_continue =
                    self.set_break_continue(Some((end_block, condition_block)));
                let _ = self.translate_block(&loop_builder, block)?;
                loop_builder.build_unconditional_branch(&condition_block);

                self.set_break_continue(old_break_continue);
            }
            AstStatement::Break => {
                builder.build_unconditional_branch(self.get_break());

                let (fake_block, _) = self.get_new_block(builder)?;
                builder.position_at_end(&fake_block);
            }
            AstStatement::Continue => {
                builder.build_unconditional_branch(self.get_continue());

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
            AstExpressionData::SelfRef => unreachable!(),
            AstExpressionData::Unimplemented => unreachable!(),

            AstExpressionData::True => vec![self.context.bool_type().const_int(1, false).into()],
            AstExpressionData::False => vec![self.context.bool_type().const_int(1, false).into()],

            AstExpressionData::Null => vec![self
                .get_type(&expression.ty)?
                .into_pointer_type()
                .const_null()
                .into()],

            AstExpressionData::String { string, len } => {
                let string_const = self.context.const_string(&string, false);
                let string_global =
                    self.module
                        .add_global(string_const.get_type(), None, &temp_name());
                string_global.set_initializer(&string_const);

                // Access the [N x i8]* as a i8*
                let zero = self.context.i64_type().const_int(0, false);
                let string = unsafe {
                    builder.build_gep(
                        string_global.as_pointer_value(),
                        &[zero, zero],
                        &temp_name(),
                    )
                };

                let len = self.context.i64_type().const_int(*len as u64, false);
                let fn_ret = builder.build_call(
                    self.alloc_string,
                    &[string.into(), len.into()],
                    &temp_name(),
                );

                vec![unwrap_callsite(fn_ret)]
            }

            AstExpressionData::Int(i) => vec![self
                .context
                .i64_type()
                .const_int_from_string(&i, StringRadix::Decimal)
                .unwrap()
                .into()],
            AstExpressionData::Char(c) => {
                let mut k = [0u8];
                c.encode_utf8(&mut k);
                vec![self.context.i8_type().const_int(k[0] as u64, false).into()]
            }

            // Lval-always expressions. Can be calculated by getting the lval then deref'ing.
            AstExpressionData::Identifier { .. } | AstExpressionData::ObjectAccess { .. } => self
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
                let element_ty = self.get_type(&element_ast_ty)?;
                let array_ty = self.get_type(&expression.ty)?;
                let ty_size = type_size(element_ty)?;
                let num_elements = self
                    .context
                    .i64_type()
                    .const_int(elements.len() as u64, false);

                let ty_id = self.type_ids[&expression.ty];
                let ptr = builder.build_call(
                    self.alloc_array,
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

                for (idx, e) in elements.iter().enumerate() {
                    let idx = self.context.i64_type().const_int(idx as u64, false);
                    let loc = self.get_array_idx(builder, ptr, idx, element_ty)?;
                    let e = self.bundle_vals(builder, e, &element_ast_ty)?;
                    builder.build_store(loc, e);
                }

                vec![ptr.into()]
            }

            AstExpressionData::Call {
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
                    associated_trait.as_ref().unwrap(),
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
                    let begin = types[0..*idx].iter().map(num_subvals).sum();
                    let end = begin + num_subvals(&types[*idx]);

                    tup[begin..end].iter().cloned().collect()
                } else {
                    unreachable!()
                }
            }

            AstExpressionData::AllocateObject { object } => {
                let ptr_type = self.get_type(object)?.into_pointer_type();
                let size = type_size(ptr_type.get_element_type().into_struct_type().into())?;
                let ty_id = self.type_ids[&expression.ty];

                let val = builder.build_call(
                    self.alloc_object,
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

                vec![builder
                    .build_pointer_cast(ptr.into_pointer_value(), ptr_type, &temp_name())
                    .into()]
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
                then_builder.build_unconditional_branch(&after_block);

                let else_values = self.translate_block(&else_builder, &else_ast_block)?;
                else_builder.build_unconditional_branch(&after_block);

                builder.position_at_end(&after_block);

                // Collect all of the flattened values in a series of Φs.
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
        };

        Ok(value)
    }

    fn get_array_idx(
        &mut self,
        builder: &Builder,
        loc: PointerValue,
        idx: IntValue,
        element_ty: BasicTypeEnum,
    ) -> PResult<PointerValue> {
        let array = unsafe { builder.build_struct_gep(loc, 2, &temp_name()) };
        let array_ptr = builder.build_pointer_cast(
            array,
            ptr_type(element_ty, GLOBAL).into_pointer_type(),
            &temp_name(),
        );
        let elem_ptr = unsafe { builder.build_gep(array_ptr, &[idx], &temp_name()) };
        Ok(elem_ptr)
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
                let object = object[0];

                let object = builder.build_address_space_cast(
                    object.into_pointer_value(),
                    ptr_type(
                        object
                            .get_type()
                            .into_pointer_type()
                            .get_element_type()
                            .into_struct_type()
                            .into(),
                        GLOBAL,
                    )
                    .into_pointer_type(),
                    &temp_name(),
                );

                let member = unsafe {
                    builder.build_struct_gep(object, mem_idx.unwrap() as u32, &temp_name())
                };

                self.flatten_ptr(builder, member, &expression.ty)
            }
            AstExpressionData::TupleAccess { accessible, idx } => {
                let object = self.translate_expression_lval(builder, &accessible)?;

                if let AstType::Tuple { types } = &accessible.ty {
                    let begin = types[0..*idx].iter().map(num_subvals).sum();
                    let end = begin + num_subvals(&types[*idx]);

                    Ok(object[begin..end].iter().cloned().collect())
                } else {
                    unreachable!()
                }
            }
            _ => unreachable!(),
        }
    }

    fn translate_gc_visit(
        &self,
        tys: &HashSet<AstType>,
        instantiated_objects: &HashMap<InstObjectSignature, Option<AstObject>>,
    ) -> PResult<()> {
        let gc_visit = self.module.add_function(
            "gc_visit",
            self.context.void_type().fn_type(
                &[
                    self.context.i8_type().ptr_type(GLOBAL).into(),
                    self.context.i16_type().into(),
                ],
                false,
            ),
            None,
        );

        let gc_visit_array = self.module.add_function(
            "gc_visit_array",
            self.context.void_type().fn_type(
                &[
                    self.context.i8_type().ptr_type(GC).into(),
                    self.context.i16_type().into(), // Child type
                ],
                false,
            ),
            None,
        );

        let gc_mark = self.module.add_function(
            "gc_mark",
            self.context
                .bool_type()
                .fn_type(&[self.context.i8_type().ptr_type(GC).into()], false),
            None,
        );

        let ptr_param = gc_visit.get_params()[0].into_pointer_value();
        let id_param = gc_visit.get_params()[1].into_int_value();

        let block = gc_visit.append_basic_block("first");
        let builder = Builder::create();
        builder.position_at_end(&block);

        let (end_block, end_builder) = self.get_new_block(&builder)?;

        let mut switch = Vec::new();
        for t in tys {
            let id = self.type_ids[&t];
            let (block, builder) = self.get_new_block(&builder)?;

            match t {
                AstType::Int | AstType::Char | AstType::Bool => {
                    // Do nothing.
                }

                AstType::String => {
                    // Change the i8* into an string addrspace(1)**
                    let string_ptr = builder.build_pointer_cast(
                        ptr_param,
                        ptr_type(self.get_type(t)?, GLOBAL).into_pointer_type(),
                        &temp_name(),
                    );
                    let string = builder.build_load(string_ptr, &temp_name());
                    let mark_ptr = builder.build_pointer_cast(
                        string.into_pointer_value(),
                        self.context.i8_type().ptr_type(GC),
                        &temp_name(),
                    );
                    builder.build_call(gc_mark, &[mark_ptr.into()], &temp_name());
                }

                AstType::Object(name, generics) => {
                    // Change the i8* into an object addrspace(1)**
                    let object_ptr = builder.build_pointer_cast(
                        ptr_param,
                        ptr_type(self.get_type(t)?, GLOBAL).into_pointer_type(),
                        &temp_name(),
                    );
                    let object = builder.build_load(object_ptr, &temp_name());

                    // Stupid logic to make the object back into an i8 addrspace(1)* and mark it.
                    // Then conditionally branch to the end if it has been marked, so we don't
                    // loop forever.
                    let mark_ptr = builder.build_pointer_cast(
                        object.into_pointer_value(),
                        self.context.i8_type().ptr_type(GC),
                        &temp_name(),
                    );
                    let marked = builder.build_call(gc_mark, &[mark_ptr.into()], &temp_name());
                    let marked = unwrap_callsite(marked);
                    let (next_block, _) = self.get_new_block(&builder)?;
                    builder.build_conditional_branch(
                        marked.into_int_value(),
                        &next_block,
                        &end_block,
                    );
                    builder.position_at_end(&next_block);

                    for (idx, member) in instantiated_objects
                        [&InstObjectSignature(name.clone(), generics.clone())]
                        .as_ref()
                        .unwrap()
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
                            ],
                            &temp_name(),
                        );
                    }
                }

                AstType::Array { ty } => {
                    // Change the i8* into an array addrspace(1)**
                    let array_ptr = builder.build_pointer_cast(
                        ptr_param,
                        ptr_type(self.get_type(t)?, GLOBAL).into_pointer_type(),
                        &temp_name(),
                    );
                    // Now get the array addrspace(1)*
                    let array = builder.build_load(array_ptr, &temp_name());

                    let mark_ptr = builder.build_pointer_cast(
                        array.into_pointer_value(),
                        self.context.i8_type().ptr_type(GC),
                        &temp_name(),
                    );
                    let marked = builder.build_call(gc_mark, &[mark_ptr.into()], &temp_name());
                    let marked = unwrap_callsite(marked);
                    let (next_block, _) = self.get_new_block(&builder)?;
                    builder.build_conditional_branch(
                        marked.into_int_value(),
                        &next_block,
                        &end_block,
                    );
                    builder.position_at_end(&next_block);

                    let elem_type_id = self.type_ids[&ty];
                    builder.build_call(
                        gc_visit_array,
                        &[
                            mark_ptr.into(),
                            self.context
                                .i16_type()
                                .const_int(elem_type_id as u64, false)
                                .into(),
                        ],
                        &temp_name(),
                    );
                }

                AstType::Tuple { types } => {
                    let tuple_ptr = builder.build_pointer_cast(
                        ptr_param,
                        ptr_type(self.get_type(t)?, GLOBAL).into_pointer_type(),
                        &temp_name(),
                    );

                    for (idx, ty) in types.iter().enumerate() {
                        let subty_id = self.type_ids[&ty];
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
                            ],
                            &temp_name(),
                        );
                    }
                }

                _ => unreachable!(),
            }

            builder.build_unconditional_branch(&end_block);
            switch.push((id, block));
        }

        let switch: Vec<_> = switch
            .iter()
            .map(|(id, block)| (self.context.i16_type().const_int(*id as u64, false), block))
            .collect();
        builder.build_switch(id_param, &end_block, &switch);
        end_builder.build_return(None);

        Ok(())
    }

    fn get_break(&self) -> &BasicBlock {
        self.break_continue.as_ref().map(|(b, _)| b).unwrap()
    }

    fn get_continue(&self) -> &BasicBlock {
        self.break_continue.as_ref().map(|(_, c)| c).unwrap()
    }

    fn set_break_continue(
        &mut self,
        mut break_continue: Option<(BasicBlock, BasicBlock)>,
    ) -> Option<(BasicBlock, BasicBlock)> {
        std::mem::swap(&mut self.break_continue, &mut break_continue);

        break_continue
    }

    fn get_new_block(&self, builder: &Builder) -> PResult<(BasicBlock, Builder)> {
        let b = builder.get_insert_block().unwrap();
        let p = b.get_parent().unwrap();

        let block = p.append_basic_block(&temp_name());
        let builder = Builder::create();
        builder.position_at_end(&block);
        Ok((block, builder))
    }

    fn get_type(&self, t: &AstType) -> PResult<BasicTypeEnum> {
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
                let ty = self.get_type(ty)?;
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
                    .map(|t| self.get_type(t))
                    .collect::<PResult<Vec<BasicTypeEnum>>>()?;
                self.context.struct_type(&types, false).into()
            }

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
            let mut subset = vals;
            let mut bundled_value: AggregateValueEnum =
                self.get_type(t)?.into_struct_type().get_undef().into();

            for (i, subty) in types.iter().enumerate() {
                let (subvals, remaining) = subset.split_at(num_subvals(subty));
                let bundled_subval = self.bundle_vals(builder, subvals, subty)?;

                // Insert the bundled set of values...
                bundled_value = builder
                    .build_insert_value(bundled_value, bundled_subval, i as u32, &temp_name())
                    .unwrap();

                // The rest of the values will be used to build the rest of the tuple.
                subset = remaining;
            }

            // I hate that I can't just change this enum into BasicValueEnum...
            if let AggregateValueEnum::StructValue(bundled_value) = bundled_value {
                Ok(bundled_value.into())
            } else {
                unreachable!()
            }
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
        } else {
            let ptr = builder.build_alloca(self.get_type(t)?, &temp_name());
            builder.build_store(ptr, v);

            Ok(vec![ptr])
        }
    }
}

fn set_fn_attrs(context: &Context, fun: FunctionValue) {
    fun.add_attribute(
        AttributeLoc::Function,
        context.create_enum_attribute(Attribute::get_named_enum_kind_id("nounwind"), 1),
    );
}

fn is_array_deref(sig: &InstFunctionSignature) -> PResult<bool> {
    return Ok(&sig.0.full_name()? == "std::internal::operators::deref_array");
}

fn is_array_deref_assign(sig: &InstFunctionSignature) -> PResult<bool> {
    return Ok(&sig.0.full_name()? == "std::internal::operators::deref_array_assign");
}

fn is_array_len(sig: &InstFunctionSignature) -> PResult<bool> {
    return Ok(&sig.0.full_name()? == "std::internal::operators::array_len");
}

lazy_static! {
    static ref TEMP_NAME_COUNTER: RwLock<usize> = RwLock::new(1);
}

fn temp_name() -> String {
    let mut id_ref = TEMP_NAME_COUNTER.write().unwrap();
    *id_ref += 1;
    let id: usize = *id_ref;

    format!("temp_{}", id)
}
