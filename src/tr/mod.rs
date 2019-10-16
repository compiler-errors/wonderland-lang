use self::decorate::*;
use crate::inst::{InstObjectSignature, InstantiatedProgram};
use crate::parser::ast::*;
use crate::util::{IntoError, PError, PResult, ZipExact};
use either::Either;
use inkwell::attributes::{Attribute, AttributeLoc};
use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::passes::{PassManager, PassManagerBuilder};
use inkwell::types::*;
use inkwell::values::*;
use inkwell::{AddressSpace, OptimizationLevel};
use std::collections::HashMap;
use std::ffi::{OsStr, OsString};
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::sync::RwLock;
use tempfile::TempDir;

mod decorate;

const MANAGED: AddressSpace = AddressSpace::Generic; /* This is fucked. */
const GLOBAL: AddressSpace = AddressSpace::Generic; /* This too. */

lazy_static! {
    static ref STDLIB_PATH: &'static OsStr = OsStr::new("std/lib.c");
    static ref LIBC_PATH: &'static OsStr = if cfg!(target_os = "linux") {
        OsStr::new("/usr/lib/libc.a")
    } else if cfg!(target_os = "macos") {
        OsStr::new("/lib/libc.dylib")
    } else {
        panic!("Unsupported OS")
    };
}

type AccessSignature = Vec<usize>;

struct Translator {
    context: Context,
    module: Module,

    gcroot: FunctionValue,
    alloc_string: FunctionValue,
    alloc_object: FunctionValue,
    alloc_array: FunctionValue,

    break_continue: Option<(BasicBlock, BasicBlock)>,

    variables: HashMap<VariableId, (PointerValue, Vec<PointerValue>)>,
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

    println!("{}", module.print_to_string().to_string());

    if let Result::Err(why) = module.verify() {
        println!("{}", module.print_to_string().to_string());

        PError::new(format!("LLVM: {}", why.to_string()));
    }

    let pmb = PassManagerBuilder::create();
    pmb.set_optimization_level(OptimizationLevel::Aggressive);
    let pm = PassManager::create(());
    pmb.populate_module_pass_manager(&pm);
    pm.run_on(&module);

    //println!("{}", module.print_to_string().to_string());

    if output_file != "-" {
        emit_module(module, llvm_ir, output_file, included_files)?;
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
        module.write_bitcode_to_path(Path::new(&ll));

        let mut command = Command::new(format!("clang"));

        command
            .args(&[
                *STDLIB_PATH,
                &ll,
                OsStr::new("-o"),
                OsStr::new(output_file),
                OsStr::new("-O3"),
            ])
            .args(&included_files)
            .stdin(Stdio::null())
            .stdout(Stdio::null());

        println!("Executing command: {:?}", command);

        let clang_status = command
            .status()
            .map_err(|e| PError::new(format!("Command Error (ld): {}", e)))?;

        if !clang_status.success() {
            return PResult::error(format!(
                "Program `ld` exited with code: {}",
                clang_status.code().unwrap_or(-1),
            ));
        }
    }

    Ok(())
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

        let gcroot = module.add_function(
            "llvm.gcroot",
            context.void_type().fn_type(
                &[
                    context
                        .i8_type()
                        .ptr_type(MANAGED)
                        .ptr_type(GLOBAL /* TODO: This is wrong. */)
                        .into(),
                    context.i8_type().ptr_type(GLOBAL).into(),
                ],
                false,
            ),
            None,
        );
        set_nounwind(&context, gcroot);

        let alloc_string = module.add_function(
            "alloc_string",
            module
                .get_type("string")
                .unwrap()
                .into_struct_type()
                .ptr_type(MANAGED)
                .fn_type(
                    &[
                        context.i8_type().ptr_type(GLOBAL).into(),
                        context.i64_type().into(),
                    ],
                    false,
                ),
            None,
        );
        set_nounwind(&context, alloc_string);

        let alloc_object = module.add_function(
            "alloc_object",
            context
                .i8_type()
                .ptr_type(MANAGED)
                .fn_type(&[context.i64_type().into()], false),
            None,
        );
        set_nounwind(&context, alloc_object);

        let alloc_array = module.add_function(
            "alloc_array",
            context.i8_type().ptr_type(MANAGED).fn_type(
                &[context.i64_type().into(), context.i64_type().into()],
                false,
            ),
            None,
        );
        set_nounwind(&context, alloc_array);

        Translator {
            module,
            context,

            gcroot,
            alloc_string,
            alloc_object,
            alloc_array,

            break_continue: None,

            variables: HashMap::new(),
        }
    }

    fn translate(&mut self, file: InstantiatedProgram) -> PResult<()> {
        // Forward declaration.

        for (sig, _) in &file.instantiated_objects {
            let name = decorate_object(&sig.0, &sig.1)?;
            self.context.opaque_struct_type(&name);
        }

        for (sig, obj) in file.instantiated_objects {
            self.translate_object(sig, obj.unwrap())?;
        }

        for (sig, fun) in &file.instantiated_fns {
            let fun = fun.as_ref().unwrap();
            let name = decorate_fn(&sig.0, &sig.1)?;
            self.forward_declare_function(&name, &fun.parameter_list, &fun.return_type)?;
        }

        for (sig, fun) in &file.instantiated_object_fns {
            let fun = fun.as_ref().unwrap();
            let name = decorate_object_fn(&sig.0, &sig.1, &sig.2, &sig.3)?;
            self.forward_declare_function(&name, &fun.parameter_list, &fun.return_type)?;
        }

        for (sig, fun) in &file.instantiated_fns {
            let fun = fun.as_ref().unwrap();
            let name = decorate_fn(&sig.0, &sig.1)?;

            self.translate_function(
                &name,
                &fun.parameter_list,
                &fun.return_type,
                &fun.variables,
                &fun.definition,
            )?;
        }

        for (sig, fun) in &file.instantiated_object_fns {
            let fun = fun.as_ref().unwrap();
            let name = decorate_object_fn(&sig.0, &sig.1, &sig.2, &sig.3)?;

            self.translate_function(
                &name,
                &fun.parameter_list,
                &fun.return_type,
                &fun.variables,
                &fun.definition,
            )?;
        }

        self.translate_main(&file.main_fn)?;

        Ok(())
    }

    fn translate_main(&mut self, main_fn: &ModuleRef) -> PResult<()> {
        let main_fn_name = decorate_fn(main_fn, &[])?;
        let cheshire_main_fn = self.module.get_function(&main_fn_name).unwrap();

        let real_main_fn = self.forward_declare_function("main", &[], &AstType::Int)?;
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
    ) -> PResult<FunctionValue> {
        let param_tys = parameter_list
            .iter()
            .map(|p| self.get_type(&p.ty))
            .collect::<PResult<Vec<_>>>()?;
        let ret = self.get_type(return_type)?;
        let fun_ty = fun_type(ret, &param_tys);

        let llvm_fun = self.module.add_function(&name, fun_ty, None);

        llvm_fun.set_gc("shadow-stack");
        set_nounwind(&self.context, llvm_fun);

        Ok(llvm_fun)
    }

    fn translate_object(&mut self, sig: InstObjectSignature, obj: AstObject) -> PResult<()> {
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
        definition: &Option<AstBlock>,
    ) -> PResult<()> {
        let llvm_fun = self.module.get_function(&fun_name).unwrap();

        if let Some(definition) = definition {
            let param_values: HashMap<_, _> =
                ZipExact::zip_exact(parameter_list, &llvm_fun.get_params(), "params")?
                    .map(|(p, v)| (p.id, v.clone()))
                    .collect();

            let block = llvm_fun.append_basic_block("pre");
            let first_builder = Builder::create();
            first_builder.position_at_end(&block);

            for (id, var) in variables {
                let ty = self.get_type(&var.ty)?;
                let loc = first_builder.build_alloca(ty, &temp_name());

                let param = param_values
                    .get(&id)
                    .map(|f| Ok(f.clone()))
                    .unwrap_or_else(|| type_zero(ty))?;
                first_builder.build_store(loc, param);

                let subtys = flatten_type(&var.ty);

                // In this case, the `loc` is already basically a spilled version
                // of itself.
                // I'm sorry this predicate is kinda stupid. Basically saying if
                // the spilled version of the object is just 1 type, and the signature
                // of that is just [], meaning "nothing", then we already have allocated
                // a space to spill it.
                let spilled = if subtys.len() == 1 && subtys[0].0 == vec![] {
                    self.call_gcroot(&first_builder, loc)?;
                    first_builder.build_store(loc, param);

                    vec![loc]
                } else {
                    self.spill_value(&first_builder, param, &var.ty, true)?
                };

                self.variables.insert(*id, (loc, spilled));
            }

            let start_block = llvm_fun.append_basic_block("start");
            first_builder.build_unconditional_branch(&start_block);

            let start_builder = Builder::create();
            start_builder.position_at_end(&start_block);

            let (ret, _) = self.translate_block(&start_builder, &definition)?;
            start_builder.build_return(Some(&ret));
        }

        Ok(())
    }

    fn translate_block(
        &mut self,
        builder: &Builder,
        block: &AstBlock,
    ) -> PResult<(BasicValueEnum, Vec<PointerValue>)> {
        if self.has_break_continue() && !block.locals.is_empty() {
            let (inner_block, inner_builder) = self.get_new_block(builder)?;
            let (break_block, break_builder) = self.get_new_block(builder)?;
            let (continue_block, continue_builder) = self.get_new_block(builder)?;
            let (after_block, _after_builder) = self.get_new_block(builder)?;

            builder.build_unconditional_branch(&inner_block);
            let (old_break, old_continue) = self
                .set_break_continue(Some((break_block, continue_block)))
                .unwrap();

            for s in &block.statements {
                self.translate_statement(&inner_builder, s)?;
            }

            let (val, temps) = self.translate_expression(&inner_builder, &block.expression)?;

            self.set_break_continue(Some((old_break, old_continue)));
            for f in &block.locals {
                let (_, temps) = &self.variables[f];
                self.free_temps(&inner_builder, temps)?;
                self.free_temps(&break_builder, temps)?;
                self.free_temps(&continue_builder, temps)?;
            }

            inner_builder.build_unconditional_branch(&after_block);
            break_builder.build_unconditional_branch(self.get_break());
            continue_builder.build_unconditional_branch(self.get_continue());
            builder.position_at_end(&after_block);

            Ok((val, temps))
        } else {
            for s in &block.statements {
                self.translate_statement(&builder, s)?;
            }

            self.translate_expression(&builder, &block.expression)
        }
    }

    fn translate_statement(&mut self, builder: &Builder, statement: &AstStatement) -> PResult<()> {
        match statement {
            AstStatement::Let { .. } => unreachable!(),
            AstStatement::Assert { .. } => unimplemented!(),

            AstStatement::Expression { expression } => {
                let (_, temps) = self.translate_expression(builder, expression)?;
                self.free_temps(builder, &temps)?;
            }

            AstStatement::While { condition, block } => {
                let (condition_block, condition_builder) = self.get_new_block(builder)?;
                let (loop_block, loop_builder) = self.get_new_block(builder)?;
                let (end_block, _) = self.get_new_block(builder)?;

                builder.build_unconditional_branch(&condition_block);
                builder.position_at_end(&end_block);

                let (condition, temps) =
                    self.translate_expression(&condition_builder, condition)?;
                self.free_temps(&condition_builder, &temps)?;
                condition_builder.build_conditional_branch(
                    condition.into_int_value(),
                    &loop_block,
                    &end_block,
                );

                let old_break_continue =
                    self.set_break_continue(Some((end_block, condition_block)));
                let (_, temps) = self.translate_block(&loop_builder, block)?;
                self.free_temps(&loop_builder, &temps)?;
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
                let (val, _temps) = self.translate_expression(builder, value)?;

                builder.build_return(Some(&val));

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
    ) -> PResult<(BasicValueEnum, Vec<PointerValue>)> {
        let value: BasicValueEnum = match &expression.data {
            AstExpressionData::SelfRef => unreachable!(),
            AstExpressionData::Unimplemented => unreachable!(),

            AstExpressionData::True => self.context.bool_type().const_int(1, false).into(),
            AstExpressionData::False => self.context.bool_type().const_int(1, false).into(),

            AstExpressionData::Null => match self.get_type(&expression.ty)? {
                BasicTypeEnum::PointerType(p) => p.const_null().into(),
                _ => unreachable!(),
            },

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

                unwrap_callsite(fn_ret)
            }

            AstExpressionData::Int(i) => self
                .context
                .i64_type()
                .const_int_from_string(&i, StringRadix::Decimal)
                .unwrap()
                .into(),
            AstExpressionData::Char(c) => {
                let mut k = [0u8];
                c.encode_utf8(&mut k);
                self.context.i8_type().const_int(k[0] as u64, false).into()
            }

            // Lval-always expressions. Can be calculated by getting the lval then deref'ing.
            AstExpressionData::Identifier { .. }
            | AstExpressionData::ObjectAccess { .. }
            | AstExpressionData::ArrayAccess { .. } => {
                let (ptr, temps) = self.translate_expression_lval(builder, expression)?;
                self.free_temps(builder, &temps)?;
                builder.build_load(ptr, &temp_name())
            }

            AstExpressionData::Tuple { values } => {
                let mapped = values
                    .iter()
                    .map(|e| self.translate_expression(builder, e))
                    .collect::<PResult<Vec<(BasicValueEnum, Vec<PointerValue>)>>>()?;
                let (values, temps): (Vec<BasicValueEnum>, Vec<Vec<_>>) =
                    mapped.into_iter().unzip();
                let temps: Vec<_> = temps.into_iter().flat_map(|e| e.into_iter()).collect();

                self.free_temps(builder, &temps)?;

                let mut obj: StructValue = self
                    .get_type(&expression.ty)?
                    .into_struct_type()
                    .get_undef();

                for (i, v) in values.into_iter().enumerate() {
                    obj = builder
                        .build_insert_value(obj, v, i as u32, &temp_name())
                        .unwrap()
                        .into_struct_value();
                }

                obj.into()
            }

            AstExpressionData::ArrayLiteral { elements } => {
                let mapped = elements
                    .into_iter()
                    .map(|e| self.translate_expression(builder, e))
                    .collect::<PResult<Vec<(BasicValueEnum, Vec<PointerValue>)>>>()?;
                let (elements, temps): (Vec<BasicValueEnum>, Vec<Vec<_>>) =
                    mapped.into_iter().unzip();
                let temps: Vec<_> = temps.into_iter().flat_map(|e| e.into_iter()).collect();

                // Allocating the array.
                let element_ty = self.get_type(&AstType::get_element(&expression.ty)?)?;
                let array_ty = self.get_type(&expression.ty)?;
                let ty_size = self.type_size(element_ty)?;
                let num_elements = self
                    .context
                    .i64_type()
                    .const_int(elements.len() as u64, false);

                let ptr = builder.build_call(
                    self.alloc_array,
                    &[ty_size.into(), num_elements.into()],
                    &temp_name(),
                );
                let ptr = builder
                    .build_bitcast(unwrap_callsite(ptr), array_ty, &temp_name())
                    .into_pointer_value();

                self.free_temps(builder, &temps)?;

                for (idx, &e) in elements.iter().enumerate() {
                    let idx = self.context.i64_type().const_int(idx as u64, false);
                    let loc = self.get_array_idx(builder, ptr, idx)?;
                    builder.build_store(loc, e);
                }

                ptr.into()
            }

            AstExpressionData::Call {
                fn_name,
                generics,
                args,
            } => {
                let name = decorate_fn(fn_name, generics)?;
                let fun = self.module.get_function(&name).unwrap();

                let mapped = args
                    .into_iter()
                    .map(|e| self.translate_expression(builder, e))
                    .collect::<PResult<Vec<(BasicValueEnum, Vec<PointerValue>)>>>()?;
                let (args, temps): (Vec<BasicValueEnum>, Vec<Vec<_>>) = mapped.into_iter().unzip();
                let temps: Vec<_> = temps.into_iter().flat_map(|e| e.into_iter()).collect();

                let fn_ret = builder.build_call(fun, &args, &temp_name());
                self.free_temps(builder, &temps)?;
                unwrap_callsite(fn_ret)
            }

            AstExpressionData::ObjectCall { .. } => unreachable!(),

            AstExpressionData::StaticCall {
                call_type,
                fn_name,
                fn_generics,
                args,

                associated_trait,
                impl_signature: _,
            } => {
                let name = decorate_object_fn(
                    call_type,
                    associated_trait.as_ref().unwrap(),
                    fn_name,
                    fn_generics,
                )?;
                let fun = self.module.get_function(&name).unwrap();

                let mapped = args
                    .into_iter()
                    .map(|e| self.translate_expression(builder, e))
                    .collect::<PResult<Vec<(BasicValueEnum, Vec<PointerValue>)>>>()?;
                let (args, temps): (Vec<BasicValueEnum>, Vec<Vec<_>>) = mapped.into_iter().unzip();
                let temps: Vec<_> = temps.into_iter().flat_map(|e| e.into_iter()).collect();

                let fn_ret = builder.build_call(fun, &args, &temp_name());
                self.free_temps(builder, &temps)?;
                unwrap_callsite(fn_ret)
            }

            AstExpressionData::TupleAccess { accessible, idx } => {
                let (tup, temps) = self.translate_expression(&builder, &accessible)?;
                let tup = tup.into_struct_value();
                self.free_temps(builder, &temps)?;

                builder
                    .build_extract_value(tup, *idx as u32, &temp_name())
                    .unwrap()
            }

            AstExpressionData::AllocateObject { object } => {
                let ptr_type = self.get_type(object)?.into_pointer_type();
                let size = self.type_size(ptr_type.get_element_type().into_struct_type().into())?;
                let val = builder.build_call(self.alloc_object, &[size.into()], &temp_name());
                let ptr = unwrap_callsite(val);
                builder.build_bitcast(ptr, ptr_type, &temp_name())
            }

            AstExpressionData::Not(_expr) => unimplemented!(),
            AstExpressionData::Negate(_expr) => unimplemented!(),

            AstExpressionData::Assign { lhs, rhs } => {
                let (lval, lval_temps) = self.translate_expression_lval(builder, &lhs)?;
                let (rval, rval_temps) = self.translate_expression(builder, &rhs)?;

                self.free_temps(builder, &lval_temps)?;
                builder.build_store(lval, rval);
                return Ok((rval, rval_temps));
            }

            AstExpressionData::BinOp {
                kind: _,
                lhs: _,
                rhs: _,
            } => unreachable!(),

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

                let (condition, temps) = self.translate_expression(builder, &condition)?;
                self.free_temps(builder, &temps)?;
                builder.build_conditional_branch(
                    condition.into_int_value(),
                    &then_block,
                    &else_block,
                );

                let (then_value, temps) = self.translate_block(&then_builder, &then_ast_block)?;
                self.free_temps(&then_builder, &temps)?;
                then_builder.build_unconditional_branch(&after_block);

                let (else_value, temps) = self.translate_block(&else_builder, &else_ast_block)?;
                self.free_temps(&else_builder, &temps)?;
                else_builder.build_unconditional_branch(&after_block);

                builder.position_at_end(&after_block);
                let if_type = self.get_type(&expression.ty)?;
                let phi = builder.build_phi(if_type, &temp_name());
                phi.add_incoming(&[(&then_value, &then_block), (&else_value, &else_block)]);

                phi.as_basic_value()
            }
        };

        let spill = self.spill_value(builder, value, &expression.ty, false)?;
        Ok((value, spill))
    }

    fn get_array_idx(
        &mut self,
        builder: &Builder,
        loc: PointerValue,
        idx: IntValue,
    ) -> PResult<PointerValue> {
        let struct_val = builder.build_load(loc, &temp_name()).into_struct_value();
        let array_ptr = builder
            .build_extract_value(struct_val, 1, &temp_name())
            .unwrap()
            .into_pointer_value();

        let elem_ptr = unsafe { builder.build_gep(array_ptr, &[idx], &temp_name()) };

        Ok(elem_ptr)
    }

    fn translate_expression_lval(
        &mut self,
        builder: &Builder,
        expression: &AstExpression,
    ) -> PResult<(PointerValue, Vec<PointerValue>)> {
        let (value, spill) = match &expression.data {
            AstExpressionData::Identifier { variable_id, .. } => {
                let variable_id = &variable_id.unwrap();
                (self.variables[variable_id].0, Vec::new())
            }
            AstExpressionData::ArrayAccess { accessible, idx } => {
                let (arr, arr_temps) = self.translate_expression(builder, &accessible)?;
                let (idx, idx_temps) = self.translate_expression(builder, &idx)?;

                self.free_temps(builder, &idx_temps)?;
                let elem =
                    self.get_array_idx(builder, arr.into_pointer_value(), idx.into_int_value())?;

                (elem, arr_temps)
            }
            AstExpressionData::ObjectAccess {
                object, mem_idx, ..
            } => {
                let (object, temps) = self.translate_expression(builder, &object)?;

                let member = unsafe {
                    builder.build_gep(
                        object.into_pointer_value(),
                        &[
                            self.context.i64_type().const_int(0, false),
                            self.context
                                .i32_type()
                                .const_int(mem_idx.unwrap() as u64, false),
                        ],
                        &temp_name(),
                    )
                };

                (member, temps)
            }
            AstExpressionData::TupleAccess { accessible, idx } => {
                let (object, temps) = self.translate_expression_lval(builder, &accessible)?;

                let member = unsafe {
                    builder.build_gep(
                        object,
                        &[
                            self.context.i32_type().const_int(0, false),
                            self.context.i64_type().const_int(*idx as u64, false),
                        ],
                        &temp_name(),
                    )
                };

                (member, temps)
            }
            _ => unreachable!(),
        };

        Ok((value, spill))
    }

    fn translate_access(
        &self,
        builder: &Builder,
        mut value: BasicValueEnum,
        sig: &AccessSignature,
    ) -> PResult<BasicValueEnum> {
        for &idx in sig {
            value = builder
                .build_extract_value(value.into_struct_value(), idx as u32, &temp_name())
                .unwrap();
        }

        Ok(value)
    }

    fn spill_value(
        &mut self,
        builder: &Builder,
        value: BasicValueEnum,
        ty: &AstType,
        is_first: bool,
    ) -> PResult<Vec<PointerValue>> {
        let spillable = flatten_type(&ty);
        if spillable.is_empty() {
            return Ok(vec![]);
        }

        // Get the first builder, or make it if we don't have it.
        // This is where we need to put the gc stuff.
        let first_builder = if is_first {
            None
        } else {
            Some(self.get_first_builder(builder)?)
        };

        let mut ptrs = Vec::new();

        for (sig, ty) in spillable {
            let first_builder = if is_first {
                builder
            } else {
                first_builder.as_ref().unwrap()
            };

            let ty = self.get_type(&ty)?;
            let loc = first_builder.build_alloca(ty, &temp_name());
            let zero = type_zero(ty)?;

            first_builder.build_store(loc, zero);

            let v = self.translate_access(builder, value, &sig)?;
            builder.build_store(loc, v);

            self.call_gcroot(first_builder, loc)?;

            ptrs.push(loc);
        }

        Ok(ptrs)
    }

    fn free_temps(&self, builder: &Builder, temps: &[PointerValue]) -> PResult<()> {
        for &val in temps {
            let ptr = val.get_type().get_element_type().into_pointer_type();
            builder.build_store(val, ptr.const_null());
        }

        Ok(())
    }

    fn has_break_continue(&self) -> bool {
        self.break_continue.is_some()
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

    fn get_first_builder(&mut self, builder: &Builder) -> PResult<Builder> {
        let first_builder = Builder::create();

        let b = builder.get_insert_block().unwrap(); /* Yes, builder is always assigned to a block. */
        let p = b.get_parent().unwrap(); /* Yes, block is always attached to a function. */
        let b = p.get_first_basic_block().unwrap(); /* Yes, we already have a first block. */
        let i = b.get_last_instruction().unwrap(); /* This already has a jump instruction. */

        first_builder.position_before(&i);

        Ok(first_builder)
    }

    fn get_new_block(&mut self, builder: &Builder) -> PResult<(BasicBlock, Builder)> {
        let b = builder.get_insert_block().unwrap();
        let p = b.get_parent().unwrap();

        let block = p.append_basic_block(&temp_name());
        let builder = Builder::create();
        builder.position_at_end(&block);
        Ok((block, builder))
    }

    fn call_gcroot(&self, builder: &Builder, loc: PointerValue) -> PResult<()> {
        let val = builder.build_bitcast(
            loc,
            self.context
                .i8_type()
                .ptr_type(AddressSpace::Generic)
                .ptr_type(AddressSpace::Generic),
            &temp_name(),
        );
        builder.build_call(
            self.gcroot,
            &[
                val,
                self.context
                    .i8_type()
                    .ptr_type(AddressSpace::Generic)
                    .const_null()
                    .into(),
            ],
            &temp_name(),
        );

        Ok(())
    }

    fn get_type(&mut self, t: &AstType) -> PResult<BasicTypeEnum> {
        let ty = match t {
            AstType::Int => self.context.i64_type().into(),
            AstType::Char => self.context.i8_type().into(),
            AstType::Bool => self.context.bool_type().into(),

            AstType::String => ptr_type(self.module.get_type("string").unwrap(), MANAGED).into(),

            AstType::Object(name, generics) => {
                let name = decorate_object(name, generics)?;
                let ty = self.module.get_type(&name).unwrap();
                ptr_type(ty, MANAGED)
            }

            AstType::Array { ty } => {
                let ty = self.get_type(ty)?;
                let ctx = self
                    .context
                    .struct_type(
                        &[self.context.i64_type().into(), ptr_type(ty, MANAGED)],
                        false,
                    )
                    .into();
                ptr_type(ctx, MANAGED)
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

    fn type_size(&self, t: BasicTypeEnum) -> PResult<IntValue> {
        let ty = match t {
            BasicTypeEnum::StructType(t) => t.size_of().unwrap(),
            BasicTypeEnum::ArrayType(t) => t.size_of().unwrap(),
            BasicTypeEnum::IntType(t) => t.size_of(),
            BasicTypeEnum::FloatType(t) => t.size_of(),
            BasicTypeEnum::PointerType(t) => t.size_of(),
            BasicTypeEnum::VectorType(t) => t.size_of().unwrap(),
        };

        Ok(ty)
    }
}

fn set_nounwind(context: &Context, fun: FunctionValue) {
    fun.add_attribute(
        AttributeLoc::Function,
        context.create_enum_attribute(Attribute::get_named_enum_kind_id("nounwind"), 1),
    );
}

fn flatten_type(t: &AstType) -> Vec<(AccessSignature, AstType)> {
    match t {
        AstType::Int | AstType::Char | AstType::Bool => vec![],

        AstType::String | AstType::Object(..) | AstType::Array { .. } => vec![(vec![], t.clone())],

        AstType::Tuple { types } => types
            .into_iter()
            .enumerate()
            .flat_map(|(i, v)| {
                let mut sigs = flatten_type(v);

                for (sig, _) in &mut sigs {
                    sig.insert(0, i);
                }

                sigs
            })
            .collect(),

        _ => unreachable!(),
    }
}

fn fun_type(t: BasicTypeEnum, p: &[BasicTypeEnum]) -> FunctionType {
    match t {
        BasicTypeEnum::StructType(t) => t.fn_type(p, false),
        BasicTypeEnum::ArrayType(t) => t.fn_type(p, false),
        BasicTypeEnum::IntType(t) => t.fn_type(p, false),
        BasicTypeEnum::FloatType(t) => t.fn_type(p, false),
        BasicTypeEnum::PointerType(t) => t.fn_type(p, false),
        BasicTypeEnum::VectorType(t) => t.fn_type(p, false),
    }
}

fn ptr_type(t: BasicTypeEnum, a: AddressSpace) -> BasicTypeEnum {
    match t {
        BasicTypeEnum::StructType(t) => t.ptr_type(a),
        BasicTypeEnum::ArrayType(t) => t.ptr_type(a),
        BasicTypeEnum::IntType(t) => t.ptr_type(a),
        BasicTypeEnum::FloatType(t) => t.ptr_type(a),
        BasicTypeEnum::PointerType(t) => t.ptr_type(a),
        BasicTypeEnum::VectorType(t) => t.ptr_type(a),
    }
    .into()
}

fn type_zero(t: BasicTypeEnum) -> PResult<BasicValueEnum> {
    let ty: BasicValueEnum = match t {
        BasicTypeEnum::StructType(t) => t.const_zero().into(),
        BasicTypeEnum::ArrayType(t) => t.const_zero().into(),
        BasicTypeEnum::IntType(t) => t.const_zero().into(),
        BasicTypeEnum::FloatType(t) => t.const_zero().into(),
        BasicTypeEnum::PointerType(t) => t.const_zero().into(),
        BasicTypeEnum::VectorType(t) => t.const_zero().into(),
    };

    Ok(ty)
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

fn unwrap_callsite(callsite: CallSiteValue) -> BasicValueEnum {
    match callsite.try_as_basic_value() {
        Either::Left(x) => x,
        Either::Right(_) => unreachable!(),
    }
}
