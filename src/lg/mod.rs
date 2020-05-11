use crate::{
    ast::{
        AstBlock, AstExpression, AstExpressionData, AstFunction, AstGlobalVariable, AstLiteral,
        AstMatchBranch, AstMatchPattern, AstMatchPatternData, AstNamedVariable, AstObject,
        AstObjectFunction, AstStatement, InstructionArgument, InstructionOutput, ModuleRef,
        VariableId,
    },
    inst::{
        InstEnumRepresentation, InstEnumSignature, InstFunctionSignature,
        InstObjectFunctionSignature, InstObjectSignature, InstantiatedProgram,
    },
    lg::represent::{CheshireValue, LError, LResult, ShouldPopStack},
    util::{Expect, PResult, StackMap, ZipExact},
};
use std::{
    cell::RefCell,
    collections::HashMap,
    ops::{BitXor, Shr},
};

mod represent;

struct LookingGlass {
    pub main_fn: ModuleRef,

    pub program_fns: HashMap<InstFunctionSignature, AstFunction>,
    pub program_object_fns: HashMap<InstObjectFunctionSignature, AstObjectFunction>,
    pub program_objects: HashMap<InstObjectSignature, AstObject>,
    pub program_enums: HashMap<InstEnumSignature, InstEnumRepresentation>,
    pub program_globals: HashMap<ModuleRef, AstGlobalVariable>,

    pub global_variables: RefCell<HashMap<ModuleRef, CheshireValue>>,
}

pub fn evaluate(program: InstantiatedProgram) -> PResult<()> {
    let InstantiatedProgram {
        main_fn,

        instantiated_fns,
        instantiated_object_fns,
        instantiated_objects,
        instantiated_enums,
        instantiated_globals,
        ..
    } = program;

    let mut lg = LookingGlass {
        main_fn,

        program_fns: instantiated_fns,
        program_object_fns: instantiated_object_fns,
        program_objects: instantiated_objects,
        program_enums: instantiated_enums,
        program_globals: instantiated_globals,

        global_variables: RefCell::new(HashMap::new()),
    };

    let exit_code = lg.init_globals_and_evaluate_main();

    match exit_code {
        Ok(exit_code) => {
            info!("Exited normally with code `{}'", exit_code);
            Ok(())
        },
        Err(LError::Exit(exit_code)) => {
            info!("Exited early with code `{}'", exit_code);
            Ok(())
        },
        Err(LError::Return(_)) => unreachable!("Uncaught `return` in LookingGlass evaluation"),
        Err(LError::Continue(_)) => unreachable!("Uncaught `continue` in LookingGlass evaluation"),
        Err(LError::Break(..)) => unreachable!("Uncaught `break` in LookingGlass evaluation"),
        Err(LError::InternalException(err)) => Err(err),
    }
}

impl LookingGlass {
    fn init_globals_and_evaluate_main(&mut self) -> LResult<i64> {
        let mut global_variables = HashMap::new();

        for (var, val) in &self.program_globals {
            let mut scope = StackMap::new();
            debug!("Initializing `{}` as `{:?}`", val.name, val.init);
            let val = self.evaluate_expression(&val.init, &mut scope)?;
            global_variables.insert(var.clone(), val);
        }

        self.global_variables.get_mut().extend(global_variables);

        let main_signature = InstFunctionSignature(self.main_fn.clone(), vec![]);
        let exit_value = self.evaluate_function(&self.program_fns[&main_signature], vec![])?;

        if let CheshireValue::Int(exit_value) = exit_value {
            Ok(exit_value)
        } else {
            unreachable!(
                "Should always be getting an exit int from main @ `{}`",
                self.main_fn.full_name()
            )
        }
    }

    fn evaluate_function(
        &self,
        fun: &AstFunction,
        args: Vec<CheshireValue>,
    ) -> LResult<CheshireValue> {
        self.evaluate_functionish(&fun.parameter_list, args, fun.definition.as_ref().unwrap())
    }

    // TODO: This will likely be copypasta
    fn evaluate_object_function(
        &self,
        fun: &AstObjectFunction,
        args: Vec<CheshireValue>,
    ) -> LResult<CheshireValue> {
        self.evaluate_functionish(&fun.parameter_list, args, fun.definition.as_ref().unwrap())
    }

    fn evaluate_functionish(
        &self,
        parameters: &[AstNamedVariable],
        args: Vec<CheshireValue>,
        definition: &AstBlock,
    ) -> LResult<CheshireValue> {
        let mut scope = StackMap::new();
        scope.push();

        for (param, arg) in ZipExact::zip_exact(parameters, args, "arguments")? {
            scope.add(param.id, arg);
        }

        let value_result = self.evaluate_block(definition, &mut scope);
        let value = match value_result {
            Ok(value) => value,
            Err(LError::Return(value)) => value,
            Err(LError::Continue(_)) =>
                unreachable!("Uncaught `continue` in LookingGlass evaluation"),
            Err(LError::Break(..)) => unreachable!("Uncaught `break` in LookingGlass evaluation"),
            Err(other) => {
                return Err(other);
            },
        };

        scope.pop();

        if !scope.is_empty() {
            perror!(
                "Scope leaked somewhere. Expected `0` stack frames, got `{}`",
                scope.height()
            )?;
        }

        Ok(value)
    }

    fn evaluate_closure(
        &self,
        parameters: &[AstMatchPattern],
        args: Vec<CheshireValue>,
        env: HashMap<VariableId, CheshireValue>,
        definition: &AstExpression,
    ) -> LResult<CheshireValue> {
        let mut scope = StackMap::new();
        scope.push();

        for (id, val) in env {
            scope.add(id, val);
        }

        for (param, arg) in ZipExact::zip_exact(parameters, args, "arguments")? {
            if !self.apply_pattern(param, &arg, &mut scope)? {
                unreachable!();
            }
        }

        let value_result = self.evaluate_expression(definition, &mut scope);
        let value = match value_result {
            Ok(value) => value,
            Err(LError::Return(value)) => value,
            Err(LError::Continue(_)) =>
                unreachable!("Uncaught `continue` in LookingGlass evaluation"),
            Err(LError::Break(..)) => unreachable!("Uncaught `break` in LookingGlass evaluation"),
            Err(other) => {
                return Err(other);
            },
        };

        scope.pop();

        if !scope.is_empty() {
            perror!(
                "Scope leaked somewhere. Expected `0` stack frames, got `{}`",
                scope.height()
            )?;
        }

        Ok(value)
    }

    fn evaluate_expression(
        &self,
        expr: &AstExpression,
        scope: &mut StackMap<VariableId, CheshireValue>,
    ) -> LResult<CheshireValue> {
        let span = expr.span;

        let value = match &expr.data {
            AstExpressionData::SelfRef
            | AstExpressionData::Unimplemented
            | AstExpressionData::ExprCall { .. }
            | AstExpressionData::NamedEnum { .. }
            | AstExpressionData::PlainEnum { .. }
            | AstExpressionData::AllocateArray { .. }
            | AstExpressionData::As { .. }
            | AstExpressionData::For { .. }
            | AstExpressionData::Assert { .. }
            | AstExpressionData::ArrayAccess { .. }
            | AstExpressionData::ObjectCall { .. }
            | AstExpressionData::Not(_)
            | AstExpressionData::Negate(_)
            | AstExpressionData::BinOp { .. } => unreachable!(),

            AstExpressionData::Literal(AstLiteral::True) => CheshireValue::Int(1),
            AstExpressionData::Literal(AstLiteral::False) => CheshireValue::Int(0),
            AstExpressionData::Literal(AstLiteral::Char(chr)) => CheshireValue::Int(*chr as i64),
            AstExpressionData::Literal(AstLiteral::Int(int)) =>
                CheshireValue::Int(int.parse().unwrap()),
            AstExpressionData::Literal(AstLiteral::Float(float)) =>
                CheshireValue::Float(float.parse().unwrap()),
            AstExpressionData::Literal(AstLiteral::String(string)) =>
                CheshireValue::String(string.clone()),

            AstExpressionData::Identifier { variable_id, name } => scope
                .get(variable_id.as_ref().unwrap())
                .as_expected(span, "variable", name)?
                .clone(),

            AstExpressionData::GlobalVariable { name } => self
                .global_variables
                .borrow()
                .get(name)
                .as_expected(span, "global variable", &name.full_name())?
                .clone(),

            AstExpressionData::GlobalFn { name } => CheshireValue::GlobalFn(name.clone()),

            AstExpressionData::Tuple { values } =>
                CheshireValue::value_collection(self.evaluate_expressions(&values, scope)?),
            AstExpressionData::ArrayLiteral { elements } =>
                CheshireValue::heap_collection(self.evaluate_expressions(&elements, scope)?),

            AstExpressionData::Closure {
                params,
                expr,
                captured,
                ..
            } =>
                CheshireValue::closure(params.clone(), captured.clone().unwrap(), (**expr).clone()),

            AstExpressionData::FnCall {
                fn_name,
                generics,
                args,
            } => {
                // TODO(borrow_key): We could really do without these clones.
                let signature = InstFunctionSignature(fn_name.clone(), generics.clone());
                let args = self.evaluate_expressions(&args, scope)?;

                self.evaluate_function(&self.program_fns[&signature], args)?
            },

            AstExpressionData::StaticCall {
                call_type,
                fn_name,
                fn_generics,
                args,
                associated_trait,
                ..
            } => {
                // TODO(borrow_key): We could really do without these clones.
                let signature = InstObjectFunctionSignature(
                    call_type.clone(),
                    associated_trait.as_ref().map(|t| t.trt.clone()),
                    fn_name.clone(),
                    fn_generics.clone(),
                );
                let args = self.evaluate_expressions(&args, scope)?;

                self.evaluate_object_function(&self.program_object_fns[&signature], args)?
            },

            AstExpressionData::TupleAccess { accessible, idx } => self
                .evaluate_expression(accessible, scope)?
                .get_member(*idx)?,

            AstExpressionData::ObjectAccess {
                object, mem_idx, ..
            } => self
                .evaluate_expression(object, scope)?
                .get_member(mem_idx.unwrap())?,

            AstExpressionData::AllocateObject {
                children,
                children_idxes,
                ..
            } => {
                let children_idxes = children_idxes.as_ref().unwrap();
                let mut placeholders = vec![Option::None; children.len()];

                for (name, expr) in children {
                    placeholders[children_idxes[name]] =
                        Some(self.evaluate_expression(expr, scope)?);
                }

                let contents = placeholders.into_iter().map(Option::unwrap).collect();

                CheshireValue::heap_collection(contents)
            },

            AstExpressionData::Assign { lhs, rhs } => {
                let rhs = self.evaluate_expression(&rhs, scope)?;
                self.assign_lval(&lhs, rhs.clone(), scope)?;

                rhs
            },

            AstExpressionData::Block { block } => self.evaluate_block(block, scope)?,

            AstExpressionData::If {
                condition,
                block,
                else_block,
            } => {
                let condition = self.evaluate_expression(&condition, scope)?;

                if condition.is_true() {
                    self.evaluate_block(block, scope)?
                } else {
                    self.evaluate_block(else_block, scope)?
                }
            },

            AstExpressionData::Match {
                expression,
                branches,
            } => {
                let match_condition = self.evaluate_expression(expression, scope)?;
                let mut value = None;

                for AstMatchBranch {
                    pattern,
                    expression,
                } in branches
                {
                    scope.push();

                    debug!(
                        "Trying to apply pattern {:?} to {:?}",
                        pattern, match_condition
                    );
                    if self.apply_pattern(pattern, &match_condition, scope)? {
                        debug!("Pattern succeeded");
                        let value_result = self.evaluate_expression(expression, scope);
                        scope.pop();
                        value = Some(value_result?);
                        break;
                    } else {
                        debug!("Pattern failed");
                        scope.pop();
                    }
                }

                if let Some(value) = value {
                    value
                } else {
                    error!("Match was exhausted of branches");
                    return Err(LError::Exit(-1));
                }
            },

            AstExpressionData::While {
                id,
                condition,
                block,
                else_block,
                ..
            } => {
                let mut value = None;

                while self.evaluate_expression(&condition, scope)?.is_true() {
                    let value_result = self.evaluate_block(block, scope);

                    match value_result {
                        Err(LError::Continue(continue_id)) if continue_id == *id => {
                            continue;
                        },
                        Err(LError::Break(break_id, break_value)) if break_id == *id => {
                            value = Some(break_value);
                            break;
                        },
                        value_result => {
                            value = Some(value_result?);
                        },
                    }
                }

                if let Some(value) = value {
                    value
                } else {
                    self.evaluate_block(else_block, scope)?
                }
            },

            AstExpressionData::PositionalEnum {
                variant, children, ..
            } => {
                let contents = self.evaluate_expressions(children, scope)?;
                CheshireValue::enum_variant(variant.clone(), contents)
            },

            AstExpressionData::Break { id, value, .. } => {
                let value = self.evaluate_expression(&value, scope)?;
                return Err(LError::Break(id.unwrap(), value));
            },

            AstExpressionData::Continue { id, .. } => {
                return Err(LError::Continue(id.unwrap()));
            },
            AstExpressionData::Return { value } => {
                let value = self.evaluate_expression(&value, scope)?;
                return Err(LError::Return(value));
            },

            AstExpressionData::ConditionalCompilation { branches } => {
                let branch = &branches["looking_glass"];
                self.evaluate_block(branch, scope)?
            },

            AstExpressionData::Instruction {
                instruction,
                arguments,
                output,
            } => self.evaluate_instruction(instruction, arguments, output, scope)?,
        };

        Ok(value)
    }

    fn evaluate_instruction(
        &self,
        instruction: &str,
        arguments: &[InstructionArgument],
        output: &InstructionOutput,
        scope: &mut StackMap<VariableId, CheshireValue>,
    ) -> LResult<CheshireValue> {
        if let InstructionOutput::Type(_) = output {
            let value = match (instruction, arguments) {
                ("call", fun_and_args) => {
                    let mut args = self.evaluate_instruction_arguments(fun_and_args, scope)?;
                    let fun_value = args.remove(0);

                    match &fun_value {
                        CheshireValue::GlobalFn(name) => {
                            let fun_signature = InstFunctionSignature(name.clone(), vec![]);
                            let fun = &self.program_fns[&fun_signature];
                            self.evaluate_function(fun, args)?
                        },
                        CheshireValue::Closure {
                            parameters,
                            captured,
                            expression,
                        } => {
                            let mut env = HashMap::new();

                            for (old, new) in captured {
                                env.insert(new.id, scope.get(&old.id).unwrap());
                            }

                            self.evaluate_closure(parameters, args, env, expression)?
                        },
                        _ => unreachable!(),
                    }
                },
                ("add", [a, b]) => CheshireValue::Int(
                    self.evaluate_instruction_argument(a, scope)?
                        .unwrap_int()?
                        .wrapping_add(self.evaluate_instruction_argument(b, scope)?.unwrap_int()?),
                ),
                ("fadd", [a, b]) => CheshireValue::Float(
                    self.evaluate_instruction_argument(a, scope)?
                        .unwrap_float()?
                        + self
                            .evaluate_instruction_argument(b, scope)?
                            .unwrap_float()?,
                ),
                ("mul", [a, b]) => CheshireValue::Int(
                    self.evaluate_instruction_argument(a, scope)?
                        .unwrap_int()?
                        .wrapping_mul(self.evaluate_instruction_argument(b, scope)?.unwrap_int()?),
                ),
                ("fmul", [a, b]) => CheshireValue::Float(
                    self.evaluate_instruction_argument(a, scope)?
                        .unwrap_float()?
                        * self
                            .evaluate_instruction_argument(b, scope)?
                            .unwrap_float()?,
                ),
                ("sdiv", [a, b]) => CheshireValue::Int(
                    self.evaluate_instruction_argument(a, scope)?.unwrap_int()?
                        / self.evaluate_instruction_argument(b, scope)?.unwrap_int()?,
                ),
                ("fdiv", [a, b]) => CheshireValue::Float(
                    self.evaluate_instruction_argument(a, scope)?
                        .unwrap_float()?
                        / self
                            .evaluate_instruction_argument(b, scope)?
                            .unwrap_float()?,
                ),
                ("srem", [a, b]) => CheshireValue::Int(
                    self.evaluate_instruction_argument(a, scope)?.unwrap_int()?
                        % self.evaluate_instruction_argument(b, scope)?.unwrap_int()?,
                ),
                ("csub", [a, b]) => CheshireValue::Int(
                    self.evaluate_instruction_argument(a, scope)?.unwrap_int()?
                        - self.evaluate_instruction_argument(b, scope)?.unwrap_int()?,
                ),
                ("neg", [a]) =>
                    CheshireValue::Int(-self.evaluate_instruction_argument(a, scope)?.unwrap_int()?),
                ("fneg", [a]) => CheshireValue::Float(
                    -self
                        .evaluate_instruction_argument(a, scope)?
                        .unwrap_float()?,
                ),
                ("xor", [a, b]) => CheshireValue::Int(
                    self.evaluate_instruction_argument(a, scope)?
                        .unwrap_int()?
                        .bitxor(self.evaluate_instruction_argument(b, scope)?.unwrap_int()?),
                ),
                ("lshr", [a, b]) => {
                    let a = self.evaluate_instruction_argument(a, scope)?.unwrap_int()?;
                    let b = self.evaluate_instruction_argument(b, scope)?.unwrap_int()?;

                    CheshireValue::Int((a as u64).shr(b as u64) as i64)
                },
                ("int_to_float", [a]) => CheshireValue::Float(
                    self.evaluate_instruction_argument(a, scope)?.unwrap_int()? as f64,
                ),
                ("icmp eq", [a, b]) => CheshireValue::int_from_bool(
                    self.evaluate_instruction_argument(a, scope)?.unwrap_int()?
                        == self.evaluate_instruction_argument(b, scope)?.unwrap_int()?,
                ),
                ("icmp sgt", [a, b]) => CheshireValue::int_from_bool(
                    self.evaluate_instruction_argument(a, scope)?.unwrap_int()?
                        > self.evaluate_instruction_argument(b, scope)?.unwrap_int()?,
                ),
                ("fcmp eq", [a, b]) => CheshireValue::int_from_bool(
                    self.evaluate_instruction_argument(a, scope)?
                        .unwrap_float()?
                        == self
                            .evaluate_instruction_argument(b, scope)?
                            .unwrap_float()?,
                ),
                ("fcmp gt", [a, b]) => CheshireValue::int_from_bool(
                    self.evaluate_instruction_argument(a, scope)?
                        .unwrap_float()?
                        > self
                            .evaluate_instruction_argument(b, scope)?
                            .unwrap_float()?,
                ),
                ("add_string", [a, b]) => {
                    let a = self
                        .evaluate_instruction_argument(a, scope)?
                        .unwrap_string()?;
                    let b = self
                        .evaluate_instruction_argument(b, scope)?
                        .unwrap_string()?;

                    CheshireValue::String(a + &b)
                },
                ("print", [a]) => {
                    print!(
                        "{}",
                        self.evaluate_instruction_argument(a, scope)?
                            .unwrap_string()?
                    );
                    CheshireValue::value_collection(vec![])
                },
                ("int_to_string", [a]) => CheshireValue::String(format!(
                    "{}",
                    self.evaluate_instruction_argument(a, scope)?.unwrap_int()?,
                )),
                ("char_to_string", [a]) => CheshireValue::String(format!(
                    "{}",
                    self.evaluate_instruction_argument(a, scope)?.unwrap_int()? as u8 as char,
                )),
                ("float_to_string", [a]) => CheshireValue::String(format!(
                    "{}",
                    self.evaluate_instruction_argument(a, scope)?
                        .unwrap_float()?,
                )),
                ("ch_typestring", [InstructionArgument::Type(t)]) =>
                    CheshireValue::String(format!("{}", t)),
                ("undefined_value", []) => CheshireValue::Undefined,
                ("allocate_array_undefined", [len]) => {
                    let len = self
                        .evaluate_instruction_argument(len, scope)?
                        .unwrap_int()? as usize;
                    CheshireValue::heap_collection(vec![CheshireValue::Undefined; len])
                },
                ("exit", [code]) => {
                    let code = self
                        .evaluate_instruction_argument(code, scope)?
                        .unwrap_int()?;
                    return Err(LError::Exit(code));
                },
                ("array_deref", [array, idx]) => {
                    let array = self.evaluate_instruction_argument(array, scope)?;
                    let idx = self
                        .evaluate_instruction_argument(idx, scope)?
                        .unwrap_int()?;

                    if idx < 0 {
                        perror!(
                            "Oops, cannot access an array at negative indices. Should've checked \
                             this earlier..."
                        )?;
                    }

                    array.get_member(idx as usize)?
                },
                ("string_deref", [string, idx]) => {
                    let string = self.evaluate_instruction_argument(string, scope)?;
                    let idx = self
                        .evaluate_instruction_argument(idx, scope)?
                        .unwrap_int()?;

                    if idx < 0 {
                        perror!(
                            "Oops, cannot access an array at negative indices. Should've checked \
                             this earlier..."
                        )?;
                    }

                    let chr = string.unwrap_string()?.chars().nth(idx as usize).unwrap();
                    CheshireValue::Int(chr as i64)
                },
                ("array_store", [array, idx, value]) => {
                    let array = self.evaluate_instruction_argument(array, scope)?;
                    let value = self.evaluate_instruction_argument(value, scope)?;
                    let idx = self
                        .evaluate_instruction_argument(idx, scope)?
                        .unwrap_int()?;

                    if idx < 0 {
                        perror!(
                            "Oops, cannot access an array at negative indices. Should've checked \
                             this earlier..."
                        )?;
                    }

                    array.set_heap_member(idx as usize, value)?;
                    CheshireValue::value_collection(vec![])
                },
                ("array_len", [array]) => {
                    let array = self.evaluate_instruction_argument(array, scope)?;
                    CheshireValue::Int(array.array_len()? as i64)
                },
                ("string_len", [string]) => {
                    let string = self.evaluate_instruction_argument(string, scope)?;
                    CheshireValue::Int(string.string_len()? as i64)
                },
                ("reinterpret", [arg]) => self.evaluate_instruction_argument(arg, scope)?,
                ("breakpoint", []) => {
                    self.breakpoint();
                    CheshireValue::value_collection(vec![])
                },
                ("gc", []) => {
                    gc::force_collect();
                    CheshireValue::value_collection(vec![])
                },
                _ => perror!("Unknown instruction `{}`", instruction)?,
            };

            Ok(value)
        } else {
            perror!(
                "Anonymous instruction outputs not supported in instruction `{}`",
                instruction
            )?
        }
    }

    fn evaluate_instruction_argument(
        &self,
        arg: &InstructionArgument,
        scope: &mut StackMap<VariableId, CheshireValue>,
    ) -> LResult<CheshireValue> {
        match arg {
            InstructionArgument::Expression(expr) => self.evaluate_expression(expr, scope),
            InstructionArgument::Type(_) => unreachable!(),
            InstructionArgument::Anonymous(_) => unreachable!(),
        }
    }

    fn evaluate_instruction_arguments(
        &self,
        args: &[InstructionArgument],
        scope: &mut StackMap<VariableId, CheshireValue>,
    ) -> LResult<Vec<CheshireValue>> {
        let mut ret = vec![];

        for arg in args {
            match arg {
                InstructionArgument::Expression(expr) => {
                    ret.push(self.evaluate_expression(expr, scope)?);
                },
                InstructionArgument::Type(_) => unreachable!(),
                InstructionArgument::Anonymous(_) => unreachable!(),
            }
        }

        Ok(ret)
    }

    fn assign_lval(
        &self,
        lhs: &AstExpression,
        rhs: CheshireValue,
        scope: &mut StackMap<VariableId, CheshireValue>,
    ) -> LResult<()> {
        let (lhs_root, indices) = self.flatten_lval(lhs)?;
        self.assign_flattened(lhs_root, &indices, rhs, scope)?;

        Ok(())
    }

    fn flatten_lval<'a>(&self, lhs: &'a AstExpression) -> LResult<(&'a AstExpression, Vec<usize>)> {
        let pair = match &lhs.data {
            AstExpressionData::Identifier { .. } => (lhs, vec![]),

            AstExpressionData::ObjectAccess { .. } => (lhs, vec![]),

            AstExpressionData::GlobalVariable { .. } => (lhs, vec![]),

            AstExpressionData::TupleAccess { accessible, idx } => {
                let (root_lhs, mut indices) = self.flatten_lval(&accessible)?;
                indices.push(*idx);
                (root_lhs, indices)
            },

            _ => unreachable!(),
        };

        Ok(pair)
    }

    fn assign_flattened<'a>(
        &self,
        lhs_root: &'a AstExpression,
        indices: &[usize],
        rhs: CheshireValue,
        scope: &mut StackMap<VariableId, CheshireValue>,
    ) -> LResult<()> {
        let span = lhs_root.span;

        match &lhs_root.data {
            AstExpressionData::Identifier { variable_id, name } => {
                let mut lhs = scope
                    .get_mut(variable_id.as_ref().unwrap())
                    .as_expected(span, "variable", name)?;

                for idx in indices {
                    lhs = lhs.get_tuple_member_mut(*idx)?;
                }

                *lhs = rhs;
            },

            AstExpressionData::ObjectAccess {
                object, mem_idx, ..
            } => {
                let object = self.evaluate_expression(&object, scope)?;

                if let CheshireValue::HeapCollection { contents } = &object {
                    let mut contents = contents.borrow_mut();
                    let mut lhs = &mut contents[mem_idx.unwrap()];

                    for idx in indices {
                        lhs = lhs.get_tuple_member_mut(*idx)?;
                    }

                    *lhs = rhs;
                } else {
                    unreachable!()
                }
            },

            AstExpressionData::GlobalVariable { name } => {
                let mut globals = self.global_variables.borrow_mut();
                let mut lhs = globals.get_mut(name).unwrap();

                for idx in indices {
                    lhs = lhs.get_tuple_member_mut(*idx)?;
                }

                *lhs = rhs;
            },

            _ => unreachable!(),
        }

        Ok(())
    }

    fn evaluate_block(
        &self,
        block: &AstBlock,
        scope: &mut StackMap<VariableId, CheshireValue>,
    ) -> LResult<CheshireValue> {
        scope.push();

        for x in &block.statements {
            let stmt_result = self.evaluate_statement(x, scope);

            if stmt_result.should_pop_stack() {
                scope.pop();
            }

            stmt_result?;
        }

        let expr = self.evaluate_expression(&block.expression, scope);

        // We *always* can pop scope here, regardless of if we need to do control flow
        // or not.
        scope.pop();

        expr
    }

    fn evaluate_expressions(
        &self,
        exprs: &[AstExpression],
        scope: &mut StackMap<VariableId, CheshireValue>,
    ) -> LResult<Vec<CheshireValue>> {
        let mut ret = vec![];

        for expr in exprs {
            ret.push(self.evaluate_expression(expr, scope)?);
        }

        Ok(ret)
    }

    fn evaluate_statement(
        &self,
        stmt: &AstStatement,
        scope: &mut StackMap<VariableId, CheshireValue>,
    ) -> LResult<()> {
        match stmt {
            AstStatement::Let { pattern, value } => {
                let value = self.evaluate_expression(value, scope)?;

                if !self.apply_pattern(pattern, &value, scope)? {
                    unreachable!("Irrefutability should have been checked by now");
                }
            },
            AstStatement::Expression { expression } => {
                self.evaluate_expression(expression, scope)?;
            },
        }

        Ok(())
    }

    fn apply_pattern(
        &self,
        pattern: &AstMatchPattern,
        value: &CheshireValue,
        scope: &mut StackMap<VariableId, CheshireValue>,
    ) -> PResult<bool> {
        let truthy = match (&pattern.data, value) {
            (AstMatchPatternData::Underscore, _) => true,
            (AstMatchPatternData::Literal(literal), value) => self.match_literal(literal, value),
            (AstMatchPatternData::Identifier(var1), value) => {
                scope.add(var1.id, value.clone());
                true
            },
            (
                AstMatchPatternData::Tuple(pattern_children),
                CheshireValue::ValueCollection { contents },
            ) => {
                let mut matches = true;

                for (pat_child, val_child) in
                    ZipExact::zip_exact(pattern_children, contents, "tuple members")?
                {
                    if !self.apply_pattern(pat_child, val_child, scope)? {
                        matches = false;
                        break;
                    }
                }

                matches
            },
            (
                AstMatchPatternData::PositionalEnum {
                    variant: pattern_variant,
                    children: pattern_children,
                    ..
                },
                CheshireValue::EnumVariant {
                    variant: value_variant,
                    contents,
                },
            ) if pattern_variant == value_variant => {
                let mut matches = true;

                for (pat_child, tup_child) in
                    ZipExact::zip_exact(pattern_children, contents, "enum members")?
                {
                    if !self.apply_pattern(pat_child, tup_child, scope)? {
                        matches = false;
                        break;
                    }
                }

                matches
            },
            _ => false,
        };

        Ok(truthy)
    }

    fn match_literal(&self, literal: &AstLiteral, value: &CheshireValue) -> bool {
        match (literal, value) {
            (AstLiteral::True, CheshireValue::Int(1)) => true,
            (AstLiteral::False, CheshireValue::Int(0)) => true,
            (AstLiteral::Int(int1), CheshireValue::Int(int2)) =>
                int1.parse::<i64>().unwrap() == *int2,
            (AstLiteral::Float(float1), CheshireValue::Float(float2)) =>
                float1.parse::<f64>().unwrap() == *float2,
            (AstLiteral::Char(chr1), CheshireValue::Int(chr2)) => (*chr1 as i64) == *chr2,
            (AstLiteral::String(str1), CheshireValue::String(str2)) => str1 == str2,
            _ => false,
        }
    }

    #[cfg_attr(build = "debug", inline(never))]
    fn breakpoint(&self) {}
}
