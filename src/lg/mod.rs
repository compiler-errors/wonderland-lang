use crate::{
    inst::{
        InstEnumRepresentation, InstEnumSignature, InstFunctionSignature,
        InstObjectFunctionSignature, InstObjectSignature, InstantiatedProgram,
    },
    lg::represent::{CheshireValue, LError, LResult, ShouldPopStack},
    parser::ast::{
        AstBlock, AstExpression, AstExpressionData, AstFunction, AstGlobalVariable, AstLiteral,
        AstMatchBranch, AstMatchPattern, AstMatchPatternData, AstNamedVariable, AstObject,
        AstObjectFunction, AstStatement, ModuleRef, VariableId,
    },
    util::{Expect, PResult, StackMap, ZipExact},
};
use std::{cell::RefCell, collections::HashMap};

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

                    if self.apply_pattern(pattern, &match_condition, scope)? {
                        let value_result = self.evaluate_expression(expression, scope);
                        scope.pop();
                        value = Some(value_result?);
                    } else {
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
                            continue;
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

            AstExpressionData::Instruction {
                instruction: _,
                arguments: _,
                output: _,
            } => todo!(),
        };

        Ok(value)
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
        let mut ret = Vec::new();

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
}
