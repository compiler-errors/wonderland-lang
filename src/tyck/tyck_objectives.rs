use crate::ana::represent::AnalyzedProgram;
use crate::parser::ast::*;
use crate::parser::ast_visitor::AstAdapter;
use crate::tyck::tyck_instantiate::GenericsInstantiator;
use crate::tyck::tyck_solver::TyckSolver;
use crate::util::{PResult, ZipExact};
use std::collections::HashMap;
use std::rc::Rc;

pub struct TyckObjectiveAdapter {
    pub solver: TyckSolver,
    variables: HashMap<VariableId, AstType>,
    analyzed_program: Rc<AnalyzedProgram>,
    return_type: Option<AstType>,
}

impl TyckObjectiveAdapter {
    pub fn new(solver: TyckSolver, analyzed_program: Rc<AnalyzedProgram>) -> TyckObjectiveAdapter {
        TyckObjectiveAdapter {
            solver,
            variables: HashMap::new(),
            analyzed_program,
            return_type: None,
        }
    }

    fn unify_pattern_type(&mut self, pattern: &AstMatchPattern, other_ty: &AstType) -> PResult<()> {
        self.solver.unify(&pattern.ty, other_ty)?;

        match &pattern.data {
            AstMatchPatternData::Underscore => {}
            AstMatchPatternData::Identifier(v) => self.solver.unify(&v.ty, other_ty)?,
            AstMatchPatternData::Tuple(children) => {
                let mut children_tys = Vec::new();

                for child in children {
                    let child_ty = AstType::infer();
                    self.unify_pattern_type(child, &child_ty)?;
                    children_tys.push(child_ty);
                }

                self.solver.unify(&AstType::tuple(children_tys), other_ty)?;
            }
            AstMatchPatternData::Literal(lit) => match lit {
                AstLiteral::True | AstLiteral::False => {
                    self.solver.unify(&AstType::Bool, other_ty)?
                }
                AstLiteral::Null => self.solver.add_delayed_nullable_goal(other_ty)?,
                AstLiteral::Int(..) => self.solver.unify(&AstType::Int, other_ty)?,
                AstLiteral::Char(..) => self.solver.unify(&AstType::Char, other_ty)?,
                AstLiteral::String { .. } => self.solver.unify(&AstType::String, other_ty)?,
            },
            AstMatchPatternData::PositionalEnum {
                enumerable,
                generics,
                variant,
                children,
                ..
            } => {
                let expected_tys = GenericsInstantiator::instantiate_enum_pattern(
                    &self.analyzed_program,
                    enumerable,
                    &generics,
                    &variant,
                )?;

                for (child, ty) in
                    ZipExact::zip_exact(children, expected_tys, "positional elements")?
                {
                    self.unify_pattern_type(child, &ty)?;
                }

                self.solver.unify(&other_ty, &AstType::enumerable(enumerable.clone(), generics.clone()))?;
            }
            AstMatchPatternData::PlainEnum { .. } | AstMatchPatternData::NamedEnum { .. } => {
                unreachable!()
            }
        }

        Ok(())
    }
}

impl<'a> AstAdapter for TyckObjectiveAdapter {
    fn enter_impl(&mut self, _i: AstImpl) -> PResult<AstImpl> {
        unreachable!()
    }

    fn enter_trait(&mut self, t: AstTrait) -> PResult<AstTrait> {
        self.solver.add_objectives(&t.restrictions)?;

        Ok(t)
    }

    fn enter_object(&mut self, o: AstObject) -> PResult<AstObject> {
        self.solver.add_objectives(&o.restrictions)?;

        // Nothing special here to do.
        Ok(o)
    }

    fn enter_enum(&mut self, e: AstEnum) -> PResult<AstEnum> {
        self.solver.add_objectives(&e.restrictions)?;

        Ok(e)
    }

    fn enter_function(&mut self, f: AstFunction) -> PResult<AstFunction> {
        self.solver.add_objectives(&f.restrictions)?;

        // TODO: Might lift this into a constructor...
        self.return_type = Some(f.return_type.clone());
        self.variables = f
            .variables
            .iter()
            .map(|(&k, v)| (k, v.ty.clone()))
            .collect();

        if let Some(block) = &f.definition {
            self.solver.unify(&f.return_type, &block.expression.ty)?;
        }

        Ok(f)
    }

    fn enter_object_function(&mut self, o: AstObjectFunction) -> PResult<AstObjectFunction> {
        self.solver.add_objectives(&o.restrictions)?;

        self.return_type = Some(o.return_type.clone());
        self.variables = o
            .variables
            .iter()
            .map(|(&k, v)| (k, v.ty.clone()))
            .collect();

        if let Some(block) = &o.definition {
            self.solver.unify(&o.return_type, &block.expression.ty)?;
        }

        Ok(o)
    }

    fn enter_statement(&mut self, s: AstStatement) -> PResult<AstStatement> {
        match &s {
            // Removed in earlier stages
            AstStatement::For { .. } => unreachable!(),

            AstStatement::Expression { .. } | AstStatement::Break | AstStatement::Continue => {}
            AstStatement::Let { pattern, value } => {
                self.unify_pattern_type(pattern, &value.ty)?;
            }
            AstStatement::While { condition, .. } => {
                self.solver.unify(&condition.ty, &AstType::Bool)?;
            }
            AstStatement::Return { value } => {
                self.solver
                    .unify(&value.ty, self.return_type.as_ref().unwrap())?;
            }
            AstStatement::Assert { condition } => {
                self.solver.unify(&condition.ty, &AstType::Bool)?;
            }
        }

        Ok(s)
    }

    fn enter_expression(&mut self, e: AstExpression) -> PResult<AstExpression> {
        let AstExpression { data, ty, span } = e;

        match &data {
            AstExpressionData::ExprCall { .. } => unreachable!(),

            AstExpressionData::Unimplemented => {}
            AstExpressionData::Block { block } => {
                self.solver.unify(&block.expression.ty, &ty)?;
            }
            AstExpressionData::If {
                condition,
                block,
                else_block,
            } => {
                self.solver.unify(&condition.ty, &AstType::Bool)?;
                self.solver.unify(&ty, &block.expression.ty)?;
                self.solver
                    .unify(&block.expression.ty, &else_block.expression.ty)?;
            }
            AstExpressionData::Match {
                expression,
                branches,
            } => {
                let match_expr_ty = &expression.ty;

                for AstMatchBranch {
                    pattern,
                    expression,
                } in branches
                {
                    self.unify_pattern_type(pattern, match_expr_ty)?;
                    self.solver.unify(&expression.ty, &ty)?;
                }
            }
            AstExpressionData::SelfRef => unreachable!(),
            AstExpressionData::Literal(lit) => match lit {
                AstLiteral::True | AstLiteral::False => {
                    self.solver.unify(&ty, &AstType::Bool)?;
                }
                AstLiteral::Null => {
                    self.solver.add_delayed_nullable_goal(&ty)?;
                }
                AstLiteral::String { .. } => self.solver.unify(&ty, &AstType::String)?,
                AstLiteral::Int(..) => {
                    self.solver.unify(&ty, &AstType::Int)?;
                }
                AstLiteral::Char(..) => {
                    self.solver.unify(&ty, &AstType::Char)?;
                }
            },
            AstExpressionData::Identifier { variable_id, .. } => {
                let variable_id = variable_id.as_ref().unwrap();
                self.solver.unify(&ty, &self.variables[variable_id])?;
            }
            AstExpressionData::GlobalVariable { name } => {
                self.solver
                    .unify(&ty, &self.analyzed_program.analyzed_globals[name])?;
            }
            AstExpressionData::Tuple { values } => {
                let tuple_tys = into_types(values);
                self.solver.unify(&ty, &AstType::tuple(tuple_tys))?;
            }
            AstExpressionData::ArrayLiteral { elements } => {
                let _tuple_tys = into_types(elements);
                let elem_ty = AstType::infer();

                for elem in elements {
                    self.solver.unify(&elem.ty, &elem_ty)?;
                }

                self.solver.unify(&AstType::array(elem_ty), &ty)?;
            }
            AstExpressionData::AllocateArray { object, size } => {
                self.solver.unify(&AstType::array(object.clone()), &ty)?;
                self.solver.unify(&AstType::Int, &size.ty)?;
            }

            // A regular function call
            AstExpressionData::FnCall {
                fn_name,
                generics,
                args,
            } => {
                let (param_tys, return_ty, objectives) =
                    GenericsInstantiator::instantiate_fn_signature(
                        &self.analyzed_program,
                        fn_name,
                        generics,
                    )?;
                let arg_tys = into_types(args);
                self.solver.unify_all(&param_tys, &arg_tys)?;
                self.solver.unify(&return_ty, &ty)?;
                self.solver.add_objectives(&objectives)?; // Add fn restrictions
            }
            // Call an object's member function
            AstExpressionData::ObjectCall { .. } => unreachable!(),
            // Call an object's static function
            AstExpressionData::StaticCall {
                call_type,
                fn_name,
                fn_generics,
                args,
                associated_trait,
                ..
            } => {
                let associated_trait = associated_trait.as_ref().unwrap();
                let (param_tys, return_ty, objectives) =
                    GenericsInstantiator::instantiate_trait_fn_signature(
                        &*self.analyzed_program,
                        &associated_trait.0,
                        &associated_trait.1,
                        fn_name,
                        fn_generics,
                        &call_type,
                    )?;
                let arg_tys = into_types(args);
                self.solver.unify_all(&param_tys, &arg_tys)?;
                self.solver.unify(&return_ty, &ty)?;
                self.solver.add_objective(&call_type, &associated_trait)?;
                self.solver.add_objectives(&objectives)?; // Add fn restrictions
            }
            // An array access `a[1u]`
            AstExpressionData::ArrayAccess { .. } => unreachable!(),
            // A tuple access `a:1`
            AstExpressionData::TupleAccess { accessible, idx } => {
                let tuple_ty = &accessible.ty;
                self.solver.add_delayed_tuple_access(tuple_ty, *idx, &ty)?;
            }
            // Call an object's member
            AstExpressionData::ObjectAccess {
                object, mem_name, ..
            } => {
                let object_ty = &object.ty;
                self.solver
                    .add_delayed_object_access(object_ty, mem_name, &ty)?;
            }

            AstExpressionData::AllocateObject { object } => {
                self.solver.unify(object, &ty)?;
            }

            AstExpressionData::Not(subexpression) => {
                let sub_ty = &subexpression.ty;
                self.solver.unify(sub_ty, &AstType::Bool)?;
                self.solver.unify(&ty, &AstType::Bool)?;
            }
            AstExpressionData::Negate(subexpression) => {
                let sub_ty = &subexpression.ty;
                self.solver.unify(sub_ty, &AstType::Int)?;
                self.solver.unify(&ty, &AstType::Int)?;
            }

            AstExpressionData::Assign { lhs, rhs } => {
                let lhs_ty = &lhs.ty;
                let rhs_ty = &rhs.ty;
                self.solver.unify(lhs_ty, rhs_ty)?;
                self.solver.unify(lhs_ty, &ty)?;
            }

            AstExpressionData::GlobalFn { name } => {
                let fn_data = &self.analyzed_program.analyzed_functions[name];
                self.solver.unify(
                    &ty,
                    &AstType::fn_ptr_type(fn_data.parameters.clone(), fn_data.return_type.clone()),
                )?;
            }

            AstExpressionData::Closure {
                params,
                expr,
                variables,
                ..
            } => {
                self.variables.extend(
                    variables
                        .as_ref()
                        .unwrap()
                        .iter()
                        .map(|(&k, v)| (k, v.ty.clone())),
                );

                let ret_ty = expr.ty.clone();
                let param_tys = params.iter().map(|p| p.ty.clone()).collect();
                self.solver
                    .unify(&ty, &AstType::closure_type(param_tys, ret_ty))?;
            }

            AstExpressionData::PositionalEnum {
                enumerable,
                generics,
                variant,
                children,
            } => {
                self.solver.unify(
                    &ty,
                    &AstType::enumerable(enumerable.clone(), generics.clone()),
                )?;

                let expected_tys = GenericsInstantiator::instantiate_enum_pattern(
                    &self.analyzed_program,
                    enumerable,
                    &generics,
                    &variant,
                )?;

                for (child, ty) in
                    ZipExact::zip_exact(children, expected_tys, "positional elements")?
                {
                    self.solver.unify(&child.ty, &ty)?;
                }
            }

            AstExpressionData::NamedEnum { .. }
            | AstExpressionData::PlainEnum { .. }
            | AstExpressionData::BinOp { .. } => unreachable!(),
        }

        Ok(AstExpression { data, ty, span })
    }

    fn enter_type(&mut self, t: AstType) -> PResult<AstType> {
        match &t {
            AstType::Object(name, args) => {
                self.solver.add_objective_object_well_formed(name, args)?;
            }
            AstType::Enum(name, args) => {
                self.solver.add_objective_enum_well_formed(name, args)?;
            }
            AstType::AssociatedType {
                obj_ty, trait_ty, ..
            } => {
                self.solver
                    .add_objective(obj_ty.as_ref(), trait_ty.as_ref().unwrap())?;
            }
            _ => {}
        }

        // Add this type, so we can keep an eye on it. Should eventually be elaborated.
        self.solver.add_type(&t)?;

        Ok(t)
    }

    fn enter_global_variable(&mut self, g: AstGlobalVariable) -> PResult<AstGlobalVariable> {
        self.solver.unify(&g.ty, &g.init.ty)?;

        Ok(g)
    }

    fn enter_pattern(&mut self, p: AstMatchPattern) -> PResult<AstMatchPattern> {
        self.unify_pattern_type(&p, &AstType::infer())?;

        Ok(p)
    }
}

fn into_types(exprs: &[AstExpression]) -> Vec<AstType> {
    exprs.iter().map(|e| e.ty.clone()).collect()
}
