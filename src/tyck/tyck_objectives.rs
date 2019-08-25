use crate::analyze::represent::AnalyzedFile;
use crate::parser::ast::*;
use crate::parser::ast_visitor::Adapter;
use crate::tyck::tyck_instantiate::Instantiate;
use crate::tyck::tyck_solver::TyckSolver;
use crate::util::result::*;
use std::collections::HashMap;
use std::rc::Rc;

pub struct TyckObjectiveAdapter {
    pub solver: TyckSolver,
    variables: HashMap<VariableId, AstType>,
    analyzed_file: Rc<AnalyzedFile>,
    return_type: Option<AstType>,
}

impl TyckObjectiveAdapter {
    pub fn new(solver: TyckSolver, analyzed_file: Rc<AnalyzedFile>) -> TyckObjectiveAdapter {
        TyckObjectiveAdapter {
            solver,
            variables: HashMap::new(),
            analyzed_file,
            return_type: None,
        }
    }
}

impl<'a> Adapter for TyckObjectiveAdapter {
    fn enter_impl(&mut self, i: AstImpl) -> PResult<AstImpl> {
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

    fn enter_function(&mut self, f: AstFunction) -> PResult<AstFunction> {
        self.solver.add_objectives(&f.restrictions)?;

        // TODO: Might lift this into a constructor...
        self.return_type = Some(f.return_type.clone());
        self.variables = f
            .variables
            .iter()
            .map(|(&k, v)| (k, v.ty.clone()))
            .collect();
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
        Ok(o)
    }

    fn enter_statement(&mut self, s: AstStatement) -> PResult<AstStatement> {
        match &s {
            AstStatement::Block { .. }
            | AstStatement::Expression { .. }
            | AstStatement::Break
            | AstStatement::Continue => {}
            AstStatement::Let { .. } => unreachable!(),
            AstStatement::If { condition, .. } => {
                self.solver.unify(&condition.ty, &AstType::Bool)?;
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
            AstExpressionData::Unimplemented => { /* Literally do nothing. */ }
            AstExpressionData::True | AstExpressionData::False => {
                self.solver.unify(&ty, &AstType::Bool)?;
            }
            AstExpressionData::Null => {
                self.solver.add_delayed_object_goal(&ty)?;
            }
            AstExpressionData::SelfRef => unreachable!(),
            AstExpressionData::String { .. } => self.solver.unify(&ty, &AstType::String)?,
            AstExpressionData::Int(..) => {
                self.solver.unify(&ty, &AstType::Int)?;
            }
            AstExpressionData::Char(..) => {
                self.solver.unify(&ty, &AstType::Char)?;
            }
            AstExpressionData::Identifier { name, variable_id } => {
                let variable_id = variable_id.as_ref().unwrap();
                self.solver.unify(&ty, &self.variables[variable_id])?;
            }
            AstExpressionData::Tuple { values } => {
                let tuple_tys = into_types(values);
                self.solver.unify(&ty, &AstType::tuple(tuple_tys))?;
            }
            AstExpressionData::Array { elements } => {
                let tuple_tys = into_types(elements);
                self.solver.unify(&ty, &AstType::tuple(tuple_tys))?;
            }

            // A regular function call
            AstExpressionData::Call {
                name,
                generics,
                args,
            } => {
                let (param_tys, return_ty, objectives) =
                    Instantiate::instantiate_fn_signature(&*self.analyzed_file, name, generics)?;
                let arg_tys = into_types(args);
                self.solver.unify_all(&param_tys, &arg_tys)?;
                self.solver.unify(&return_ty, &ty)?;
                self.solver.add_objectives(&objectives)?; // Add fn restrictions
            }
            // Call an object's member function
            AstExpressionData::ObjectCall {
                object,
                fn_name,
                generics,
                args,
            } => unreachable!(),
            // Call an object's static function
            AstExpressionData::StaticCall {
                call_type,
                fn_name,
                fn_generics,
                args,
                associated_trait,
                impl_signature,
            } => {
                let associated_trait = associated_trait.as_ref().unwrap();
                let (param_tys, return_ty, objectives) =
                    Instantiate::instantiate_trait_fn_signature(
                        &*self.analyzed_file,
                        &associated_trait.0,
                        &associated_trait.1,
                        fn_name,
                        fn_generics,
                        &AstType::infer(),
                    )?;
                let arg_tys = into_types(args);
                self.solver.unify_all(&param_tys, &arg_tys)?;
                self.solver.unify(&return_ty, &ty)?;
                self.solver.add_objective(&call_type, &associated_trait)?;
                self.solver.add_objectives(&objectives)?; // Add fn restrictions
            }
            // An array access `a[1u]`
            AstExpressionData::ArrayAccess { accessible, idx } => {
                let array_ty = &accessible.ty;
                self.solver.unify(array_ty, &AstType::array(ty.clone()))?;
            }
            // A tuple access `a:1`
            AstExpressionData::TupleAccess { accessible, idx } => {
                let tuple_ty = &accessible.ty;
                self.solver.add_delayed_tuple_access(tuple_ty, *idx, &ty)?;
            }
            // Call an object's member
            AstExpressionData::ObjectAccess { object, mem_name } => {
                let object_ty = &object.ty;
                self.solver
                    .add_delayed_object_access(object_ty, mem_name, &ty)?;
            }

            AstExpressionData::Allocate { object } => {
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

            AstExpressionData::BinOp { kind, lhs, rhs } if *kind == BinOpKind::Set => {
                let lhs_ty = &lhs.ty;
                let rhs_ty = &rhs.ty;
                self.solver.unify(lhs_ty, rhs_ty)?;
                self.solver.unify(lhs_ty, &ty)?;
            }

            AstExpressionData::BinOp { lhs, rhs, .. } => {
                let lhs_ty = &lhs.ty;
                let rhs_ty = &rhs.ty;
                self.solver.unify(lhs_ty, &AstType::Int)?;
                self.solver.unify(lhs_ty, rhs_ty)?;
                self.solver.unify(lhs_ty, &ty)?;
            }
        }

        Ok(AstExpression { data, ty, span })
    }

    fn enter_type(&mut self, t: AstType) -> PResult<AstType> {
        match &t {
            AstType::Object(name, args) => {
                self.solver.add_objective_well_formed(name, args)?;
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
}

fn into_types(exprs: &Vec<AstExpression>) -> Vec<AstType> {
    exprs.iter().map(|e| e.ty.clone()).collect()
}
