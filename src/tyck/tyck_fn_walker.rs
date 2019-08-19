use crate::analyze::represent::AnalyzedFile;
use crate::parser::ast::*;
use crate::parser::ast_visitor::Adapter;
use crate::tyck::tyck_instantiate::InstantiateAdapter;
use crate::tyck::tyck_solver::TyckSolver;
use crate::util::result::*;
use crate::util::ZipExact;
use std::collections::HashMap;
use std::env::var;
use std::rc::Rc;

struct TyckPreSolveFnAnalyzer {
    solver: TyckSolver,
    variables: HashMap<VariableId, AstType>,
    analyzed_file: Rc<AnalyzedFile>,
    return_type: Option<AstType>,
}

impl Adapter for TyckPreSolveFnAnalyzer {
    fn enter_trait(&mut self, t: AstTrait) -> PResult<AstTrait> {
        unreachable!()
    }

    fn enter_impl(&mut self, i: AstImpl) -> PResult<AstImpl> {
        unreachable!()
    }

    fn enter_function(&mut self, o: AstFunction) -> PResult<AstFunction> {
        // TODO: Might lift this into a constructor...
        self.return_type = Some(o.return_type.clone());
        self.variables = o
            .variables
            .iter()
            .map(|(&k, v)| (k, v.ty.clone()))
            .collect();
        Ok(o)
    }

    fn enter_object_function(&mut self, o: AstObjectFunction) -> PResult<AstObjectFunction> {
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
                self.solver
                    .unify(condition.ty.as_ref().unwrap(), &AstType::Bool)?;
            }
            AstStatement::While { condition, .. } => {
                self.solver
                    .unify(condition.ty.as_ref().unwrap(), &AstType::Bool)?;
            }
            AstStatement::Return { value } => {
                self.solver.unify(
                    value.ty.as_ref().unwrap(),
                    self.return_type.as_ref().unwrap(),
                )?;
            }
            AstStatement::Assert { condition } => {
                self.solver
                    .unify(condition.ty.as_ref().unwrap(), &AstType::Bool)?;
            }
        }

        Ok(s)
    }

    fn enter_expression(&mut self, e: AstExpression) -> PResult<AstExpression> {
        let AstExpression { data, ty, span } = e;

        // Always, b/c AnalyzeInfer.
        let ty = ty.unwrap();

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

            /// A regular function call
            AstExpressionData::Call {
                name,
                generics,
                args,
            } => {
                let (param_tys, return_ty) = InstantiateAdapter::instantiate_fn_signature(
                    &*self.analyzed_file,
                    name,
                    generics,
                )?;
                let arg_tys = into_types(args);
                self.solver.unify_all(&param_tys, &arg_tys)?;
                self.solver.unify(&return_ty, &ty)?;
            }
            /// Call an object's member function
            AstExpressionData::ObjectCall {
                object,
                fn_name,
                generics,
                args,
            } => unreachable!(),
            /// Call an object's static function
            AstExpressionData::StaticCall {
                call_type,
                fn_name,
                fn_generics,
                args,
                associated_trait,
            } => {
                let associated_trait = associated_trait.as_ref().unwrap();
                let (param_tys, return_ty) = InstantiateAdapter::instantiate_object_fn_signature(
                    &*self.analyzed_file,
                    associated_trait,
                    fn_name,
                    fn_generics,
                )?;
                let arg_tys = into_types(args);
                self.solver.unify_all(&param_tys, &arg_tys)?;
                self.solver.unify(&return_ty, &ty)?;
                self.solver.add_objective(call_type, associated_trait)?;
            }
            /// An array access `a[1u]`
            AstExpressionData::Access { accessible, idx } => {
                let array_ty = accessible.ty.as_ref().unwrap();
                self.solver.unify(array_ty, &AstType::array(ty.clone()))?;
            }
            /// A tuple access `a:1`
            AstExpressionData::TupleAccess { accessible, idx } => {
                let tuple_ty = accessible.ty.as_ref().unwrap();
                self.solver.add_delayed_tuple_access(tuple_ty, *idx, &ty)?;
            }
            /// Call an object's member
            AstExpressionData::ObjectAccess { object, mem_name } => {
                let object_ty = object.ty.as_ref().unwrap();
                self.solver
                    .add_delayed_object_access(object_ty, mem_name, &ty)?;
            }

            AstExpressionData::Allocate { object } => {
                self.solver.unify(object, &ty)?;
            }

            AstExpressionData::Not(subexpression) => {
                let sub_ty = subexpression.ty.as_ref().unwrap();
                self.solver.unify(sub_ty, &AstType::Bool)?;
                self.solver.unify(&ty, &AstType::Bool)?;
            }
            AstExpressionData::Negate(subexpression) => {
                let sub_ty = subexpression.ty.as_ref().unwrap();
                self.solver.unify(sub_ty, &AstType::Int)?;
                self.solver.unify(&ty, &AstType::Int)?;
            }

            AstExpressionData::BinOp { kind, lhs, rhs } if *kind == BinOpKind::Set => {
                let lhs_ty = lhs.ty.as_ref().unwrap();
                let rhs_ty = rhs.ty.as_ref().unwrap();
                self.solver.unify(lhs_ty, rhs_ty)?;
                self.solver.unify(lhs_ty, &ty)?;
            }

            AstExpressionData::BinOp { lhs, rhs, .. } => {
                let lhs_ty = lhs.ty.as_ref().unwrap();
                let rhs_ty = rhs.ty.as_ref().unwrap();
                self.solver.unify(lhs_ty, &AstType::Int)?;
                self.solver.unify(lhs_ty, rhs_ty)?;
                self.solver.unify(lhs_ty, &ty)?;
            }
        }

        Ok(AstExpression {
            data,
            ty: Some(ty),
            span,
        })
    }
}

fn into_types(exprs: &Vec<AstExpression>) -> Vec<AstType> {
    exprs.iter().map(|e| e.ty.clone().unwrap()).collect()
}

struct TyckPostSolveFnAnalyzer(TyckSolver);
