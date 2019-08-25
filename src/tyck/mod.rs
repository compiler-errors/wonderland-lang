use crate::analyze::represent::*;
use crate::parser::ast::*;
use crate::parser::ast_visitor::Visit;
use crate::tyck::tyck_constraints::{Genericizer, TyckConstraintAssumptionAdapter};
use crate::tyck::tyck_instantiate::Instantiate;
use crate::tyck::tyck_objectives::TyckObjectiveAdapter;
use crate::tyck::tyck_solver::{TyckSolution, TyckSolver};
use crate::util::result::*;
use crate::util::{ZipExact, Span};
use std::collections::HashSet;
use std::rc::Rc;

mod tyck_constraints;
mod tyck_instantiate;
mod tyck_objectives;
mod tyck_solver;

/** An objective. Essentially captures the question:
 * Does obj_ty implement trait_ty (with given generics). */
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct TyckObjective {
    obj_ty: AstType,
    trait_ty: AstTraitType,
}

#[derive(Debug, Clone)]
pub struct TyckImplSignature {
    impl_id: ImplId,
    generics: Vec<AstType>,
}

pub fn typecheck(parsed_file: &ParsedFile, analyzed_file: AnalyzedFile) -> PResult<AnalyzedFile> {
    let mut constraint_assumptions = TyckConstraintAssumptionAdapter::new(analyzed_file.clone());
    let ParsedFile {
        functions,
        objects,
        traits,
        impls,
    } = parsed_file
        .clone()
        .visit(&mut Genericizer)?
        .visit(&mut constraint_assumptions)?;

    let file = Rc::new(constraint_assumptions.file);
    let base_solver = TyckSolver::new(file.clone());

    for imp in constraint_assumptions.dummy_impls {
        typecheck_dummy_impl(file.clone(), &base_solver, imp)?;
    }

    for fun in functions {
        typecheck_simple(file.clone(), &base_solver, fun)?;
    }

    for obj in objects {
        typecheck_simple(file.clone(), &base_solver, obj)?;
    }

    for tr in traits {
        typecheck_simple(file.clone(), &base_solver, tr)?;
    }

    for imp in impls {
        let imp = typecheck_impl(file.clone(), &base_solver, imp)?;

        // We want to type check these functions generically...
        for fun in imp.fns {
            let fn_generics = Genericizer::from_generics(&fun.generics)?;

            typecheck_impl_fn(file.clone(), &base_solver, fun, &imp.impl_ty, &imp.trait_ty, fn_generics)?;
        }
    }

    Ok(analyzed_file)
}

fn typecheck_dummy_impl(
    file: Rc<AnalyzedFile>,
    base_solver: &TyckSolver,
    imp: AnImplData,
) -> PResult<()> {
    let trait_data = &file.analyzed_traits[&imp.trait_ty.0];

    let mut objective_adapter = TyckObjectiveAdapter::new(base_solver.clone(), file.clone());
    objective_adapter.solver.add_objectives(&Instantiate::instantiate_restrictions(
        &file,
        &imp.impl_ty,
        &imp.trait_ty,
        &trait_data.restrictions,
    )?)?;

    // Prove well-formedness of types.
    imp.restrictions.visit(&mut objective_adapter)?;
    let impl_ty = imp.impl_ty.visit(&mut objective_adapter)?;
    let trait_ty = imp.trait_ty.visit(&mut objective_adapter)?;

    // Foreach associated type, prove instantiated assoc-type bounds from trait hold. And type itself is well-formed.
    for (name, ty) in &imp.associated_tys {
        typecheck_associated_type(file.clone(), &base_solver, &impl_ty, &trait_ty, Span::new(0, 0), name, ty)?;
    }

    Ok(())
}

pub fn typecheck_simple<T>(
    file: Rc<AnalyzedFile>,
    base_solver: &TyckSolver,
    t: T,
) -> PResult<(T, TyckSolution)>
where
    T: Visit<TyckObjectiveAdapter>,
{
    let mut objective_adapter = TyckObjectiveAdapter::new(base_solver.clone(), file);

    let obj = t.visit(&mut objective_adapter)?;
    let solution = objective_adapter.solver.solve()?;

    Ok((obj, solution))
}

fn typecheck_associated_type(
    file: Rc<AnalyzedFile>,
    base_solver: &TyckSolver,
    impl_ty: &AstType,
    trait_ty: &AstTraitType,
    span: Span,
    name: &String,
    assoc_ty: &AstType) -> PResult<()> {
    let trait_data = &file.analyzed_traits[&trait_ty.0];

    let mut objective_adapter =
        TyckObjectiveAdapter::new(base_solver.clone(), file.clone());
    let assoc_ty = assoc_ty.clone().visit(&mut objective_adapter)?;

    let restrictions = trait_data.associated_tys.get(name).expected(
            span,
            "associated type",
            &name,
        )?
        .restrictions
        .iter()
        .map(|trt| AstTypeRestriction::new(assoc_ty.clone(), trt.clone()))
        .collect();

    objective_adapter.solver.add_objectives(&Instantiate::instantiate_restrictions(
        &file,
        &impl_ty,
        &trait_ty,
        &restrictions,
    )?)?;

    objective_adapter.solver.solve()?;

    Ok(())
}

fn typecheck_impl(
    file: Rc<AnalyzedFile>,
    base_solver: &TyckSolver,
    imp: AstImpl) -> PResult<AstImpl> {
    let trait_name = &imp.trait_ty.0;
    let trait_data = file.analyzed_traits.get(trait_name).expected(
        imp.name_span,
        "trait",
        trait_name,
    )?;
    let trait_generics = imp.trait_ty.1.clone();

    let impl_data = file.analyzed_impls.get(&imp.impl_id).unwrap();

    let mut objective_adapter = TyckObjectiveAdapter::new(base_solver.clone(), file.clone());
    objective_adapter.solver.add_objectives(&Instantiate::instantiate_restrictions(
        &file,
        &imp.impl_ty,
        &imp.trait_ty,
        &trait_data.restrictions,
    )?)?;

    imp.restrictions.clone().visit(&mut objective_adapter)?;

    // Prove that the impl satisfies the restrictions of the trait, and its own restrictions.
    objective_adapter.solver.solve()?;

    // Prove well-formedness of types.
    let mut objective_adapter = TyckObjectiveAdapter::new(base_solver.clone(), file.clone());

    imp.impl_ty.clone().visit(&mut objective_adapter)?;
    imp.trait_ty.clone().visit(&mut objective_adapter)?;

    objective_adapter.solver.solve()?;

    // Foreach associated type, prove instantiated assoc-type bounds from trait hold. And type itself is well-formed.
    for (name, ty) in &imp.associated_types {
        typecheck_associated_type(file.clone(), &base_solver, &imp.impl_ty, &imp.trait_ty, Span::new(0, 0), name, ty)?;
    }

    // If it's not a dummy impl, functions and types should all be matched to one in the trait. (<=>).
    if imp.associated_types.len() != trait_data.associated_tys.len() {
        return PError::new(imp.name_span, format!("The impl has a different number of provided associated types than its corresponding trait."));
    }

    if imp.fns.len() != trait_data.methods.len() {
        return PError::new(imp.name_span, format!("The impl has a different number of implemented methods than its corresponding trait."));
    }

    Ok(imp)
}

fn typecheck_impl_fn(
    file: Rc<AnalyzedFile>,
    base_solver: &TyckSolver,
    fun: AstObjectFunction,
    impl_ty: &AstType,
    trait_ty: &AstTraitType,
    fn_generics: Vec<AstType>) -> PResult<AstObjectFunction> {
    let fn_name = &fun.name;

    let mut objective_adapter =
        TyckObjectiveAdapter::new(base_solver.clone(), file.clone());

    let fun = fun.visit(&mut objective_adapter)?;
    let mut solver = objective_adapter.solver;

    let (expected_params, expected_ret_ty, expected_constraints) =
        Instantiate::instantiate_trait_fn_signature(
            &file,
            &trait_ty.0,
            &trait_ty.1,
            &fun.name,
            &fn_generics,
            &impl_ty,
        )?;

    solver.add_objectives(&expected_constraints)?;

    for (given_param, expected_ty) in
        ZipExact::zip_exact(&fun.parameter_list, &expected_params, "parameter")?
        {
            solver.unify(expected_ty, &given_param.ty)?;
        }

    solver.unify(&expected_ret_ty, &fun.return_type)?;

    let solution = solver.solve()?;

    // Now let's do some consistency checks.

    let mut norm_expected_constraints = HashSet::new();
    for AstTypeRestriction { ty, trt } in expected_constraints {
        let ty = solution.normalize_ty(&ty)?;
        let trt = solution.normalize_trait_ty(&trt)?;

        norm_expected_constraints.insert(AstTypeRestriction::new(ty, trt));
    }

    let mut given_constraints = HashSet::new();
    for AstTypeRestriction { ty, trt } in &fun.restrictions {
        let ty = solution.normalize_ty(ty)?;
        let trt = solution.normalize_trait_ty(trt)?;

        given_constraints.insert(AstTypeRestriction::new(ty, trt));
    }

    if norm_expected_constraints != given_constraints {
        return PError::new(fun.name_span, format!("Restrictions on this method do not match the restrictions on the trait method. Expected {:?}, got {:?}.", norm_expected_constraints, given_constraints));
    }

    Ok(fun)
}
