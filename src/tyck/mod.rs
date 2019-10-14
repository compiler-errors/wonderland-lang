use crate::ana::represent::*;
use crate::parser::ast::*;
use crate::tyck::tyck_constraints::{Dummifier, TyckConstraintAssumptionAdapter};
pub use crate::tyck::tyck_instantiate::GenericsInstantiator;
pub use crate::tyck::tyck_objectives::TyckObjectiveAdapter;
pub use crate::tyck::tyck_solver::{TyckSolution, TyckSolver};
use crate::util::{Comment, Expect, FileRegistry, IntoError, PResult, Span, Visit, ZipExact};
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
    pub obj_ty: AstType,
    pub trait_ty: AstTraitType,
}

pub fn typecheck(analyzed_program: &AnalyzedProgram, parsed_program: &AstProgram) -> PResult<()> {
    let mut constraint_assumptions = TyckConstraintAssumptionAdapter::new(analyzed_program.clone());

    let parsed_program = parsed_program
        .clone()
        .visit(&mut Dummifier)?
        .visit(&mut constraint_assumptions)?;

    let analyzed_program = Rc::new(constraint_assumptions.analyzed_program);
    let base_solver = TyckSolver::new(analyzed_program.clone());

    for imp in constraint_assumptions.dummy_impls {
        typecheck_dummy_impl(analyzed_program.clone(), &base_solver, imp)?;
    }

    for m in parsed_program.modules {
        let module_name = FileRegistry::mod_path(m.id)?.join("::");
        typecheck_module(&analyzed_program, &base_solver, m, &module_name)
            .with_comment(format!("In module: {}", module_name))?;
    }

    Ok(())
}

pub fn typecheck_module(
    analyzed_program: &Rc<AnalyzedProgram>,
    base_solver: &TyckSolver,
    module: AstModule,
    module_name: &str,
) -> PResult<()> {
    let AstModule {
        functions,
        objects,
        traits,
        impls,
        ..
    } = module;

    for (name, fun) in functions {
        typecheck_simple(analyzed_program.clone(), &base_solver, fun).with_comment(format!(
            "In function `{}` in module `{}`",
            name, module_name
        ))?;
    }

    for (_name, obj) in objects {
        typecheck_simple(analyzed_program.clone(), &base_solver, obj)?;
    }

    for (_name, trt) in traits {
        typecheck_simple(analyzed_program.clone(), &base_solver, trt)?;
    }

    for (_, imp) in impls {
        let (imp, _) = typecheck_impl(analyzed_program.clone(), &base_solver, imp)?;

        // Try to detect contradictions..!
        for (other_id, other_impl) in &analyzed_program.analyzed_impls {
            // Ignore dummies. We don't care about them... Also, an impl always matches itself...
            if imp.impl_id == *other_id || other_impl.is_dummy {
                continue;
            }

            if typecheck_impl_collision(&base_solver, &imp, &other_impl).is_ok() {
                return PResult::error_at(
                    imp.name_span,
                    format!(
                        "This impl overlaps with another: impl {:?} for {:?}",
                        other_impl.trait_ty, other_impl.impl_ty
                    ),
                );
            }
        }

        // We want to type check these functions generically...
        for (_, fun) in imp.fns {
            let fn_generics = Dummifier::from_generics(&fun.generics)?;

            typecheck_impl_fn(
                analyzed_program.clone(),
                &base_solver,
                fun,
                &imp.impl_ty,
                &imp.trait_ty,
                &fn_generics,
            )?;
        }
    }

    Ok(())
}

fn typecheck_dummy_impl(
    program: Rc<AnalyzedProgram>,
    base_solver: &TyckSolver,
    imp: AnImplData,
) -> PResult<()> {
    let trait_data = &program.analyzed_traits[&imp.trait_ty.0];

    let mut objective_adapter = TyckObjectiveAdapter::new(base_solver.clone(), program.clone());
    objective_adapter
        .solver
        .add_objectives(&GenericsInstantiator::instantiate_restrictions(
            &program,
            &imp.impl_ty,
            &imp.trait_ty,
            &trait_data.restrictions,
        )?)?;

    // Prove well-formedness of types.
    imp.restrictions.visit(&mut objective_adapter)?;
    let impl_ty = imp.impl_ty.visit(&mut objective_adapter)?;
    let trait_ty = imp.trait_ty.visit(&mut objective_adapter)?;

    objective_adapter.solver.solve()?;

    // Foreach associated type, prove instantiated assoc-type bounds from trait hold. And type itself is well-formed.
    for (name, ty) in &imp.associated_tys {
        typecheck_associated_type(
            program.clone(),
            &base_solver,
            &impl_ty,
            &trait_ty,
            Span::none(),
            name,
            ty,
        )?;
    }

    Ok(())
}

pub fn typecheck_simple<T>(
    program: Rc<AnalyzedProgram>,
    base_solver: &TyckSolver,
    t: T,
) -> PResult<(T, TyckSolution)>
where
    T: Visit<TyckObjectiveAdapter>,
{
    let mut objective_adapter = TyckObjectiveAdapter::new(base_solver.clone(), program);

    let obj = t.visit(&mut objective_adapter)?;
    let solution = objective_adapter.solver.solve()?;

    Ok((obj, solution))
}

pub fn typecheck_associated_type(
    program: Rc<AnalyzedProgram>,
    base_solver: &TyckSolver,
    impl_ty: &AstType,
    trait_ty: &AstTraitType,
    span: Span,
    name: &str,
    assoc_ty: &AstType,
) -> PResult<()> {
    let trait_data = &program.analyzed_traits[&trait_ty.0];

    let mut objective_adapter = TyckObjectiveAdapter::new(base_solver.clone(), program.clone());
    let assoc_ty = assoc_ty.clone().visit(&mut objective_adapter)?;

    let restrictions: Vec<_> = trait_data
        .associated_tys
        .get(name)
        .is_expected(span, "associated type", &name)?
        .restrictions
        .iter()
        .map(|trt| AstTypeRestriction::new(assoc_ty.clone(), trt.clone()))
        .collect();

    objective_adapter
        .solver
        .add_objectives(&GenericsInstantiator::instantiate_restrictions(
            &program,
            &impl_ty,
            &trait_ty,
            &restrictions,
        )?)?;

    objective_adapter.solver.solve()?;

    Ok(())
}

pub fn typecheck_impl(
    program: Rc<AnalyzedProgram>,
    base_solver: &TyckSolver,
    imp: AstImpl,
) -> PResult<(AstImpl, TyckSolution)> {
    let trait_name = &imp.trait_ty.0;
    let trait_data = &program.analyzed_traits[trait_name];
    let mut objective_adapter = TyckObjectiveAdapter::new(base_solver.clone(), program.clone());

    // Prove that the impl satisfies the restrictions of the trait, and its own restrictions.
    objective_adapter
        .solver
        .add_objectives(&GenericsInstantiator::instantiate_restrictions(
            &program,
            &imp.impl_ty,
            &imp.trait_ty,
            &trait_data.restrictions,
        )?)?;

    imp.restrictions.clone().visit(&mut objective_adapter)?;

    // Prove well-formedness of types.
    imp.impl_ty.clone().visit(&mut objective_adapter)?;
    imp.trait_ty.clone().visit(&mut objective_adapter)?;

    let solution = objective_adapter.solver.solve()?;

    // Foreach associated type, prove instantiated assoc-type bounds from trait hold. And type itself is well-formed.
    for (name, ty) in &imp.associated_types {
        typecheck_associated_type(
            program.clone(),
            &base_solver,
            &imp.impl_ty,
            &imp.trait_ty,
            Span::none(),
            name,
            ty,
        )?;
    }

    // If it's not a dummy impl, functions and types should all be matched to one in the trait. (<=>).
    if imp.associated_types.len() != trait_data.associated_tys.len() {
        return PResult::error_at(imp.name_span, format!("The impl has a different number of provided associated types than its corresponding trait."));
    }

    if imp.fns.len() != trait_data.methods.len() {
        return PResult::error_at(imp.name_span, format!("The impl has a different number of implemented methods than its corresponding trait."));
    }

    Ok((imp, solution))
}

pub fn typecheck_impl_fn(
    program: Rc<AnalyzedProgram>,
    base_solver: &TyckSolver,
    fun: AstObjectFunction,
    impl_ty: &AstType,
    trait_ty: &AstTraitType,
    fn_generics: &[AstType],
) -> PResult<(AstObjectFunction, TyckSolution)> {
    let mut objective_adapter = TyckObjectiveAdapter::new(base_solver.clone(), program.clone());

    let fun = fun.visit(&mut objective_adapter)?;
    let mut solver = objective_adapter.solver;

    let (expected_params, expected_ret_ty, expected_constraints) =
        GenericsInstantiator::instantiate_trait_fn_signature(
            &program,
            &trait_ty.0,
            &trait_ty.1,
            &fun.name,
            fn_generics,
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
        return PResult::error_at(fun.name_span, format!("Restrictions on this method do not match the restrictions on the trait method. Expected {:?}, got {:?}.", norm_expected_constraints, given_constraints));
    }

    Ok((fun, solution))
}

/// Try to typecheck one impl as another. Useful to detect contradictions where one impl can satisfy another.
fn typecheck_impl_collision(
    base_solver: &TyckSolver,
    imp: &AstImpl,
    other_impl: &AnImplData,
) -> PResult<()> {
    let mut solver = base_solver.clone();

    let fresh_generics: Vec<_> = other_impl
        .generics
        .iter()
        .map(|_| AstType::infer())
        .collect();
    let mut instantiate =
        GenericsInstantiator::from_generics(&other_impl.generics, &fresh_generics)?;

    let other_impl_ty = other_impl.impl_ty.clone().visit(&mut instantiate)?;
    let other_trait_ty = other_impl.trait_ty.clone().visit(&mut instantiate)?;

    solver.unify(&imp.impl_ty, &other_impl_ty)?;
    solver.unify_traits(&imp.trait_ty, &other_trait_ty)?;
    solver.add_objectives(&other_impl.restrictions.clone().visit(&mut instantiate)?)?;

    solver.solve()?;
    Ok(())
}
