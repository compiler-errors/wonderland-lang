pub use crate::tyck::{
    tyck_instantiation::GenericsAdapter,
    tyck_represent::*,
    tyck_solver::{TyckObjective, TyckSolver, TypeAmbiguityAdapter},
};
use crate::{
    ana::represent::{AnImplData, AnalyzedProgram},
    parser::ast::*,
    tyck::tyck_constraints::{Dummifier, TyckConstraintAssumptionAdapter},
    util::{Context, FileRegistry, PResult, Visit},
};
use std::{collections::HashMap, fmt::Debug, rc::Rc};

mod tyck_constraints;
mod tyck_instantiation;
mod tyck_represent;
mod tyck_solver;

const TYCK_MAX_DEPTH: usize = 64;

pub fn typecheck(analyzed_program: &AnalyzedProgram, parsed_program: &AstProgram) -> PResult<()> {
    let mut constraint_assumptions = TyckConstraintAssumptionAdapter::new(analyzed_program.clone());

    let parsed_program = parsed_program
        .clone()
        .visit(&mut Dummifier)?
        .visit(&mut constraint_assumptions)?;

    let analyzed_program = Rc::new(constraint_assumptions.analyzed_program);
    let base_solver = TyckSolver::new(analyzed_program.clone());

    for imp in constraint_assumptions.dummy_impls {
        typecheck_simple(&base_solver, imp).with_comment(|| format!("Dummy impl"))?;
    }

    for m in parsed_program.modules {
        let module_name = FileRegistry::mod_path(m.id).join("::");
        typecheck_module(&analyzed_program, &base_solver, m, &module_name)?;
    }

    Ok(())
}

pub fn typecheck_module(
    program: &AnalyzedProgram,
    base_solver: &TyckSolver,
    module: AstModule,
    module_name: &str,
) -> PResult<()> {
    let AstModule {
        functions,
        objects,
        enums,
        traits,
        impls,
        globals,
        ..
    } = module;

    for (name, fun) in functions {
        typecheck_simple(&base_solver, fun)
            .with_comment(|| format!("In function `{}` in module `{}`", name, module_name))?;
    }

    for (name, obj) in objects {
        typecheck_simple(&base_solver, obj)
            .with_comment(|| format!("In object `{}` in module `{}`", name, module_name))?;
    }

    for (name, en) in enums {
        typecheck_simple(&base_solver, en)
            .with_comment(|| format!("In enum `{}` in module `{}`", name, module_name))?;
    }

    for (name, trt) in traits {
        typecheck_simple(&base_solver, trt)
            .with_comment(|| format!("In trait `{}` in module `{}`", name, module_name))?;
    }

    for (name, global) in globals {
        typecheck_simple(&base_solver, global)
            .with_comment(|| format!("In global `{}` in module `{}`", name, module_name))?;
    }

    for (_, imp) in impls {
        let imp = typecheck_impl(&base_solver, imp)?;

        // Try to detect contradictions..!
        for (other_id, other_impl) in &program.analyzed_impls {
            // Ignore dummies. We don't care about them... Also, an impl always matches
            // itself...
            if imp.impl_id == *other_id || other_impl.is_dummy {
                continue;
            }

            typecheck_impl_collision(&base_solver, &imp, &other_impl)?;
        }
    }

    Ok(())
}

pub fn typecheck_simple<T>(base_solver: &TyckSolver, t: T) -> PResult<T>
where
    T: for<'a> Visit<TypeAmbiguityAdapter<'a>> + Visit<TyckSolver> + Clone + Eq + Debug,
{
    let mut solver = base_solver.clone();
    solver.typecheck_loop(t)
}

pub fn typecheck_impl(base_solver: &TyckSolver, mut imp: AstImpl) -> PResult<AstImpl> {
    // Yank these out; we'll tyck them ourselves a bit later on.
    let fns = std::mem::take(&mut imp.fns);

    let mut imp = typecheck_simple(&base_solver, imp)?;

    let mut tycked_fns = HashMap::new();
    for (name, fun) in fns {
        let fn_generics = Dummifier::from_generics(&fun.generics)?;

        let fun = typecheck_impl_fn(
            &base_solver,
            fun,
            &imp.impl_ty,
            imp.trait_ty.as_ref(),
            &fn_generics,
        )
        .with_comment(|| format!("In method `{}`", name,))?;

        tycked_fns.insert(name, fun);
    }

    imp.fns = tycked_fns;
    Ok(imp)
}

pub fn typecheck_impl_fn(
    base_solver: &TyckSolver,
    fun: AstObjectFunction,
    impl_ty: &AstType,
    trait_ty: Option<&AstTraitType>,
    fn_generics: &[AstType],
) -> PResult<AstObjectFunction> {
    debug!("{:?} => {:?}", &fun.generics, fn_generics);
    let mut a = GenericsAdapter::new(&fun.generics, fn_generics);
    let fun = fun.visit(&mut a)?;

    let t = TyckInstantiatedObjectFunction {
        fun,
        impl_ty: impl_ty.clone(),
        trait_ty: trait_ty.cloned(),
        fn_generics: fn_generics.to_vec(),
    };
    let mut solver = base_solver.clone();
    Ok(solver.typecheck_loop(t)?.fun)
}

/// Try to typecheck one impl as another. Useful to detect contradictions where
/// one impl can satisfy another.
fn typecheck_impl_collision(
    _base_solver: &TyckSolver,
    _imp: &AstImpl,
    _other_impl: &AnImplData,
) -> PResult<()> {
    // TODO
    Ok(())

    /*
    return PResult::error_at(
                    imp.name_span,
                    format!(
                        "This `impl {} for {}` conflicts with `impl {} for {}`",
                        imp.trait_ty, imp.impl_ty, other_impl.trait_ty, other_impl.impl_ty,
                    ),
                );



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
    */
}
