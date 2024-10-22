pub use crate::tyck::{
    tyck_instantiation::GenericsAdapter,
    tyck_represent::*,
    tyck_solver::{TyckObjective, TyckSolver, TypeAmbiguityAdapter},
};
use crate::{
    ana::represent::AnalyzedProgram,
    ast::*,
    tyck::tyck_constraints::{Dummifier, TyckGenericConstraintAssumptionAdapter},
    util::{Context, FileRegistry, PResult, Visit},
};
use std::{fmt::Debug, rc::Rc};
pub use tyck_constraints::TyckDynamicAssumptionAdapter;

mod tyck_constraints;
pub mod tyck_instantiation;
mod tyck_represent;
mod tyck_solver;

const TYCK_MAX_DEPTH: usize = 64;

pub fn typecheck(analyzed_program: &AnalyzedProgram, parsed_program: &AstProgram) -> PResult<()> {
    let parsed_program = parsed_program.clone().visit(&mut Dummifier)?;

    let mut generic_assumptions =
        TyckGenericConstraintAssumptionAdapter::new(analyzed_program.clone());
    let parsed_program = parsed_program.visit(&mut generic_assumptions)?;

    let mut dynamic_assumptions =
        TyckDynamicAssumptionAdapter::new(generic_assumptions.analyzed_program);
    let parsed_program = parsed_program.visit(&mut dynamic_assumptions)?;

    let analyzed_program = Rc::new(dynamic_assumptions.analyzed_program);
    let base_solver = TyckSolver::new(analyzed_program.clone());

    for imp in generic_assumptions.dummy_impls {
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

        if program.analyzed_impls[&imp.impl_id].is_regular() {
            typecheck_impl_collisions(&base_solver, &imp)?;
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

    let mut tycked_fns = hashmap! {};
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
fn typecheck_impl_collisions(base_solver: &TyckSolver, imp: &AstImpl) -> PResult<()> {
    if let Some(trait_ty) = &imp.trait_ty {
        let t = TyckInstantiatedImpl {
            impl_ty: imp.impl_ty.clone(),
            trait_ty: trait_ty.clone(),
            impl_signature: None,
        };

        let mut solver = base_solver.clone();
        solver.typecheck_loop(t)?;
    }

    Ok(())
}
