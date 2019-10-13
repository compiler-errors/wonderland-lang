use crate::ana::represent::*;
use crate::inst::post_solve::PostSolveAdapter;
use crate::inst::represent::*;
use crate::parser::ast::*;
use crate::parser::ast_visitor::AstAdapter;
use crate::tyck::*;
use crate::util::{Expect, IntoError, PResult, Visit};
use std::collections::HashMap;
use std::rc::Rc;

mod post_solve;
mod represent;

struct InstantiationAdapter {
    analyzed_program: Rc<AnalyzedProgram>,
    base_solver: TyckSolver,

    fns: HashMap<ModuleRef, AstFunction>,
    objects: HashMap<ModuleRef, AstObject>,
    impls: HashMap<ImplId, AstImpl>,
    obj_fns: HashMap<(ImplId, String), AstObjectFunction>,

    instantiated_fns: HashMap<InstFunctionSignature, Option<AstFunction>>,
    instantiated_object_fns: HashMap<InstObjectFunctionSignature, Option<AstObjectFunction>>,
    instantiated_impls: HashMap<InstImplSignature, Option<AstImpl>>,
    instantiated_objects: HashMap<InstObjectSignature, Option<AstObject>>,

    solved_impls: HashMap<TyckObjective, InstImplSignature>,
}

#[derive(Debug)]
pub struct InstantiatedProgram {
    pub instantiated_fns: HashMap<InstFunctionSignature, Option<AstFunction>>,
    pub instantiated_object_fns: HashMap<InstObjectFunctionSignature, Option<AstObjectFunction>>,
    pub instantiated_impls: HashMap<InstImplSignature, Option<AstImpl>>,
    pub instantiated_objects: HashMap<InstObjectSignature, Option<AstObject>>,
}

pub fn instantiate(
    analyzed_program: AnalyzedProgram,
    parsed_program: AstProgram,
) -> PResult<InstantiatedProgram> {
    let mut main_finder = MainFinder(None);
    let parsed_program = parsed_program.visit(&mut main_finder)?;

    let mut fns = HashMap::new();
    let mut objects = HashMap::new();
    let mut impls = HashMap::new();
    let mut obj_fns = HashMap::new();

    for m in parsed_program.modules {
        for (_, f) in m.functions {
            fns.insert(f.module_ref.clone(), f);
        }

        for (_, f) in m.objects {
            objects.insert(f.module_ref.clone(), f);
        }

        for (id, i) in m.impls {
            for (name, fun) in &i.fns {
                obj_fns.insert((id, name.clone()), fun.clone());
            }

            impls.insert(id, i);
        }
    }

    let analyzed_program = Rc::new(analyzed_program);

    let mut i = InstantiationAdapter {
        base_solver: TyckSolver::new(analyzed_program.clone()),
        analyzed_program,

        fns,
        objects,
        impls,
        obj_fns,

        instantiated_fns: HashMap::new(),
        instantiated_object_fns: HashMap::new(),
        instantiated_impls: HashMap::new(),
        instantiated_objects: HashMap::new(),
        solved_impls: HashMap::new(),
    };

    if let Some(name) = main_finder.0 {
        i.instantiate_function(&name, &[])?;
    } else {
        return PResult::error("Cannot find main function.".into());
    }

    Ok(InstantiatedProgram {
        instantiated_fns: i.instantiated_fns,
        instantiated_object_fns: i.instantiated_object_fns,
        instantiated_impls: i.instantiated_impls,
        instantiated_objects: i.instantiated_objects,
    })
}

impl InstantiationAdapter {
    fn instantiate_function(&mut self, name: &ModuleRef, generics: &[AstType]) -> PResult<()> {
        let sig = InstFunctionSignature(name.clone(), generics.to_vec());

        if self.instantiated_fns.contains_key(&sig) {
            return Ok(());
        }

        // Insert so we don't recurse infinitely.
        self.instantiated_fns.insert(sig.clone(), None);

        let ids = &self.analyzed_program.clone().analyzed_functions[name].generics;
        let f = self.process_simple(self.fns[name].clone(), ids, generics)?;

        self.instantiated_fns.insert(sig, Some(f));

        Ok(())
    }

    fn instantiate_object(&mut self, name: &ModuleRef, generics: &[AstType]) -> PResult<()> {
        let sig = InstObjectSignature(name.clone(), generics.to_vec());

        if self.instantiated_objects.contains_key(&sig) {
            return Ok(());
        }

        // Insert so we don't recurse infinitely.
        self.instantiated_objects.insert(sig.clone(), None);

        let ids = &self.analyzed_program.clone().analyzed_objects[name].generics;
        let o = self.process_simple(self.objects[name].clone(), ids, generics)?;

        self.instantiated_objects.insert(sig, Some(o));

        Ok(())
    }

    fn instantiate_impl(
        &mut self,
        sig: &AstImplSignature,
        ty: &AstType,
        trt: &AstTraitType,
    ) -> PResult<()> {
        let id = sig.impl_id;
        let generics = &sig.generics;
        let sig = InstImplSignature(sig.impl_id, sig.generics.clone());

        if self.instantiated_impls.contains_key(&sig) {
            return Ok(());
        }

        self.instantiated_impls.insert(sig.clone(), None);
        let objective = TyckObjective {
            obj_ty: ty.clone(),
            trait_ty: trt.clone(),
        };

        // Ensure that this is the only impl for this specific `<ty as trt>::...`
        self.solved_impls
            .insert(objective, sig.clone())
            .is_not_expected(self.impls[&id].name_span, "impl", "<conflicting types>")?;

        let ids = &self.analyzed_program.clone().analyzed_impls[&id].generics;
        let mut instantiate = GenericsInstantiator::from_generics(ids, generics)?;

        let mut imp = self.impls[&id].clone();
        imp.impl_ty = imp.impl_ty.visit(&mut instantiate)?;
        imp.trait_ty = imp.trait_ty.visit(&mut instantiate)?;
        imp.restrictions = imp.restrictions.visit(&mut instantiate)?;
        imp.associated_types = imp.associated_types.visit(&mut instantiate)?;

        let (mut imp, solution) =
            typecheck_impl(self.analyzed_program.clone(), &self.base_solver, imp)?;
        let mut post_solve = PostSolveAdapter(solution, self.analyzed_program.clone());

        imp.impl_ty = imp.impl_ty.visit(&mut post_solve)?.visit(self)?;
        imp.trait_ty = imp.trait_ty.visit(&mut post_solve)?.visit(self)?;
        imp.restrictions = imp.restrictions.visit(&mut post_solve)?.visit(self)?;
        imp.associated_types = imp.associated_types.visit(&mut post_solve)?.visit(self)?;

        self.instantiated_impls.insert(sig, Some(imp));

        Ok(())
    }

    fn instantiate_object_function(
        &mut self,
        call_type: &AstType,
        trt: &AstTraitType,
        impl_sig: &AstImplSignature,
        fn_name: &str,
        fn_generics: &[AstType],
    ) -> PResult<()> {
        self.instantiate_impl(impl_sig, call_type, trt)?;

        let id = impl_sig.impl_id;
        let sig = InstObjectFunctionSignature(
            call_type.clone(),
            trt.clone(),
            fn_name.into(),
            fn_generics.into(),
        );

        if self.instantiated_object_fns.contains_key(&sig) {
            return Ok(());
        }

        // Insert so we don't recurse infinitely.
        self.instantiated_object_fns.insert(sig.clone(), None);

        let impl_data = &self.analyzed_program.clone().analyzed_impls[&id];
        let fn_data = &impl_data.methods[fn_name];

        let obj_ids = &impl_data.generics;
        let obj_generics = &impl_sig.generics;
        let fn_ids = &fn_data.generics;

        // Let's first instantiate it.
        let mut obj_instantiate = GenericsInstantiator::from_generics(obj_ids, obj_generics)?;
        let mut fn_instantiate = GenericsInstantiator::from_generics(fn_ids, fn_generics)?;
        let f = self.obj_fns[&(id, fn_name.into())]
            .clone()
            .visit(&mut obj_instantiate)?
            .visit(&mut fn_instantiate)?;

        let (f, solution) = typecheck_impl_fn(
            self.analyzed_program.clone(),
            &self.base_solver,
            f,
            call_type,
            trt,
            fn_generics,
        )?;
        let mut post_solve = PostSolveAdapter(solution, self.analyzed_program.clone());
        let f = f.visit(&mut post_solve)?.visit(self)?;

        self.instantiated_object_fns.insert(sig, Some(f));

        Ok(())
    }

    fn process_simple<T>(&mut self, t: T, ids: &[AstGeneric], tys: &[AstType]) -> PResult<T>
    where
        T: Visit<GenericsInstantiator>
            + Visit<TyckObjectiveAdapter>
            + Visit<PostSolveAdapter>
            + Visit<InstantiationAdapter>,
    {
        // Let's first instantiate it.
        let mut instantiate = GenericsInstantiator::from_generics(ids, tys)?;
        let t = t.visit(&mut instantiate)?;

        let (t, solution) = typecheck_simple(self.analyzed_program.clone(), &self.base_solver, t)?;
        let mut post_solve = PostSolveAdapter(solution, self.analyzed_program.clone());
        let t = t.visit(&mut post_solve)?.visit(self)?;

        Ok(t)
    }
}

impl AstAdapter for InstantiationAdapter {
    fn enter_type(&mut self, t: AstType) -> PResult<AstType> {
        match &t {
            AstType::Object(name, generics) => {
                self.instantiate_object(name, generics)?;
            }
            _ => { /* Do nothing. */ }
        }

        Ok(t)
    }

    fn enter_expression(&mut self, e: AstExpression) -> PResult<AstExpression> {
        match &e.data {
            AstExpressionData::Call {
                fn_name, generics, ..
            } => {
                self.instantiate_function(fn_name, generics)?;
            }
            AstExpressionData::StaticCall {
                call_type,
                associated_trait,
                impl_signature,
                fn_name,
                fn_generics,
                ..
            } => {
                let trt = associated_trait.as_ref().unwrap();
                let impl_sig = impl_signature.as_ref().unwrap();

                self.instantiate_object_function(call_type, trt, impl_sig, fn_name, fn_generics)?;
            }
            _ => { /* Do nothing. */ }
        }

        Ok(e)
    }
}

struct MainFinder(Option<ModuleRef>);

impl AstAdapter for MainFinder {
    fn enter_function(&mut self, f: AstFunction) -> PResult<AstFunction> {
        if &f.name == "main" {
            if self.0.is_some() {
                return PResult::error(format!(
                    "Duplicated `main` function: first found at `{}`, also found at `{}`",
                    self.0.as_ref().unwrap().full_name()?,
                    f.module_ref.full_name()?
                ));
            }

            if !f.generics.is_empty() {
                return PResult::error(format!(
                    "Main function `{}` must have zero generics, found {}.",
                    f.module_ref.full_name()?,
                    f.generics.len()
                ));
            }

            self.0 = Some(f.module_ref.clone());
        }

        Ok(f)
    }
}
