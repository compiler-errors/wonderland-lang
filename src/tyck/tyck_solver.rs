use crate::ana::represent::*;
use crate::parser::ast::*;
use crate::parser::ast_visitor::AstAdapter;
use crate::tyck::tyck_instantiate::GenericsInstantiator;
use crate::tyck::TyckObjective;
use crate::util::{IntoError, PResult, Span, Visit, ZipExact};
use std::collections::{HashMap, HashSet, VecDeque};
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct TyckSolver {
    solution: TyckSolution,

    /// Objectives that are used to explore the type space.
    objectives: VecDeque<TyckObjective>,

    /// Objectives which may only be solved after further inference or normalization.
    delayed_objectives: Vec<TyckDelayedObjective>,

    /// Type-space, so we make sure that every encountered type is normalized...
    types: HashSet<AstType>,

    analyzed_program: Rc<AnalyzedProgram>,
}

#[derive(Debug, Clone)]
pub struct TyckSolution {
    analyzed_program: Rc<AnalyzedProgram>,
    inferences: HashMap<InferId, AstType>,
    impl_signatures: HashMap<TyckObjective, AstImplSignature>,
}

impl TyckSolver {
    pub fn new(analyzed_program: Rc<AnalyzedProgram>) -> TyckSolver {
        TyckSolver {
            solution: TyckSolution {
                analyzed_program: analyzed_program.clone(),
                inferences: HashMap::new(),
                impl_signatures: HashMap::new(),
            },

            analyzed_program,
            objectives: VecDeque::new(),
            delayed_objectives: Vec::new(),
            types: HashSet::new(),
        }
    }
}

#[derive(Debug, Clone)]
enum TyckDelayedObjective {
    Unify(AstType, AstType),
    TupleAccess(AstType, usize, AstType),
    ObjectAccess(AstType, String, AstType),
    Nullable(AstType),
}

static MAX_ITERATIONS: usize = 1_000_000_usize;

impl TyckSolver {
    fn error<T>(why: &str) -> PResult<T> {
        PResult::error(format!("Problem during type checker! {}.", why))
    }

    fn is_solved(&self) -> bool {
        self.delayed_objectives.is_empty() && self.objectives.is_empty()
    }

    fn is_deadlocked(&self) -> bool {
        self.objectives.is_empty()
    }

    pub fn solve(mut self) -> PResult<TyckSolution> {
        let mut multiverse = VecDeque::new();
        self.normalize()?;
        multiverse.push_back(self.clone());

        let iterations = 0usize;
        while let Some(universe) = multiverse.pop_front() {
            if iterations > MAX_ITERATIONS {
                break;
            }

            if universe.is_solved() {
                // println!("Done!");
                return Ok(universe.solution);
            }

            if universe.is_deadlocked() {
                println!("Deadlocked universe...");
                continue;
            }

            if let Ok(children) = universe.elaborate_all() {
                for c in children {
                    multiverse.push_back(c);
                }
            }

            /* Otherwise, just throw it away. */
        }

        TyckSolver::error("No more solutions")
    }

    pub fn add_objective(&mut self, obj_ty: &AstType, trait_ty: &AstTraitType) -> PResult<()> {
        let t = TyckObjective {
            obj_ty: obj_ty.clone(),
            trait_ty: trait_ty.clone(),
        };

        if !self.objectives.contains(&t) {
            self.objectives.push_back(t);
        }

        Ok(())
    }

    pub fn add_objectives(&mut self, objectives: &[AstTypeRestriction]) -> PResult<()> {
        for AstTypeRestriction { ty, trt } in objectives {
            self.add_objective(ty, trt)?;
        }

        Ok(())
    }

    pub fn add_objective_well_formed(
        &mut self,
        obj_name: &ModuleRef,
        generics: &[AstType],
    ) -> PResult<()> {
        let program = self.analyzed_program.clone();
        let obj_data = &program.analyzed_objects[obj_name];
        let mut instantiate = GenericsInstantiator::from_generics(&obj_data.generics, &generics)?;

        for r in &obj_data.restrictions.clone().visit(&mut instantiate)? {
            self.add_objective(&r.ty, &r.trt)?;
        }

        Ok(())
    }

    pub fn add_type(&mut self, t: &AstType) -> PResult<()> {
        self.types.insert(t.clone());

        Ok(())
    }

    pub fn add_delayed_unify(&mut self, a: &AstType, b: &AstType) -> PResult<()> {
        if let AstType::AssociatedType {
            obj_ty, trait_ty, ..
        } = a
        {
            self.add_objective(obj_ty.as_ref(), trait_ty.as_ref().unwrap())?;
        }

        if let AstType::AssociatedType {
            obj_ty, trait_ty, ..
        } = b
        {
            self.add_objective(obj_ty.as_ref(), trait_ty.as_ref().unwrap())?;
        }

        self.delayed_objectives
            .push(TyckDelayedObjective::Unify(a.clone(), b.clone()));
        Ok(())
    }

    pub fn add_delayed_nullable_goal(&mut self, ty: &AstType) -> PResult<()> {
        self.delayed_objectives
            .push(TyckDelayedObjective::Nullable(ty.clone()));
        Ok(())
    }

    pub fn add_delayed_tuple_access(
        &mut self,
        tuple_ty: &AstType,
        idx: usize,
        element_ty: &AstType,
    ) -> PResult<()> {
        self.delayed_objectives
            .push(TyckDelayedObjective::TupleAccess(
                tuple_ty.clone(),
                idx,
                element_ty.clone(),
            ));
        Ok(())
    }

    pub fn add_delayed_object_access(
        &mut self,
        object_ty: &AstType,
        member_name: &str,
        member_ty: &AstType,
    ) -> PResult<()> {
        self.delayed_objectives
            .push(TyckDelayedObjective::ObjectAccess(
                object_ty.clone(),
                member_name.into(),
                member_ty.clone(),
            ));
        Ok(())
    }

    fn elaborate_all(mut self) -> PResult<Vec<TyckSolver>> {
        let objective = self.objectives.pop_front().unwrap();

        if self.solution.impl_signatures.contains_key(&objective) {
            self.normalize()?;
            return Ok(vec![self.clone()]);
        }

        let mut new_universes = Vec::new();

        for i in self.get_impls(&objective.trait_ty)? {
            if let Ok(new_universe) = self.clone().elaborate(i, &objective) {
                new_universes.push(new_universe);
            }
        }

        Ok(new_universes)
    }

    fn elaborate(mut self, impl_id: ImplId, objective: &TyckObjective) -> PResult<TyckSolver> {
        let program = self.analyzed_program.clone();
        let impl_data = &program.analyzed_impls[&impl_id];
        let generics = impl_data
            .generics
            .iter()
            .map(|_| AstType::infer())
            .collect();
        let impl_signature = AstImplSignature { impl_id, generics };
        let instantiate =
            &mut GenericsInstantiator::from_signature(&*self.analyzed_program, &impl_signature)?;

        let obj_ty = impl_data.impl_ty.clone().visit(instantiate)?;
        let trait_ty = impl_data.trait_ty.clone().visit(instantiate)?;

        self.unify(&objective.obj_ty, &obj_ty)?;
        self.unify_traits(&objective.trait_ty, &trait_ty)?;

        for r in &impl_data.restrictions.clone().visit(instantiate)? {
            self.add_objective(&r.ty, &r.trt)?;
        }

        if let Some(_) = self
            .solution
            .impl_signatures
            .insert(objective.clone(), impl_signature)
        {
            // This legitimately should never happen, because:
            // 1. Already-satisfied objectives should be skipped in the solve loop.
            // 2. Conflicting impls should fail during normalization.
            unreachable!();
        }

        self.normalize()?;
        Ok(self)
    }

    fn get_impls(&self, objective: &AstTraitType) -> PResult<Vec<ImplId>> {
        let mut impls = Vec::new();

        for (id, i) in &self.analyzed_program.analyzed_impls {
            if i.trait_ty.0 == objective.0 {
                impls.push(*id);
            }
        }

        Ok(impls)
    }

    pub fn unify(&mut self, lhs: &AstType, rhs: &AstType) -> PResult<()> {
        let lhs = self.normalize_ty(lhs)?;
        let rhs = self.normalize_ty(rhs)?;

        println!("; Unifying {} and {}", lhs, rhs);

        match (&lhs, &rhs) {
            /* Generics should have been repl'ed out. */
            (AstType::Generic(..), _)
            | (_, AstType::Generic(..))
            | (AstType::GenericPlaceholder(..), _)
            | (_, AstType::GenericPlaceholder(..)) => unreachable!(),

            (a @ AstType::AssociatedType { .. }, b) | (a, b @ AstType::AssociatedType { .. }) => {
                self.add_delayed_unify(&a, &b)?;

                Ok(())
            }
            (AstType::Infer(lid), AstType::Infer(rid)) if lid == rid => {
                /* Do nothing. No cycles in this house. */

                Ok(())
            }
            (AstType::Infer(lid), rhs) => {
                if self.solution.inferences.insert(*lid, rhs.clone()).is_some() {
                    panic!("ICE: Duplicated inference");
                }

                Ok(())
            }
            (lhs, AstType::Infer(rid)) => {
                if self.solution.inferences.insert(*rid, lhs.clone()).is_some() {
                    panic!("ICE: Duplicated inference");
                }

                Ok(())
            }

            (AstType::Int, AstType::Int)
            | (AstType::Char, AstType::Char)
            | (AstType::Bool, AstType::Bool)
            | (AstType::String, AstType::String) => Ok(()),

            (lhs, rhs @ AstType::SelfType) | (lhs @ AstType::SelfType, rhs) => unreachable!(
                "Self is not allowed as a non-instantiated type. Attempted to unify {:?} and {:?}",
                lhs, rhs
            ),

            (AstType::DummyGeneric(a, ..), AstType::DummyGeneric(b, ..)) if a == b => Ok(()),
            (AstType::Dummy(a), AstType::Dummy(b)) if a == b => Ok(()),

            (AstType::Array { ty: a_ty }, AstType::Array { ty: b_ty }) => {
                self.unify(&*a_ty, &*b_ty)
            }
            (AstType::Tuple { types: a_tys }, AstType::Tuple { types: b_tys }) => {
                for (a_ty, b_ty) in ZipExact::zip_exact(a_tys, b_tys, "tuple types")? {
                    self.unify(a_ty, b_ty)?;
                }

                Ok(())
            }
            (AstType::Object(a_name, a_tys), AstType::Object(b_name, b_tys)) => {
                if a_name != b_name {
                    TyckSolver::error(&format!(
                        "Object names won't unify: {} and {}",
                        a_name.full_name()?,
                        b_name.full_name()?
                    ))
                } else {
                    for (a_ty, b_ty) in ZipExact::zip_exact(a_tys, b_tys, "object generics")? {
                        self.unify(a_ty, b_ty)?;
                    }

                    Ok(())
                }
            }

            (
                AstType::ClosureType {
                    args: a_args,
                    ret_ty: a_ret,
                },
                AstType::ClosureType {
                    args: b_args,
                    ret_ty: b_ret,
                },
            ) => {
                for (a_ty, b_ty) in ZipExact::zip_exact(a_args, b_args, "closure arguments")? {
                    self.unify(a_ty, b_ty)?;
                }

                self.unify(a_ret, b_ret)?;
                Ok(())
            }
            (
                AstType::FnPointerType {
                    args: a_args,
                    ret_ty: a_ret,
                },
                AstType::FnPointerType {
                    args: b_args,
                    ret_ty: b_ret,
                },
            ) => {
                for (a_ty, b_ty) in ZipExact::zip_exact(a_args, b_args, "closure arguments")? {
                    self.unify(a_ty, b_ty)?;
                }

                self.unify(a_ret, b_ret)?;
                Ok(())
            }

            (a, b) => TyckSolver::error(&format!("Type non-union: {:?} and {:?}", a, b)),
        }
    }

    pub fn unify_all(&mut self, a: &[AstType], b: &[AstType]) -> PResult<()> {
        for (a, b) in ZipExact::zip_exact(a, b, "arguments")? {
            self.unify(a, b)?;
        }

        Ok(())
    }

    pub fn unify_traits(&mut self, lhs: &AstTraitType, rhs: &AstTraitType) -> PResult<()> {
        if lhs.0 == rhs.0 {
            for (l, r) in ZipExact::zip_exact(&lhs.1, &rhs.1, "trait generics")? {
                self.unify(l, r)?;
            }

            Ok(())
        } else {
            TyckSolver::error(&format!(
                "Trait types won't unify: {} and {}",
                lhs.0.full_name()?,
                rhs.0.full_name()?
            ))
        }
    }

    fn normalize(&mut self) -> PResult<()> {
        let mut types = HashSet::new();
        std::mem::swap(&mut types, &mut self.types);

        for ty in types {
            let ty = self.normalize_ty(&ty)?;

            if let AstType::AssociatedType {
                obj_ty, trait_ty, ..
            } = &ty
            {
                self.add_objective(obj_ty.as_ref(), trait_ty.as_ref().unwrap())?;
            }

            self.types.insert(ty);
        }

        let mut inferences = HashMap::new();
        std::mem::swap(&mut inferences, &mut self.solution.inferences);

        for (id, ty) in inferences {
            self.solution.inferences.insert(id, self.normalize_ty(&ty)?);
        }

        let mut impl_signatures = HashMap::new();
        std::mem::swap(&mut impl_signatures, &mut self.solution.impl_signatures);

        for (obj, sig) in impl_signatures {
            let obj = self.normalize_objective(&obj)?;
            let sig = self.normalize_impl_signature(&sig)?;

            if let Some(other_sig) = self
                .solution
                .impl_signatures
                .insert(obj.clone(), sig.clone())
            {
                // It's possible that we got to the same impl through different objectives.
                // In this case, it's fine that they now point to the same objective.
                // However, they MUST be equivalent.
                if other_sig.impl_id == sig.impl_id {
                    self.unify_all(&sig.generics, &other_sig.generics)?;
                } else {
                    TyckSolver::error(&format!(
                        "Impl objective {:?} provided by two impls: {:?} and {:?}",
                        obj, sig, other_sig
                    ))?;
                }
            }
        }

        let mut objectives = VecDeque::new();
        std::mem::swap(&mut objectives, &mut self.objectives);

        for obj in &objectives {
            let obj = self.normalize_objective(obj)?;

            if self.objectives.contains(&obj) {
                continue;
            }

            self.objectives.push_back(obj);
        }

        let mut progress = true;
        while progress {
            progress = false;

            // The next step will push all still-delayed unifications into the set.
            // We do this iteratively because sometimes one unstuck objective will cause
            // another objective to become unstuck.
            // I know, I hate that too.
            let mut delayed_objectives = Vec::new();
            std::mem::swap(&mut delayed_objectives, &mut self.delayed_objectives);

            for d in delayed_objectives {
                match d {
                    TyckDelayedObjective::Unify(a, b) => {
                        let norm_a = self.normalize_ty(&a)?;
                        let norm_b = self.normalize_ty(&b)?;

                        self.unify(&norm_a, &norm_b)?;

                        if a != norm_a || b != norm_b {
                            progress = true;
                        }
                    }
                    TyckDelayedObjective::TupleAccess(tuple, idx, element) => {
                        let tuple = self.normalize_ty(&tuple)?;
                        let element = self.normalize_ty(&element)?;

                        if let AstType::Tuple { types } = &tuple {
                            if types.len() > idx {
                                self.unify(&element, &types[idx])?;
                                progress = true;
                            } else {
                                TyckSolver::error("Tuple has wrong number of arguments")?;
                            }
                        } else if let AstType::Infer(..) = &tuple {
                            self.delayed_objectives
                                .push(TyckDelayedObjective::TupleAccess(tuple, idx, element));
                        } else {
                            TyckSolver::error(&format!("Tuple type expected, got {:?}", tuple))?;
                        }
                    }
                    TyckDelayedObjective::ObjectAccess(object, member_name, member) => {
                        let object = self.normalize_ty(&object)?;
                        let member = self.normalize_ty(&member)?;

                        if let AstType::Object(object_name, generics) = &object {
                            let expected_member = GenericsInstantiator::instantiate_object_member(
                                &*self.analyzed_program,
                                object_name,
                                generics,
                                &member_name,
                            )?;
                            self.unify(&expected_member, &member)?;
                            progress = true;
                        } else if let AstType::Infer(..) = &object {
                            self.delayed_objectives
                                .push(TyckDelayedObjective::ObjectAccess(
                                    object,
                                    member_name,
                                    member,
                                ));
                        } else {
                            TyckSolver::error(&format!("Object type expected, got {:?}", object))?;
                        }
                    }
                    TyckDelayedObjective::Nullable(object) => {
                        match &self.normalize_ty(&object)? {
                            AstType::Object(..) | AstType::String | AstType::Array { .. } => {
                                /* Okay! */
                                progress = true;
                            }
                            AstType::Infer(..) => {
                                self.delayed_objectives
                                    .push(TyckDelayedObjective::Nullable(object));
                            }
                            _ => {
                                return TyckSolver::error(&format!(
                                    "Object type expected, got {:?}",
                                    object
                                ))
                            }
                        }
                    }
                }
            }
        }

        Ok(())
    }

    fn normalize_ty(&self, t: &AstType) -> PResult<AstType> {
        self.solution.normalize_ty(t)
    }

    fn normalize_trait_ty(&self, t: &AstTraitType) -> PResult<AstTraitType> {
        self.solution.normalize_trait_ty(t)
    }

    fn normalize_objective(&self, t: &TyckObjective) -> PResult<TyckObjective> {
        let obj_ty = self.normalize_ty(&t.obj_ty)?;
        let trait_ty = self.normalize_trait_ty(&t.trait_ty)?;

        Ok(TyckObjective { obj_ty, trait_ty })
    }

    fn normalize_impl_signature(&self, i: &AstImplSignature) -> PResult<AstImplSignature> {
        let generics = i.generics.clone().visit(&mut Normalize(&self.solution))?;

        Ok(AstImplSignature {
            impl_id: i.impl_id,
            generics,
        })
    }
}

impl TyckSolution {
    pub fn get_impl_signature(
        &mut self,
        span: Span,
        obj_ty: &AstType,
        trait_ty: &AstTraitType,
    ) -> PResult<AstImplSignature> {
        let t = TyckObjective {
            obj_ty: obj_ty.clone(),
            trait_ty: trait_ty.clone(),
        };

        if let Some(sig) = self.impl_signatures.get(&t) {
            Ok(sig.clone())
        } else {
            PResult::error_at(
                span,
                format!(
                    "Can't find implementation for {:?} :- {:?}",
                    obj_ty, trait_ty
                ),
            )
        }
    }

    pub fn normalize_ty(&self, t: &AstType) -> PResult<AstType> {
        t.clone().visit(&mut Normalize(self))
    }

    pub fn normalize_trait_ty(&self, t: &AstTraitType) -> PResult<AstTraitType> {
        let name = t.0.clone();
        let tys = t.1.clone().visit(&mut Normalize(self))?;

        Ok(AstTraitType(name, tys))
    }
}

struct Normalize<'a>(&'a TyckSolution);

impl<'a> AstAdapter for Normalize<'a> {
    /* We do this as we walk the tree back up, just in case some replacement of a child causes
     * a type to now associate correctly to an impl signature.
     *
     * Imagine we have <_0 as Trait>::I, but then _0 gets replaced with Int by normalization.
     * Then if we have an impl candidate already chosen for <Int as Trait>, then we can match it! */
    fn exit_type(&mut self, t: AstType) -> PResult<AstType> {
        match &t {
            AstType::Infer(id) => {
                if let Some(t) = self.0.inferences.get(id) {
                    return t.clone().visit(self);
                }
            }
            AstType::AssociatedType {
                obj_ty,
                trait_ty,
                name,
            } => {
                let typecheck_objective = &TyckObjective {
                    obj_ty: *obj_ty.clone(),
                    trait_ty: trait_ty.as_ref().unwrap().clone(),
                };

                if let Some(impl_signature) = self.0.impl_signatures.get(typecheck_objective) {
                    let instantiate = GenericsInstantiator::instantiate_associated_ty(
                        &*self.0.analyzed_program,
                        impl_signature,
                        name,
                    )?;
                    return instantiate.visit(self);
                }
            }
            _ => {}
        }

        Ok(t)
    }
}
