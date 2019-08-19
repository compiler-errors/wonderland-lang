use crate::analyze::represent::*;
use crate::parser::ast::*;
use crate::parser::ast_visitor::*;
use crate::tyck::tyck_instantiate::InstantiateAdapter;
use crate::tyck::*;
use crate::util::result::*;
use crate::util::{Span, ZipExact};
use std::collections::{HashMap, VecDeque};
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct TyckSolver {
    inferences: HashMap<InferId, AstType>,
    impl_signatures: HashMap<TyckObjective, TyckImplSignature>,
    objectives: VecDeque<TyckObjective>,
    delayed_unifications: Vec<TyckDelayedObjective>,

    analyzed_file: Rc<AnalyzedFile>,
}

#[derive(Debug, Clone)]
enum TyckDelayedObjective {
    Unify(AstType, AstType),
    TupleAccess(AstType, usize, AstType),
    ObjectAccess(AstType, String, AstType),
    Object(AstType),
}

static MAX_ITERATIONS: usize = 1_000_000_usize;

impl TyckSolver {
    fn error<T>() -> PResult<T> {
        PError::new(Span::new(0, 0), format!("Problem during type checker!"))
    }

    pub fn is_solved(&self) -> bool {
        self.delayed_unifications.is_empty() && self.objectives.is_empty()
    }

    pub fn solve(mut self) -> PResult<TyckSolver> {
        let mut multiverse = VecDeque::new();
        self.normalize()?;
        multiverse.push_back(self);

        let mut iterations = 0usize;
        while let Some(mut universe) = multiverse.pop_front() {
            if iterations > MAX_ITERATIONS {
                break;
            }

            if universe.is_solved() {
                universe.normalize()?;
                // Yay! We're done.
                return Ok(universe);
            }

            if let Ok(children) = universe.elaborate_all() {
                for c in children {
                    multiverse.push_back(c);
                }
            }

            /* Otherwise, just throw it away. */
        }

        TyckSolver::error()
    }

    pub fn unify(&mut self, lhs: &AstType, rhs: &AstType) -> PResult<()> {
        let lhs = self.normalize_ty(lhs)?;
        let rhs = self.normalize_ty(rhs)?;

        match (lhs, rhs) {
            (a @ AstType::AssociatedType { .. }, b) | (a, b @ AstType::AssociatedType { .. }) => {
                if !a.is_dummy_equivalent(&b) {
                    self.add_delayed_unify(&a, &b)?;
                }

                Ok(())
            }
            (AstType::Infer(lid), rhs) => {
                self.inferences.insert(lid, rhs.clone()).not_expected(
                    Span::new(0, 0),
                    "inference",
                    &format!("_{}", lid.0),
                )?;

                Ok(())
            }
            (lhs, AstType::Infer(rid)) => {
                self.inferences.insert(rid, lhs.clone()).not_expected(
                    Span::new(0, 0),
                    "inference",
                    &format!("_{}", rid.0),
                )?;

                Ok(())
            }

            (AstType::Int, AstType::Int)
            | (AstType::Char, AstType::Char)
            | (AstType::Bool, AstType::Bool)
            | (AstType::String, AstType::String)
            | (AstType::SelfType, AstType::SelfType) => Ok(()),

            /* Generics should have been repl'ed out. */
            (AstType::Generic(..), _)
            | (_, AstType::Generic(..))
            | (AstType::GenericPlaceholder(..), _)
            | (_, AstType::GenericPlaceholder(..)) => unreachable!(),

            (AstType::Array { ty: a_ty }, AstType::Array { ty: b_ty }) => {
                self.unify(&*a_ty, &*b_ty)
            }
            (AstType::Tuple { types: ref a_tys }, AstType::Tuple { types: ref b_tys }) => {
                for (a_ty, b_ty) in ZipExact::zip_exact(a_tys, b_tys, "tuple types")? {
                    self.unify(a_ty, b_ty)?;
                }

                Ok(())
            }
            (AstType::Object(ref a_name, ref a_tys), AstType::Object(ref b_name, ref b_tys)) => {
                if a_name != b_name {
                    TyckSolver::error()
                } else {
                    for (a_ty, b_ty) in ZipExact::zip_exact(a_tys, b_tys, "object generics")? {
                        self.unify(a_ty, b_ty)?;
                    }

                    Ok(())
                }
            }

            (_, _) => TyckSolver::error(),
        }
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

    pub fn add_delayed_unify(&mut self, a: &AstType, b: &AstType) -> PResult<()> {
        self.delayed_unifications
            .push(TyckDelayedObjective::Unify(a.clone(), b.clone()));
        Ok(())
    }

    pub fn add_delayed_object_goal(&mut self, ty: &AstType) -> PResult<()> {
        self.delayed_unifications
            .push(TyckDelayedObjective::Object(ty.clone()));
        Ok(())
    }

    pub fn add_delayed_tuple_access(
        &mut self,
        tuple_ty: &AstType,
        idx: usize,
        element_ty: &AstType,
    ) -> PResult<()> {
        self.delayed_unifications
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
        member_name: &String,
        member_ty: &AstType,
    ) -> PResult<()> {
        self.delayed_unifications
            .push(TyckDelayedObjective::ObjectAccess(
                object_ty.clone(),
                member_name.clone(),
                member_ty.clone(),
            ));
        Ok(())
    }

    fn elaborate_all(mut self) -> PResult<Vec<TyckSolver>> {
        let objective = self.objectives.pop_front().unwrap();

        if self.impl_signatures.contains_key(&objective) {
            self.normalize()?;
            return Ok(vec![self]);
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
        let impl_data = &self.analyzed_file.analyzed_impls[&impl_id];
        let generics = impl_data
            .generics
            .iter()
            .map(|_| AstType::infer())
            .collect();
        let impl_signature = TyckImplSignature { impl_id, generics };
        let instantiate =
            &mut InstantiateAdapter::from_signature(&*self.analyzed_file, &impl_signature)?;

        let obj_ty = impl_data.impl_ty.clone().visit(instantiate)?;
        let trait_ty = impl_data.trait_ty.clone().visit(instantiate)?;

        self.unify(&objective.obj_ty, &obj_ty)?;
        self.unify_traits(&objective.trait_ty, &trait_ty)?;

        // Conflicted impl?!
        if let Some(_) = self
            .impl_signatures
            .insert(objective.clone(), impl_signature)
        {
            unreachable!();
        }

        self.normalize()?;

        Ok(self)
    }

    fn get_impls(&self, objective: &AstTraitType) -> PResult<Vec<ImplId>> {
        let mut impls = Vec::new();

        for (id, i) in &self.analyzed_file.analyzed_impls {
            if i.trait_ty.0 == objective.0 {
                impls.push(*id);
            }
        }

        Ok(impls)
    }

    pub fn unify_all(&mut self, a: &Vec<AstType>, b: &Vec<AstType>) -> PResult<()> {
        for (a, b) in ZipExact::zip_exact(a, b, "arguments")? {
            self.unify(a, b)?;
        }

        Ok(())
    }

    fn unify_traits(&mut self, lhs: &AstTraitType, rhs: &AstTraitType) -> PResult<()> {
        if lhs.0 == rhs.0 {
            for (l, r) in ZipExact::zip_exact(&lhs.1, &rhs.1, "trait generics")? {
                self.unify(l, r)?;
            }

            Ok(())
        } else {
            TyckSolver::error()
        }
    }

    fn normalize(&mut self) -> PResult<()> {
        let mut inferences = HashMap::new();
        for (id, ty) in &self.inferences {
            inferences.insert(*id, self.normalize_ty(ty)?);
        }

        let mut impl_signatures = HashMap::new();
        for (obj, sig) in &self.impl_signatures {
            let obj = self.normalize_objective(obj)?;
            let sig = self.normalize_impl_signature(sig)?;

            if impl_signatures.insert(obj, sig).is_some() {
                Self::error()?;
            }
        }

        let mut objectives = VecDeque::new();
        for obj in &self.objectives {
            let obj = self.normalize_objective(obj)?;

            if objectives.contains(&obj) {
                continue;
            }

            objectives.push_back(obj);
        }

        self.inferences = inferences;
        self.impl_signatures = impl_signatures;
        self.objectives = objectives;

        // The next step will push all still-delayed unifications into the set.
        let mut delayed_unifications = Vec::new();
        std::mem::swap(&mut delayed_unifications, &mut self.delayed_unifications);

        for d in delayed_unifications {
            match d {
                TyckDelayedObjective::Unify(a, b) => {
                    let a = self.normalize_ty(&a)?;
                    let b = self.normalize_ty(&b)?;

                    self.unify(&a, &b)?;
                }
                TyckDelayedObjective::TupleAccess(tuple, idx, element) => {
                    let tuple = self.normalize_ty(&tuple)?;
                    let element = self.normalize_ty(&element)?;

                    if let AstType::Tuple { types } = &tuple {
                        if types.len() > idx {
                            self.unify(&element, &types[idx])?;
                        } else {
                            Self::error()?;
                        }
                    } else if let AstType::Infer(..) = &tuple {
                        self.delayed_unifications
                            .push(TyckDelayedObjective::TupleAccess(tuple, idx, element));
                    } else {
                        Self::error()?;
                    }
                }
                TyckDelayedObjective::ObjectAccess(object, member_name, member) => {
                    let object = self.normalize_ty(&object)?;
                    let member = self.normalize_ty(&member)?;

                    if let AstType::Object(object_name, generics) = &object {
                        let expected_member = InstantiateAdapter::instantiate_object_member(
                            &*self.analyzed_file,
                            object_name,
                            generics,
                            &member_name,
                        )?;
                        self.unify(&expected_member, &member)?;
                    } else if let AstType::Infer(..) = &object {
                        self.delayed_unifications
                            .push(TyckDelayedObjective::ObjectAccess(
                                object,
                                member_name,
                                member,
                            ));
                    } else {
                        Self::error()?;
                    }
                }
                TyckDelayedObjective::Object(object) => {
                    let object = self.normalize_ty(&object)?;

                    if let AstType::Object(..) = &object {
                        /* Okay! That's all we really need to guarantee. */
                    } else if let AstType::Infer(..) = &object {
                        self.delayed_unifications
                            .push(TyckDelayedObjective::Object(object));
                    } else {
                        Self::error()?;
                    }
                }
            }
        }

        Ok(())
    }

    pub fn normalize_ty(&self, t: &AstType) -> PResult<AstType> {
        t.clone().visit(&mut Normalize(self))
    }

    fn normalize_trait_ty(&self, t: &AstTraitType) -> PResult<AstTraitType> {
        let name = t.0.clone();
        let tys = t.1.clone().visit(&mut Normalize(self))?;

        Ok(AstTraitType(name, tys))
    }

    fn normalize_objective(&self, t: &TyckObjective) -> PResult<TyckObjective> {
        let obj_ty = self.normalize_ty(&t.obj_ty)?;
        let trait_ty = self.normalize_trait_ty(&t.trait_ty)?;

        Ok(TyckObjective { obj_ty, trait_ty })
    }

    fn normalize_impl_signature(&self, i: &TyckImplSignature) -> PResult<TyckImplSignature> {
        let generics = i.generics.clone().visit(&mut Normalize(self))?;

        Ok(TyckImplSignature {
            impl_id: i.impl_id,
            generics,
        })
    }
}

struct Normalize<'a>(&'a TyckSolver);

impl<'a> Adapter for Normalize<'a> {
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
                    let instantiate = InstantiateAdapter::instantiate_associated_ty(
                        &*self.0.analyzed_file,
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
