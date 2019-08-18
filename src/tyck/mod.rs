use crate::parser::*;
use crate::util::result::*;
use std::collections::{HashMap, VecDeque};

type TResult<T> = Result<T, ()>;

#[derive(Debug, Clone)]
struct TypecheckUniverse {
    inferences: HashMap<InferId, AstType>,
    impl_choices: HashMap<TypecheckObjective, ImplId>,
    objectives: VecDeque<TypecheckObjective>,
    delayed_unifications: VecDeque<TypecheckDelayedUnification>,
}

/** An objective. Essentially captures the question:
 * Does obj_ty implement trait_ty (with given generics). */
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct TypecheckObjective {
    obj_ty: AstType,
    trait_ty: AstTraitType,
}

#[derive(Debug, Clone)]
struct TypecheckDelayedUnification;

struct TypecheckInstantiatedImpl {
    obj_ty: AstType,
    trait_ty: AstTraitType,
    constraints: Vec<AstTypeRestriction>,
}

impl TypecheckUniverse {
    fn solve(self) -> PResult<TypecheckUniverse> {
        let mut multiverse = VecDeque::new();
        multiverse.push_back(self);

        while let Some(universe) = multiverse.pop_front() {
            if universe.delayed_unifications.is_empty() && universe.objectives.is_empty() {
                return Ok(universe);
            }

            if let Ok(children) = universe.elaborate_all() {
                for c in children {
                    multiverse.push_back(c);
                }
            }

            /* Otherwise, just throw it away. */
        }

        Err(unimplemented!())
    }

    fn elaborate_all(mut self) -> TResult<Vec<TypecheckUniverse>> {
        let objective = self.objectives.pop_front().unwrap();

        if self.impl_choices.contains_key(&objective) {
            return Ok(vec![self]);
        }

        let mut new_universes = Vec::new();

        for instantiated_impl in self.get_impls(&objective.trait_ty)? {
            if let Ok(new_universe) = self.clone().elaborate(instantiated_impl, &objective) {
                new_universes.push(new_universe);
            }
        }

        Ok(new_universes)
    }

    fn elaborate(
        mut self,
        instantiated_impl: TypecheckInstantiatedImpl,
        objective: &TypecheckObjective,
    ) -> TResult<TypecheckUniverse> {
        let TypecheckInstantiatedImpl {
            obj_ty: impl_obj_ty,
            trait_ty: impl_trait_ty,
            constraints,
        } = instantiated_impl;

        self.unify(&objective.obj_ty, &impl_obj_ty)?;
        self.unify_traits(&objective.trait_ty, &impl_trait_ty)?;
        self.normalize()?;

        Ok(self)
    }

    fn get_impls(&self, objective: &AstTraitType) -> TResult<Vec<TypecheckInstantiatedImpl>> {
        unimplemented!()
    }

    fn unify(&mut self, lhs: &AstType, rhs: &AstType) -> TResult<()> {
        unimplemented!()
    }

    fn unify_traits(&mut self, lhs: &AstTraitType, rhs: &AstTraitType) -> TResult<()> {
        if lhs.0 == rhs.0 {
            assert_eq!(lhs.1.len(), rhs.1.len());

            for (l, r) in Iterator::zip(lhs.1.iter(), rhs.1.iter()) {
                self.unify(l, r)?;
            }

            Ok(())
        } else {
            Err(())
        }
    }

    fn normalize(&mut self) -> TResult<()> {
        // Normalize, then solve any delayed unifications.
        unimplemented!()
    }
}
