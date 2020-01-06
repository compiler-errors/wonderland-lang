use crate::ana::represent::{AnImplData, AnalyzedProgram};
use crate::parser::ast::{
    AstImplSignature, AstModule, AstProgram, AstTraitType, AstTraitTypeWithAssocs, AstType,
    AstTypeRestriction, ImplId, InferId, ModuleRef,
};
use crate::parser::parse_program;
use crate::util::{Comment, FileRegistry, PResult, Visit};
use std::collections::{BTreeMap, HashMap, HashSet};
use std::rc::Rc;
const DEPTH: usize = 64;
mod tyck_instantiate;
pub fn typecheck(analyzed_program: &AnalyzedProgram, parsed_program: &AstProgram) -> PResult<()> {
    /*let mut constraint_assumptions = TyckConstraintAssumptionAdapter::new(analyzed_program.clone());

    let parsed_program = parsed_program
        .clone()
        //.visit(&mut Dummifier)?
        .visit(&mut constraint_assumptions)?;

    let analyzed_program = Rc::new(constraint_assumptions.analyzed_program);

    Ok(())*/

    todo!()
}

#[derive(Eq, PartialEq, Debug, Hash)]
enum TyckObjective {
    Implements {
        ty: AstType,
        trt: AstTraitTypeWithAssocs,
    },
    WellFormed {
        ty: AstType,
    },
    AssociatedType {
        ty: AstType,
        trt: Option<AstTraitTypeWithAssocs>,
        name: String,
    },
    Method {
        ty: AstType,
        trt: Option<AstTraitTypeWithAssocs>,
        name: String,
    },
}

#[derive(Clone, Debug)]
enum TyckResult {
    Ok(Option<AstImplSignature>),
    Ambiguous(String),
    Err(String),
}

struct TypeChecker {
    program: Rc<AnalyzedProgram>,
    inferences: HashMap<InferId, AstType>,
    failed_objectives: HashSet<TyckObjective>,
    successful_objectives: HashMap<TyckObjective, AstImplSignature>,

    inference_stack: Vec<TypeCheckerEpoch>,
    changed: bool,
}

struct TypeCheckerEpoch {
    inferences: Vec<(InferId, Option<AstType>)>,
    failed_objectives: HashSet<TyckObjective>,
    successful_objectives: HashMap<TyckObjective, AstImplSignature>,
}

impl TypeChecker {
    fn assume(&mut self) {
        self.inference_stack.push(TypeCheckerEpoch {
            inferences: Vec::new(),
            failed_objectives: HashSet::new(),
            successful_objectives: HashMap::new(),
        });
    }

    fn commit(&mut self) {
        let epoch = self.inference_stack.pop().unwrap();

        if let Some(top) = self.inference_stack.last_mut() {
            top.inferences.extend(epoch.inferences);
            top.failed_objectives.extend(epoch.failed_objectives);
            top.successful_objectives.extend(epoch.successful_objectives);
        } else {
            self.changed = !epoch.inferences.is_empty() || !epoch.failed_objectives.is_empty() || !epoch.successful_objectives.is_empty();
        }

        self.normalize_the_world();
    }

    fn rollback(&mut self) {
        let epoch = self.inference_stack.pop().unwrap();

        for (k, v) in epoch.inferences.into_iter().rev() {
            if let Some(v) = v {
                self.inferences.insert(k, v);
            } else {
                self.inferences.remove(&k);
            }
        }

        for o in epoch.failed_objectives {
            self.failed_objectives.remove(&o);
        }

        for o in epoch.successful_objectives.keys() {
            self.successful_objectives.remove(o);
        }
    }

    fn prove(&mut self, objective: TyckObjective) -> TyckResult {
        let objective = self.normalize_objective(objective)?;
        match self.conclusions.get(&objective) {
            None | Some(TyckResult::Ambiguous(_)) => { /* Keep going, try again. */ }
            Some(r @ TyckResult::Ok(_)) => {
                return r.clone();
            }
            Some(r @ TyckResult::Err(_)) => {
                return r.clone();
            }
        }

        let result = match &objective {
            TyckObjective::Implements { ty, trt } => self.internal_prove_objective(&objective, ty, trt),
            TyckObjective::AssociatedType {
                ty,
                trt: Some(trt),
                name,
            } => self.internal_prove_objective(&objective, ty, trt),
            TyckObjective::AssociatedType {
                ty,
                trt: None,
                name,
            } => {
                let mut trait_candidates = Vec::new();

                for t in &self.program.associated_types_to_traits[name] {
                    trait_candidates.push(self.freshen_trait(t));
                }

                self.internal_prove_implements_one(&objective, ty, &trait_candidates)
            }
            TyckObjective::Method {
                ty,
                trt: Some(trt),
                name,
            } => self.internal_prove_objective(&objective, ty, trt),
            TyckObjective::Method {
                ty,
                trt: None,
                name,
            } => {
                let mut trait_candidates = Vec::new();

                for t in &self.program.methods_to_traits[name] {
                    trait_candidates.push(self.freshen_trait(t));
                }

                self.internal_prove_implements_one(&objective, ty, &trait_candidates)
            }
            TyckObjective::WellFormed { ty } => match ty {
                AstType::Object(name, generics) => self.internal_prove_objectives(
                    &self.instantiate_object_constraints(name, generics),
                ),
                AstType::Enum(name, generics) => self.internal_prove_objectives(
                    &self.instantiate_enum_constraints(name, generics),
                ),
                _ => {} //AstType::AssociatedType { .. } => TyckResult::Ambiguous()
            },
        };
    }

    fn internal_prove_objective(
        &mut self,
        objective: &TyckObjective,
        ty: &AstType,
        trt: &AstTraitTypeWithAssocs,
    ) -> TyckResult {
        if self.inference_stack.len() > DEPTH {
            todo!("die!");
        }

        let mut proof_traits = Vec::new();
        let mut proof_impls = Vec::new();

        for imp in self.program.analyzed_traits[&trt.name].impls {
            self.assume();
            proof_traits.push(trt);
            proof_impls.push(self.internal_apply(ty, trt, imp, true));
            self.rollback();
        }

        self.internal_disambiguate_commit(ty, objective, proof_traits, proof_impls)
    }

    fn internal_prove_objectives(
        &mut self,
        objectives: &[AstTypeRestriction],
    ) -> TyckResult {
        if self.inference_stack.len() > DEPTH {
            todo!("die!");
        }

        self.assume();

        for AstTypeRestriction { ty, trt } in objectives {
            match self.internal_prove_objective(&TyckObjective::Implements { ty: ty.clone(), trt: trt.clone() }, ty, trt) {
                TyckResult::Ok(_) => { /* Do nothing! */ }
                e @ TyckResult::Ambiguous(_) | e @ TyckResult::Error(_) => {
                    self.rollback();
                    return e;
                }
            }
        }

        self.commit();
        TyckResult::Ok(None)
    }

    fn internal_prove_implements_one(
        &mut self,
        objective: &TyckObjective,
        ty: &AstType,
        traits: &[AstTraitTypeWithAssocs],
    ) -> TyckResult {
        if self.inference_stack.len() > DEPTH {
            todo!("die!");
        }

        let mut proof_traits = Vec::new();
        let mut proof_impls = Vec::new();

        for trt in traits {
            for imp in self.program.analyzed_traits[&trt.name].impls {
                self.assume();
                proof_traits.push(trt);
                proof_impls.push(self.internal_apply(ty, trt, imp, true));
                self.rollback();
            }
        }

        self.internal_disambiguate_commit(ty, objective, proof_traits, proof_impls)
    }

    fn internal_disambiguate_commit(
        &mut self,
        ty: &AstType,
        objective: &TyckObjective,
        proof_traits: Vec<&AstTraitTypeWithAssocs>,
        proof_impls: Vec<TyckResult>,
    ) -> TyckResult {
        let mut yes = None;
        let mut ambiguous = None;

        for (trt, r) in Iterator::zip(proof_traits.into_iter(), proof_impls.into_iter()) {
            match r {
                a @ TyckResult::Ambiguous(_) => {
                    ambiguous = Some(a);
                    break;
                }
                TyckResult::Ok(_) if yes.is_some() => todo!("Error"),
                TyckResult::Ok(imp) => {
                    yes = Some((trt, imp.unwrap()));
                }
            }
        }

        if let Some(ambiguous) = ambiguous {
            ambiguous
        } else if let Some((trt, (imp, _))) = yes {
            // I know this is kinda gross, but RE-apply the impl that ended up working.
            // NOTE: It would be nice instead of committing and rolling back, if we end up
            // successful, then just pop that Epoch and then just stick it back on top and commit
            // it later...
            let result = self.internal_apply(ty, trt, imp, false);
            self.generalize_and_memoize(objective, &result);
            result
        } else {
            todo!("Error about no impl for this goal")
        }
    }

    fn internal_apply(
        &mut self,
        ty: &AstType,
        trt: &AstTraitTypeWithAssocs,
        impl_id: ImplId,
        probe: bool,
    ) -> TyckResult {
        let generics: Vec<_> = self.program.analyzed_impls[&impl_id]
            .generics
            .iter()
            .map(|_| AstType::infer())
            .collect();

        // Try unifying the trait type...
        let result = self.left_unify_trait(self.instantiate_impl_trait(impl_id, &generics), trt);
        match result {
            TyckResult::Ok(_) => {}
            r @ TyckResult::Ambiguous(_) | r @ TyckResult::Error(_) => {
                return r;
            }
        }

        // Refresh, and unify the type...
        let generics = self.normalize_tys(generics);
        let result = self.left_unify_type(self.instantiate_impl_ty(impl_id, &generics), ty);
        match result {
            TyckResult::Ok(_) => {}
            r @ TyckResult::Ambiguous(_) | r @ TyckResult::Error(_) => {
                return r;
            }
        }

        // Refresh, and THEN try applying all of the constraints...
        let generics = self.normalize_tys(generics);
        let result = self.internal_prove_objectives(
            &self.instantiate_impl_constraints(impl_id, generics),
        );

        // TODO: This should NOT happen in the application phase :-(
        // Probe is there to make sure we disambiguate between "try" and "commit" phases.
        if !probe {
            // Refresh, and THEN try applying all of the associated types...
            let generics = self.normalize_tys(generics);
            let associated_types = self.instantiate_impl_associated_types(impl_id, generics);

            for (name, ty) in trt.assoc_bindings {
                match self.full_unify(&associated_types[name], &ty) {
                    TyckResult::Ambiguous(_) => unreachable!("Ambiguity is not possible in full unification"),
                    TyckResult::Ok(_) => {}
                    r @ TyckResult::Error(_) => {
                        return r;
                    }
                }
            }
        }

        // Finally return the result with the generics (which should reflect the info we have deduced)
        let generics = self.normalize_tys(generics);
        let result = match result {
            TyckResult::Ok(_) => TyckResult::Ok(Some(AstImplSignature { impl_id, generics })),
            r @ TyckResult::Ambiguous(_) | r => r,
        };

        result
    }

    fn freshen_trait(&self, trt: &ModuleRef) -> AstTraitTypeWithAssocs {
        AstTraitTypeWithAssocs::new(
            trt.clone(),
            self.program.analyzed_traits[trt]
                .generics
                .iter()
                .map(|_| AstType::infer())
                .collect(),
            BTreeMap::new(),
        )
    }

    fn instantiate_object_constraints(
        &mut self,
        name: &ModuleRef,
        generics: &[AstType],
    ) -> Vec<AstTypeRestriction> {
        todo!()
    }

    fn instantiate_enum_constraints(
        &mut self,
        name: &ModuleRef,
        generics: &[AstType],
    ) -> Vec<AstTypeRestriction> {
        todo!()
    }
}
