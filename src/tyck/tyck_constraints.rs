use crate::{
    ana::represent::*,
    ast::{visitor::AstAdapter, *},
    tyck::{tyck_instantiation::instantiate_associated_ty_restrictions, TYCK_MAX_DEPTH},
    util::{PError, PResult},
};
use std::collections::{BTreeMap, HashMap};

pub struct TyckGenericConstraintAssumptionAdapter {
    pub analyzed_program: AnalyzedProgram,
    pub dummy_impls: Vec<AnImplData>,

    /// Dummy type that I use to populate the self_instantiate.
    /// This is necessary for self-consistency checking.
    self_ty: Option<AstType>,
}

impl TyckGenericConstraintAssumptionAdapter {
    pub fn new(analyzed_program: AnalyzedProgram) -> TyckGenericConstraintAssumptionAdapter {
        TyckGenericConstraintAssumptionAdapter {
            analyzed_program,
            dummy_impls: vec![],
            self_ty: None,
        }
    }

    pub fn assume(
        &mut self,
        ty: &AstType,
        trt: &AstTraitTypeWithAssocs,
        depth: usize,
    ) -> PResult<AnImplData> {
        if depth > TYCK_MAX_DEPTH {
            return perror!("Typechecker overflow while assuming {} :- {}", ty, trt);
        }

        debug!("Assuming {} :- {}", ty, trt);

        let trt_data = self
            .analyzed_program
            .analyzed_traits
            .get_mut(&trt.trt.name)
            .unwrap();
        let impl_id = AstImpl::new_id();
        trt_data.impls.push(impl_id);

        let mut associated_tys = HashMap::new();
        for (name, _) in trt_data.associated_tys.clone() {
            if &name == "Self" {
                unreachable!(
                    "ICE: this should never happen. (I removed assoc-Self a long time ago...)"
                );
            };

            let assoc_ty = if let Some(ty) = trt.assoc_bindings.get(&name) {
                ty.clone()
            } else {
                AstType::dummy()
            };

            let restrictions = instantiate_associated_ty_restrictions(
                &self.analyzed_program,
                &trt.trt,
                &name,
                &ty,
            )?;

            for c in restrictions {
                // Alas, this means that we might have assumption bounds that are literally
                // unprovable.
                self.assume(&assoc_ty, &c, depth + 1)?;
            }

            debug!(
                "Associated type <{} as {}>::{} = {}",
                ty, trt, name, assoc_ty
            );

            associated_tys.insert(name.clone(), assoc_ty);
        }

        let dummy = AnImplData {
            impl_id,
            generics: vec![],
            methods: HashMap::new(),
            trait_ty: Some(trt.trt.clone()),
            impl_ty: ty.clone(),
            restrictions: vec![],
            associated_tys,
            kind: AnImplKind::Dummy,
        };

        self.dummy_impls.push(dummy.clone());

        self.analyzed_program
            .analyzed_impls
            .insert(impl_id, dummy.clone());

        Ok(dummy)
    }
}

impl AstAdapter for TyckGenericConstraintAssumptionAdapter {
    fn enter_ast_type(&mut self, t: AstType) -> PResult<AstType> {
        match t {
            AstType::SelfType => self
                .self_ty
                .clone()
                .ok_or_else(|| PError::new(format!("No self type in this context"))),
            t => Ok(t),
        }
    }

    fn enter_ast_trait(&mut self, t: AstTrait) -> PResult<AstTrait> {
        // The trait's own restrictions will be assumed here.
        self.self_ty = Some(AstType::dummy());

        Ok(t)
    }

    fn exit_ast_type_restriction(&mut self, t: AstTypeRestriction) -> PResult<AstTypeRestriction> {
        self.assume(&t.ty, &t.trt, 0)?;

        Ok(t)
    }

    fn exit_ast_trait(&mut self, t: AstTrait) -> PResult<AstTrait> {
        let self_trt = AstTraitTypeWithAssocs::new(
            t.module_ref.clone(),
            Dummifier::from_generics(&t.generics)?,
            BTreeMap::new(),
        );

        self.assume(&self.self_ty.clone().unwrap(), &self_trt, 0)?;
        self.self_ty = None;

        Ok(t)
    }
}

pub struct TyckDynamicAssumptionAdapter {
    pub analyzed_program: AnalyzedProgram,
    pub dummy_impls: Vec<AnImplData>,
}

impl TyckDynamicAssumptionAdapter {
    pub fn new(analyzed_program: AnalyzedProgram) -> TyckDynamicAssumptionAdapter {
        TyckDynamicAssumptionAdapter {
            analyzed_program,
            dummy_impls: vec![],
        }
    }

    pub fn assume(&mut self, ty: &AstType, trt: &AstTraitTypeWithAssocs) -> PResult<()> {
        debug!("Assuming {} :- {}", ty, trt);

        let trt_data = self
            .analyzed_program
            .analyzed_traits
            .get_mut(&trt.trt.name)
            .unwrap();
        let impl_id = AstImpl::new_id();
        trt_data.impls.push(impl_id);

        let mut associated_tys = HashMap::new();
        for (name, _) in trt_data.associated_tys.clone() {
            if &name == "Self" {
                unreachable!(
                    "ICE: this should never happen. (I removed assoc-Self a long time ago...)"
                );
            };

            let assoc_ty = if let Some(ty) = trt.assoc_bindings.get(&name) {
                ty.clone()
            } else {
                unreachable!(
                    "ICE: Expected dynamic trait to have all of its associated types elaborated."
                )
            };

            debug!(
                "Associated type <{} as {}>::{} = {}",
                ty, trt, name, assoc_ty
            );

            associated_tys.insert(name.clone(), assoc_ty);
        }

        let dummy = AnImplData {
            impl_id,
            generics: vec![],
            methods: HashMap::new(),
            trait_ty: Some(trt.trt.clone()),
            impl_ty: ty.clone(),
            restrictions: vec![],
            associated_tys,
            kind: AnImplKind::DynamicDispatch,
        };

        self.dummy_impls.push(dummy.clone());

        self.analyzed_program.analyzed_impls.insert(impl_id, dummy);

        Ok(())
    }

    fn add_dyn_into(&mut self, ty: &AstType) -> PResult<()> {
        // impl<_T> Into<Dyn<Trt1 + Trt2>> for _T where _T: Trt1 + Trt2

        if let AstType::DynamicType { trait_tys } = ty {
            let into_trait = self
                .analyzed_program
                .construct_trt_ref("operators::lang::Into")?;

            let trt_data = self
                .analyzed_program
                .analyzed_traits
                .get_mut(&into_trait)
                .unwrap();
            let impl_id = AstImpl::new_id();
            trt_data.impls.push(impl_id);

            let generic = AstGeneric::new("T".to_string());
            let restrictions = trait_tys
                .iter()
                .map(|trt| AstTypeRestriction::new(generic.clone().into(), trt.clone()))
                .collect();

            let dummy = AnImplData {
                impl_id,
                generics: vec![generic.clone()],
                methods: HashMap::new(),
                trait_ty: Some(AstTraitType::new(into_trait.clone(), vec![ty.clone()])),
                impl_ty: generic.clone().into(),
                restrictions,
                associated_tys: HashMap::new(),

                kind: AnImplKind::DynamicCoersion,
            };

            self.dummy_impls.push(dummy.clone());
            self.analyzed_program.analyzed_impls.insert(impl_id, dummy);

            Ok(())
        } else {
            unreachable!()
        }
    }

    fn add_dyn_downcast(&mut self, ty: &AstType) -> PResult<()> {
        // impl<_T> TryDowncast<_T> for Dyn<Trt1 + Trt2> where _T: Trt1 + Trt2

        if let AstType::DynamicType { .. } = ty {
            let downcast_trait = self.analyzed_program.construct_trt_ref("any::Downcast")?;

            let trt_data = self
                .analyzed_program
                .analyzed_traits
                .get_mut(&downcast_trait)
                .unwrap();
            let impl_id = AstImpl::new_id();
            trt_data.impls.push(impl_id);

            let dummy = AnImplData {
                impl_id,
                generics: vec![],
                methods: HashMap::new(),
                trait_ty: Some(AstTraitType::new(downcast_trait.clone(), vec![])),
                impl_ty: ty.clone(),
                restrictions: vec![],
                associated_tys: HashMap::new(),
                kind: AnImplKind::DynamicDowncast,
            };

            self.dummy_impls.push(dummy.clone());
            self.analyzed_program.analyzed_impls.insert(impl_id, dummy);

            Ok(())
        } else {
            unreachable!()
        }
    }
}

impl AstAdapter for TyckDynamicAssumptionAdapter {
    fn enter_ast_type(&mut self, t: AstType) -> PResult<AstType> {
        match &t {
            AstType::DynamicType { trait_tys } => {
                self.add_dyn_into(&t)?;
                self.add_dyn_downcast(&t)?;

                for trait_ty in trait_tys {
                    self.assume(&t, trait_ty)?;
                }
            },
            _ => {},
        }

        Ok(t)
    }
}

/// An adapter that turns Generics into Dummy types, so we can typecheck generic
/// functions.
pub struct Dummifier;

impl Dummifier {
    pub fn from_generics(generics: &[AstGeneric]) -> PResult<Vec<AstType>> {
        let dummies = generics
            .iter()
            .map(|AstGeneric(id, name)| AstType::DummyGeneric(*id, name.clone()))
            .collect();

        Ok(dummies)
    }
}

impl AstAdapter for Dummifier {
    fn enter_ast_type(&mut self, t: AstType) -> PResult<AstType> {
        if let AstType::GenericPlaceholder(id, name) = t {
            Ok(AstType::DummyGeneric(id, name))
        } else {
            Ok(t)
        }
    }
}
