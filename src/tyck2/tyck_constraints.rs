use crate::ana::represent::*;
use crate::parser::ast::*;
use crate::parser::ast_visitor::AstAdapter;
use crate::tyck2::tyck_instantiate::GenericsInstantiator;
use crate::util::{PError, PResult, Visit};
use std::collections::{BTreeMap, HashMap};

pub struct TyckConstraintAssumptionAdapter {
    pub analyzed_program: AnalyzedProgram,
    pub dummy_impls: Vec<AnImplData>,

    /// Dummy type that I use to populate the self_instantiate.
    /// This is necessary for self-consistency checking.
    self_ty: Option<AstType>,
}

impl TyckConstraintAssumptionAdapter {
    pub fn new(analyzed_program: AnalyzedProgram) -> TyckConstraintAssumptionAdapter {
        TyckConstraintAssumptionAdapter {
            analyzed_program,
            dummy_impls: Vec::new(),
            self_ty: None,
        }
    }

    pub fn assume(&mut self, ty: &AstType, trt: &AstTraitTypeWithAssocs) -> PResult<AnImplData> {
        println!("Assuming {} :- {}", ty, trt);

        let trt_data = &self.analyzed_program.analyzed_traits[&trt.trt.name];
        let impl_id = AstImpl::new_id();

        let mut associated_tys = HashMap::new();
        for (name, assoc_ty) in trt_data.associated_tys.clone() {
            if &name == "Self" {
                panic!("ICE: this should never happen.");
            } else {
                let mut instantiate =
                    GenericsInstantiator::from_trait(&self.analyzed_program, &trt.trt)?;

                let dummy_ty = if let Some(ty) = trt.assoc_bindings.get(&name) {
                    ty.clone()
                } else {
                    AstType::dummy()
                };

                for c in &assoc_ty.restrictions {
                    // Alas, this means that we might have assumption bounds that are literally unprovable.
                    self.assume(&dummy_ty, &c.clone().visit(&mut instantiate)?)?;
                }

                println!(
                    "Associated type <{} as {}>::{} = {}",
                    ty, trt, name, dummy_ty
                );
                associated_tys.insert(name.clone(), dummy_ty);
            }
        }

        let dummy = AnImplData {
            impl_id,
            generics: Vec::new(),
            methods: HashMap::new(),
            trait_ty: trt.trt.clone(),
            impl_ty: ty.clone(),
            restrictions: Vec::new(),
            associated_tys,
            is_dummy: true,
        };

        self.dummy_impls.push(dummy.clone());

        self.analyzed_program
            .analyzed_impls
            .insert(impl_id, dummy.clone());

        Ok(dummy)
    }
}

impl AstAdapter for TyckConstraintAssumptionAdapter {
    fn enter_type(&mut self, t: AstType) -> PResult<AstType> {
        match t {
            AstType::SelfType => self
                .self_ty
                .clone()
                .ok_or_else(|| PError::new(format!("No self type in this context"))),
            t => Ok(t),
        }
    }

    fn enter_trait(&mut self, t: AstTrait) -> PResult<AstTrait> {
        // The trait's own restrictions will be assumed here.
        self.self_ty = Some(AstType::dummy());

        Ok(t)
    }

    fn exit_type_restriction(&mut self, t: AstTypeRestriction) -> PResult<AstTypeRestriction> {
        self.assume(&t.ty, &t.trt)?;

        Ok(t)
    }

    fn exit_trait(&mut self, t: AstTrait) -> PResult<AstTrait> {
        let self_trt = AstTraitTypeWithAssocs::new(
            t.module_ref.clone(),
            Dummifier::from_generics(&t.generics)?,
            BTreeMap::new(),
        );

        self.assume(&self.self_ty.clone().unwrap(), &self_trt)?;
        self.self_ty = None;

        Ok(t)
    }
}

/// An adapter that turns Generics into Dummy types, so we can typecheck generic functions.
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
    fn enter_type(&mut self, t: AstType) -> PResult<AstType> {
        if let AstType::GenericPlaceholder(id, name) = t {
            Ok(AstType::DummyGeneric(id, name))
        } else {
            Ok(t)
        }
    }
}
