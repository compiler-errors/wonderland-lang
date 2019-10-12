use crate::ana::represent::*;
use crate::parser::ast::*;
use crate::parser::ast_visitor::AstAdapter;
use crate::tyck::tyck_instantiate::GenericsInstantiator;
use crate::util::{Expect, PResult, Span, Visit};
use std::collections::HashMap;

pub struct TyckConstraintAssumptionAdapter {
    pub analyzed_program: AnalyzedProgram,
    pub dummy_impls: Vec<AnImplData>,

    /// Dummy type that I use to populate the self_instantiate.
    /// This is necessary for self-consistency checking.
    self_ty: Option<AstType>,
}

impl TyckConstraintAssumptionAdapter {
    pub fn new(program: AnalyzedProgram) -> TyckConstraintAssumptionAdapter {
        TyckConstraintAssumptionAdapter {
            analyzed_program: program,
            dummy_impls: Vec::new(),
            self_ty: None,
        }
    }

    pub fn assume(&mut self, ty: &AstType, trt: &AstTraitType) -> PResult<AnImplData> {
        println!("; Assuming {:?} :- {:?}", ty, trt);

        let trt_data = self
            .analyzed_program
            .analyzed_traits
            .get(&trt.0)
            .is_expected(Span::none(), "trait", &trt.0.full_name()?)?;
        let impl_id = AstImpl::new_id();

        let mut associated_tys = HashMap::new();
        for (name, assoc_ty) in trt_data.associated_tys.clone() {
            if &name == "Self" {
                associated_tys.insert("Self".into(), ty.clone());
            } else {
                let mut instantiate =
                    GenericsInstantiator::from_trait(&self.analyzed_program, trt)?;
                let dummy = AstType::dummy();

                for c in &assoc_ty.restrictions {
                    // Alas, this means that we might have assumption bounds that are literally unprovable.
                    self.assume(&dummy, &c.clone().visit(&mut instantiate)?)?;
                }

                associated_tys.insert(name.clone(), dummy);
            }
        }

        let dummy = AnImplData {
            impl_id,
            generics: Vec::new(),
            methods: HashMap::new(),
            trait_ty: trt.clone(),
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
                .is_expected(Span::none(), "type", "self"),
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
        let self_trt = AstTraitType(t.module_ref.clone(), Dummifier::from_generics(&t.generics)?);
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
