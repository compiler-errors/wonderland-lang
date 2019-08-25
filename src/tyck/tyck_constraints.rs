use crate::analyze::represent::{AnImplData, AnalyzedFile};
use crate::parser::ast::*;
use crate::parser::ast_visitor::{Adapter, Visit};
use crate::util::result::{Expect, PResult};
use crate::util::Span;
use std::collections::HashMap;
use crate::tyck::GenericsInstantiator;

pub struct TyckConstraintAssumptionAdapter {
    pub file: AnalyzedFile,
    pub dummy_impls: Vec<AnImplData>,

    /// Dummy type that I use to populate the self_instantiate.
    /// This is necessary for self-consistency checking.
    self_ty: Option<AstType>,
}

impl TyckConstraintAssumptionAdapter {
    pub fn new(file: AnalyzedFile) -> TyckConstraintAssumptionAdapter {
        TyckConstraintAssumptionAdapter {
            file,
            dummy_impls: Vec::new(),
            self_ty: None,
        }
    }

    pub fn assume(&mut self, ty: &AstType, trt: &AstTraitType) -> PResult<AnImplData> {
        println!("Assuming {:?} :- {:?}", ty, trt);

        let trt_data =
            self.file
                .analyzed_traits
                .get(&trt.0)
                .expected(Span::new(0, 0), "trait", &trt.0)?;
        let impl_id = AstImpl::new_id();

        let mut associated_tys = HashMap::new();
        for (name, assoc_ty) in trt_data.associated_tys.clone() {
            if &name == "Self" {
                associated_tys.insert("Self".to_string(), ty.clone());
            } else {
                let mut instantiate = GenericsInstantiator::from_trait(&self.file, trt)?;
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

        self.file.analyzed_impls.insert(impl_id, dummy.clone());

        Ok(dummy)
    }
}

impl Adapter for TyckConstraintAssumptionAdapter {
    fn enter_type(&mut self, t: AstType) -> PResult<AstType> {
        match t {
            AstType::SelfType => self
                .self_ty
                .clone()
                .expected(Span::new(0, 0), "type", "self"),
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
        let self_trt = AstTraitType(t.name.clone(), Dummifier::from_generics(&t.generics)?);
        let assumed_impl_data = self.assume(&self.self_ty.clone().unwrap(), &self_trt)?;
        self.self_ty = None;

        Ok(t)
    }
}

/// An adapter that turns Generics into Dummy types, so we can typecheck generic functions.
pub struct Dummifier;

impl Dummifier {
    pub fn from_generics(generics: &Vec<AstGeneric>) -> PResult<Vec<AstType>> {
        let dummies = generics
            .clone()
            .into_iter()
            .map(|AstGeneric(id, name)| AstType::DummyGeneric(id, name))
            .collect();

        Ok(dummies)
    }
}

impl Adapter for Dummifier {
    fn enter_type(&mut self, t: AstType) -> PResult<AstType> {
        if let AstType::GenericPlaceholder(id, name) = t {
            Ok(AstType::DummyGeneric(id, name))
        } else {
            Ok(t)
        }
    }
}
