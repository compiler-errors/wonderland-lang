use crate::ana::represent::AnalyzedProgram;
use crate::parser::ast::*;
use crate::parser::ast_visitor::*;
use crate::util::{PResult, Visit, ZipExact};
use std::collections::HashMap;

pub struct GenericsInstantiator(HashMap<GenericId, AstType>);

// TODO: I really need to give these signatures a long, hard look at. They're somewhat irregular.

impl GenericsInstantiator {
    pub fn from_signature(
        analyzed_program: &AnalyzedProgram,
        i: &AstImplSignature,
    ) -> PResult<GenericsInstantiator> {
        let impl_data = &analyzed_program.analyzed_impls[&i.impl_id];
        let mapping = ZipExact::zip_exact(&impl_data.generics, &i.generics, "generics")?
            .map(|(id, t)| (id.0, t.clone()))
            .collect();
        Ok(GenericsInstantiator(mapping))
    }

    pub fn from_generics(ids: &[AstGeneric], tys: &[AstType]) -> PResult<GenericsInstantiator> {
        let mapping = ZipExact::zip_exact(ids, tys, "generics")?
            .map(|(id, t)| (id.0, t.clone()))
            .collect();
        Ok(GenericsInstantiator(mapping))
    }

    pub fn from_trait(
        analyzed_program: &AnalyzedProgram,
        trt: &AstTraitType,
    ) -> PResult<GenericsInstantiator> {
        let trt_data = &analyzed_program.analyzed_traits[&trt.0];
        GenericsInstantiator::from_generics(&trt_data.generics, &trt.1)
    }

    pub fn instantiate_associated_ty(
        analyzed_program: &AnalyzedProgram,
        i: &AstImplSignature,
        name: &str,
    ) -> PResult<AstType> {
        let impl_data = &analyzed_program.analyzed_impls[&i.impl_id];
        let mut instantiate = GenericsInstantiator::from_signature(analyzed_program, i)?;

        impl_data.associated_tys[name]
            .clone()
            .visit(&mut instantiate)
    }

    pub fn instantiate_fn_signature(
        analyzed_program: &AnalyzedProgram,
        name: &ModuleRef,
        generics: &[AstType],
    ) -> PResult<(Vec<AstType>, AstType, Vec<AstTypeRestriction>)> {
        let fn_data = &analyzed_program.analyzed_functions[name];
        let mapping = ZipExact::zip_exact(&fn_data.generics, generics, "fn generics")?
            .map(|(id, t)| (id.0, t.clone()))
            .collect();
        let mut instantiate = GenericsInstantiator(mapping);

        Ok((
            fn_data.parameters.clone().visit(&mut instantiate)?,
            fn_data.return_type.clone().visit(&mut instantiate)?,
            fn_data.restrictions.clone().visit(&mut instantiate)?,
        ))
    }

    pub fn instantiate_trait_fn_signature(
        analyzed_program: &AnalyzedProgram,
        trait_name: &ModuleRef,
        trait_generics: &[AstType],
        fn_name: &str,
        fn_generics: &[AstType],
        self_ty: &AstType,
    ) -> PResult<(Vec<AstType>, AstType, Vec<AstTypeRestriction>)> {
        let trait_data = &analyzed_program.analyzed_traits[trait_name];
        let fn_data = &trait_data.methods[fn_name];
        let mapping = ZipExact::zip_exact(&trait_data.generics, trait_generics, "object generics")?
            .chain(ZipExact::zip_exact(
                &fn_data.generics,
                fn_generics,
                "fn generics",
            )?)
            .map(|(id, t)| (id.0, t.clone()))
            .collect();

        let mut instantiate_self = InstantiateSelfLocal(
            self_ty.clone(),
            AstTraitType(trait_name.clone(), trait_generics.to_vec()),
        );
        let mut instantiate = GenericsInstantiator(mapping);

        Ok((
            fn_data
                .parameters
                .clone()
                .visit(&mut instantiate)?
                .visit(&mut instantiate_self)?,
            fn_data
                .return_type
                .clone()
                .visit(&mut instantiate)?
                .visit(&mut instantiate_self)?,
            fn_data
                .restrictions
                .clone()
                .visit(&mut instantiate)?
                .visit(&mut instantiate_self)?,
        ))
    }

    pub fn instantiate_object_member(
        analyzed_program: &AnalyzedProgram,
        obj_name: &ModuleRef,
        generics: &[AstType],
        member_name: &str,
    ) -> PResult<AstType> {
        let obj_data = &analyzed_program.analyzed_objects[obj_name];
        let mapping = ZipExact::zip_exact(&obj_data.generics, generics, "object generics")?
            .map(|(id, t)| (id.0, t.clone()))
            .collect();
        let mut instantiate = GenericsInstantiator(mapping);

        obj_data.member_tys[member_name]
            .clone()
            .visit(&mut instantiate)
    }

    pub fn instantiate_restrictions(
        analyzed_program: &AnalyzedProgram,
        impl_ty: &AstType,
        trait_ty: &AstTraitType,
        restrictions: &[AstTypeRestriction],
    ) -> PResult<Vec<AstTypeRestriction>> {
        let mut instantiate = GenericsInstantiator::from_trait(analyzed_program, &trait_ty)?;
        let mut instantiate_self = InstantiateSelfLocal(impl_ty.clone(), trait_ty.clone());

        restrictions
            .to_vec()
            .visit(&mut instantiate)?
            .visit(&mut instantiate_self)
    }
}

impl AstAdapter for GenericsInstantiator {
    fn enter_type(&mut self, t: AstType) -> PResult<AstType> {
        match t {
            AstType::GenericPlaceholder(id, _) if self.0.contains_key(&id) => {
                self.0[&id].clone().visit(self)
            }
            t => Ok(t),
        }
    }
}

pub struct InstantiateSelfLocal(AstType, AstTraitType);

impl AstAdapter for InstantiateSelfLocal {
    fn enter_type(&mut self, t: AstType) -> PResult<AstType> {
        match t {
            AstType::SelfType => Ok(AstType::AssociatedType {
                obj_ty: Box::new(self.0.clone()),
                trait_ty: Some(self.1.clone()),
                name: "Self".into(),
            }),
            t => Ok(t),
        }
    }
}
