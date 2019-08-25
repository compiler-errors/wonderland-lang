use crate::analyze::represent::AnalyzedFile;
use crate::parser::ast::*;
use crate::parser::ast_visitor::*;
use crate::tyck::TyckImplSignature;
use crate::util::result::PResult;
use crate::util::ZipExact;
use std::collections::HashMap;

pub struct Instantiate(HashMap<GenericId, AstType>);

// TODO: I really need to give these signatures a long, hard look at. They're somewhat irregular.

impl Instantiate {
    pub fn from_signature(
        analyzed_file: &AnalyzedFile,
        i: &TyckImplSignature,
    ) -> PResult<Instantiate> {
        let impl_data = &analyzed_file.analyzed_impls[&i.impl_id];
        let mapping = ZipExact::zip_exact(&impl_data.generics, &i.generics, "generics")?
            .map(|(&id, t)| (id, t.clone()))
            .collect();
        Ok(Instantiate(mapping))
    }

    pub fn from_generics(ids: &Vec<GenericId>, tys: &Vec<AstType>) -> PResult<Instantiate> {
        let mapping = ZipExact::zip_exact(ids, tys, "generics")?
            .map(|(&id, t)| (id, t.clone()))
            .collect();
        Ok(Instantiate(mapping))
    }

    pub fn instantiate_associated_ty(
        analyzed_file: &AnalyzedFile,
        i: &TyckImplSignature,
        name: &String,
    ) -> PResult<AstType> {
        let impl_data = &analyzed_file.analyzed_impls[&i.impl_id];
        let mut instantiate = Instantiate::from_signature(analyzed_file, i)?;

        impl_data.associated_tys[name]
            .clone()
            .visit(&mut instantiate)
    }

    pub fn instantiate_fn_signature(
        analyzed_file: &AnalyzedFile,
        name: &String,
        generics: &Vec<AstType>,
    ) -> PResult<(Vec<AstType>, AstType, Vec<AstTypeRestriction>)> {
        let fn_data = &analyzed_file.analyzed_functions[name];
        let mapping = ZipExact::zip_exact(&fn_data.generics, generics, "fn generics")?
            .map(|(&id, t)| (id, t.clone()))
            .collect();
        let mut instantiate = Instantiate(mapping);

        Ok((
            fn_data.parameters.clone().visit(&mut instantiate)?,
            fn_data.return_type.clone().visit(&mut instantiate)?,
            fn_data.restrictions.clone().visit(&mut instantiate)?,
        ))
    }

    pub fn instantiate_trait_fn_signature(
        analyzed_file: &AnalyzedFile,
        trait_name: &String,
        trait_generics: &Vec<AstType>,
        fn_name: &String,
        fn_generics: &Vec<AstType>,
        self_ty: &AstType,
    ) -> PResult<(Vec<AstType>, AstType, Vec<AstTypeRestriction>)> {
        let trait_data = &analyzed_file.analyzed_traits[trait_name];
        let fn_data = &trait_data.methods[fn_name];
        let mapping = ZipExact::zip_exact(&trait_data.generics, trait_generics, "object generics")?
            .chain(ZipExact::zip_exact(
                &fn_data.generics,
                fn_generics,
                "fn generics",
            )?)
            .map(|(&id, t)| (id, t.clone()))
            .collect();

        let mut instantiate_self = InstantiateSelfLocal(
            self_ty.clone(),
            AstTraitType(trait_name.clone(), trait_generics.clone()),
        );
        let mut instantiate = Instantiate(mapping);

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
        analyzed_file: &AnalyzedFile,
        obj_name: &String,
        generics: &Vec<AstType>,
        member_name: &String,
    ) -> PResult<AstType> {
        let obj_data = &analyzed_file.analyzed_objects[obj_name];
        let mapping = ZipExact::zip_exact(&obj_data.generics, generics, "object generics")?
            .map(|(&id, t)| (id, t.clone()))
            .collect();
        let mut instantiate = Instantiate(mapping);

        obj_data.members[member_name]
            .clone()
            .visit(&mut instantiate)
    }

    pub fn instantiate_restrictions(
        analyzed_file: &AnalyzedFile,
        impl_ty: &AstType,
        trait_ty: &AstTraitType,
        restrictions: &Vec<AstTypeRestriction>,
    ) -> PResult<Vec<AstTypeRestriction>> {
        let trait_data = &analyzed_file.analyzed_traits[&trait_ty.0];

        let mut instantiate = Instantiate::from_generics(&trait_data.generics, &trait_ty.1)?;
        let mut instantiate_self = InstantiateSelfLocal(impl_ty.clone(), trait_ty.clone());

        restrictions
            .clone()
            .visit(&mut instantiate)?
            .visit(&mut instantiate_self)
    }
}

impl Adapter for Instantiate {
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

impl Adapter for InstantiateSelfLocal {
    fn enter_type(&mut self, t: AstType) -> PResult<AstType> {
        match t {
            AstType::SelfType => Ok(AstType::AssociatedType {
                obj_ty: Box::new(self.0.clone()),
                trait_ty: Some(self.1.clone()),
                name: "Self".to_string(),
            }),
            t => Ok(t),
        }
    }
}
