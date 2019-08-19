use crate::analyze::represent::AnalyzedFile;
use crate::parser::ast::*;
use crate::parser::ast_visitor::*;
use crate::tyck::TyckImplSignature;
use crate::util::result::PResult;
use crate::util::ZipExact;
use std::collections::HashMap;

pub struct InstantiateAdapter(pub HashMap<GenericId, AstType>);

impl InstantiateAdapter {
    pub fn from_signature(
        analyzed_file: &AnalyzedFile,
        i: &TyckImplSignature,
    ) -> PResult<InstantiateAdapter> {
        let impl_data = &analyzed_file.analyzed_impls[&i.impl_id];
        let mapping = ZipExact::zip_exact(&impl_data.generics, &i.generics, "generics")?
            .map(|(&id, t)| (id, t.clone()))
            .collect();
        Ok(InstantiateAdapter(mapping))
    }

    pub fn instantiate_associated_ty(
        analyzed_file: &AnalyzedFile,
        i: &TyckImplSignature,
        name: &String,
    ) -> PResult<AstType> {
        let impl_data = &analyzed_file.analyzed_impls[&i.impl_id];
        let mut instantiate = InstantiateAdapter::from_signature(analyzed_file, i)?;

        impl_data.associated_tys[name]
            .clone()
            .visit(&mut instantiate)
    }

    pub fn instantiate_fn_signature(
        analyzed_file: &AnalyzedFile,
        name: &String,
        generics: &Vec<AstType>,
    ) -> PResult<(Vec<AstType>, AstType)> {
        let fn_data = &analyzed_file.analyzed_functions[name];
        let mapping = ZipExact::zip_exact(&fn_data.generics, generics, "fn generics")?
            .map(|(&id, t)| (id, t.clone()))
            .collect();
        let mut instantiate = InstantiateAdapter(mapping);

        Ok((
            fn_data.parameters.clone().visit(&mut instantiate)?,
            fn_data.return_type.clone().visit(&mut instantiate)?,
        ))
    }

    pub fn instantiate_object_fn_signature(
        analyzed_file: &AnalyzedFile,
        associated_trait: &AstTraitType,
        fn_name: &String,
        fn_generics: &Vec<AstType>,
    ) -> PResult<(Vec<AstType>, AstType)> {
        let trait_data = &analyzed_file.analyzed_traits[&associated_trait.0];
        let fn_data = &trait_data.methods[fn_name];
        let mapping =
            ZipExact::zip_exact(&trait_data.generics, &associated_trait.1, "object generics")?
                .chain(ZipExact::zip_exact(
                    &fn_data.generics,
                    fn_generics,
                    "fn generics",
                )?)
                .map(|(&id, t)| (id, t.clone()))
                .collect();
        let mut instantiate = InstantiateAdapter(mapping);

        Ok((
            fn_data.parameters.clone().visit(&mut instantiate)?,
            fn_data.return_type.clone().visit(&mut instantiate)?,
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
        let mut instantiate = InstantiateAdapter(mapping);

        obj_data.members[obj_name].clone().visit(&mut instantiate)
    }
}

impl Adapter for InstantiateAdapter {
    fn enter_type(&mut self, t: AstType) -> PResult<AstType> {
        match t {
            AstType::GenericPlaceholder(id, _) => self.0[&id].clone().visit(self),
            t => Ok(t),
        }
    }
}
