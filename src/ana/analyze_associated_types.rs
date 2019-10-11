use crate::ana::represent::AnalyzedProgram;
use crate::ana::represent_visitor::{AnAdapter, DirtyAnalysisPass};
use crate::parser::ast::{AstImpl, AstTraitType, AstType, ModuleRef};
use crate::parser::ast_visitor::AstAdapter;
use crate::util::{IntoError, PResult, Visit};
use std::collections::HashMap;

pub struct AnalyzeAssociatedTypes {
    type_map: HashMap<String, ModuleRef>,
}

impl DirtyAnalysisPass for AnalyzeAssociatedTypes {
    fn new(a: &AnalyzedProgram) -> PResult<Self> {
        let mut type_map: HashMap<String, ModuleRef> = HashMap::new();

        for (name, trt) in &a.analyzed_traits {
            for ty in trt.associated_tys.keys() {
                if type_map.contains_key(ty) {
                    return PResult::error(format!(
                        "Duplicated associated typename `{}`: \
                         First found in `{}`, also found in `{}`.",
                        ty,
                        type_map[ty].full_name()?,
                        name.full_name()?
                    ));
                }

                type_map.insert(ty.clone(), name.clone());
            }
        }

        Ok(AnalyzeAssociatedTypes { type_map })
    }
}

impl AnAdapter for AnalyzeAssociatedTypes {}

impl AstAdapter for AnalyzeAssociatedTypes {
    fn enter_impl(&mut self, mut i: AstImpl) -> PResult<AstImpl> {
        i.impl_ty = i.impl_ty.visit(&mut DenyAssociatedTypes)?;

        Ok(i)
    }

    fn enter_type(&mut self, t: AstType) -> PResult<AstType> {
        if let AstType::AssociatedType {
            obj_ty,
            trait_ty: None,
            name,
        } = t
        {
            if self.type_map.contains_key(&name) {
                let trait_name = self.type_map[&name].clone();

                Ok(AstType::AssociatedType {
                    obj_ty,
                    trait_ty: Some(AstTraitType(trait_name, vec![])),
                    name,
                })
            } else {
                return PResult::error(format!("No such associated typename `{}`.", name));
            }
        } else {
            Ok(t)
        }
    }
}

/* --- --- --- --- --- --- --- --- --- --- --- --- */
/* --- --- --- --- Helper adapter  --- --- --- --- */
/* --- --- --- --- --- --- --- --- --- --- --- --- */

struct DenyAssociatedTypes;

impl AstAdapter for DenyAssociatedTypes {
    fn enter_type(&mut self, t: AstType) -> PResult<AstType> {
        if let AstType::AssociatedType { .. } = &t {
            return PResult::error(format!("No associated types allowed in this context"));
        }

        Ok(t)
    }
}
