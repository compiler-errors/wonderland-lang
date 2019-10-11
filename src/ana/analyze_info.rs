use crate::ana::represent::{
    AnFunctionData, AnImplData, AnObjectData, AnTraitData, AnalyzedProgram,
};
use crate::parser::ast::*;
use crate::parser::ast_visitor::AstAdapter;
use crate::util::{Expect, PResult};
use std::collections::HashMap;

pub struct AnalyzeInfo {
    pub analyzed_program: AnalyzedProgram,
}

impl AnalyzeInfo {
    pub fn new() -> AnalyzeInfo {
        AnalyzeInfo {
            analyzed_program: AnalyzedProgram {
                variable_ids: HashMap::new(),
                analyzed_functions: HashMap::new(),
                analyzed_traits: HashMap::new(),
                analyzed_objects: HashMap::new(),
                analyzed_impls: HashMap::new(),
            },
        }
    }

    pub fn map_object_function(fun: &AstObjectFunction) -> AnFunctionData {
        AnFunctionData {
            name: None,
            generics: fun.generics.iter().map(|g| g.0).collect(),
            parameters: fun.parameter_list.iter().map(|p| p.ty.clone()).collect(),
            return_type: fun.return_type.clone(),
            restrictions: fun.restrictions.clone(),
        }
    }
}

impl AstAdapter for AnalyzeInfo {
    fn enter_module(&mut self, m: AstModule) -> PResult<AstModule> {
        for fun in m.functions.values() {
            let ana_fun = AnFunctionData {
                name: Some(fun.module_ref.clone()),
                generics: fun.generics.iter().map(|g| g.0).collect(),
                parameters: fun.parameter_list.iter().map(|p| p.ty.clone()).collect(),
                return_type: fun.return_type.clone(),
                restrictions: fun.restrictions.clone(),
            };

            self.analyzed_program
                .analyzed_functions
                .insert(fun.module_ref.clone(), ana_fun);
        }

        for obj in m.objects.values() {
            let ana_obj = AnObjectData {
                name: obj.module_ref.clone(),
                self_type: AstType::Object(
                    obj.module_ref.clone(),
                    obj.generics.iter().map(|g| g.clone().into()).collect(),
                ),
                generics: obj.generics.iter().map(|g| g.0).collect(),
                member_tys: obj
                    .members
                    .iter()
                    .map(|m| (m.name.clone(), m.member_type.clone()))
                    .collect(),
                member_indices: obj
                    .members
                    .iter()
                    .enumerate()
                    .map(|(i, m)| (m.name.clone(), i))
                    .collect(),
                restrictions: obj.restrictions.clone(),
            };

            self.analyzed_program
                .analyzed_objects
                .insert(obj.module_ref.clone(), ana_obj);
        }

        for trt in m.traits.values() {
            let ana_trt = AnTraitData {
                name: trt.module_ref.clone(),
                generics: trt.generics.iter().map(|g| g.0).collect(),
                methods: trt
                    .functions
                    .iter()
                    .map(|(name, fun)| (name.clone(), Self::map_object_function(fun)))
                    .collect(),
                associated_tys: Default::default(),
                restrictions: trt.restrictions.clone(),
                impls: vec![],
            };

            self.analyzed_program
                .analyzed_traits
                .insert(trt.module_ref.clone(), ana_trt);
        }

        for imp in m.impls.values() {
            let trt_name = imp.trait_ty.0.clone();
            self.analyzed_program
                .analyzed_traits
                .get_mut(&trt_name)
                .is_expected(imp.name_span, "trait", &trt_name.full_name()?)?
                .impls
                .push(imp.impl_id);

            let ana_imp = AnImplData {
                impl_id: imp.impl_id,
                generics: imp.generics.iter().map(|g| g.0).collect(),
                methods: imp
                    .fns
                    .iter()
                    .map(|(name, fun)| (name.clone(), Self::map_object_function(fun)))
                    .collect(),
                trait_ty: imp.trait_ty.clone(),
                impl_ty: imp.impl_ty.clone(),
                restrictions: imp.restrictions.clone(),
                associated_tys: imp.associated_types.clone(),
                is_dummy: false,
            };

            self.analyzed_program
                .analyzed_impls
                .insert(imp.impl_id, ana_imp);
        }

        Ok(m)
    }
}
