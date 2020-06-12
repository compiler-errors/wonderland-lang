use crate::{
    ana::{
        analyze_modules::{ModuleMap, SharedModule},
        represent::{
            AnEnumData, AnEnumVariantData, AnFunctionData, AnImplData, AnImplKind, AnObjectData,
            AnTraitData, AnalyzedProgram,
        },
    },
    ast::{visitor::AstAdapter, *},
    util::{Expect, FileId, PResult},
};
use std::collections::{HashMap, HashSet};

pub struct AnalyzeInfo {
    pub analyzed_program: AnalyzedProgram,
}

impl AnalyzeInfo {
    pub fn new(mod_map: ModuleMap, analyzed_modules: HashMap<FileId, SharedModule>) -> AnalyzeInfo {
        AnalyzeInfo {
            analyzed_program: AnalyzedProgram {
                analyzed_functions: hashmap! {},
                analyzed_traits: hashmap! {},
                analyzed_objects: hashmap! {},
                analyzed_enums: hashmap! {},
                analyzed_impls: hashmap! {},
                analyzed_globals: hashmap! {},
                associated_types_to_traits: hashmap! {},
                methods_to_traits: hashmap! {},
                methods_to_anonymous_impls: hashmap! {},
                analyzed_modules,
                top_module: Some(mod_map.top),
            },
        }
    }

    pub fn map_object_function(fun: &AstObjectFunction) -> AnFunctionData {
        AnFunctionData {
            name: None,
            generics: fun.generics.clone(),
            parameters: fun.parameter_list.iter().map(|p| p.ty.clone()).collect(),
            return_type: fun.return_type.clone(),
            restrictions: fun.restrictions.clone(),
            has_self: fun.has_self,
        }
    }
}

impl AstAdapter for AnalyzeInfo {
    fn enter_ast_module(&mut self, m: AstModule) -> PResult<AstModule> {
        for fun in m.functions.values() {
            let ana_fun = AnFunctionData {
                name: Some(fun.module_ref.clone()),
                generics: fun.generics.clone(),
                parameters: fun.parameter_list.iter().map(|p| p.ty.clone()).collect(),
                return_type: fun.return_type.inner.clone(),
                restrictions: fun.restrictions.clone(),
                has_self: false,
            };

            self.analyzed_program
                .analyzed_functions
                .insert(fun.module_ref.clone(), ana_fun);
        }

        for obj in m.objects.values() {
            let mut seen = HashSet::new();
            for m in &obj.members {
                if !seen.insert(m.name.clone()) {
                    return perror_at!(
                        obj.name_span,
                        "Duplicated member `{}` in object `{}`",
                        m.name,
                        obj.module_ref.full_name()
                    );
                }
            }

            let ana_obj = AnObjectData {
                name: obj.module_ref.clone(),
                self_ty: AstType::Object(
                    obj.module_ref.clone(),
                    obj.generics.iter().map(|g| g.clone().into()).collect(),
                ),
                generics: obj.generics.clone(),
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

        for en in m.enums.values() {
            let mut seen = HashSet::new();
            for v in en.variants.keys() {
                if !seen.insert(v.clone()) {
                    return perror_at!(
                        en.name_span,
                        "Duplicated variant `{}` in enum `{}`",
                        v,
                        en.module_ref.full_name()
                    );
                }
            }

            let ana_en = AnEnumData {
                name: en.module_ref.clone(),
                self_ty: AstType::Enum(
                    en.module_ref.clone(),
                    en.generics.iter().map(|g| g.clone().into()).collect(),
                ),
                generics: en.generics.clone(),
                variants: en
                    .variants
                    .values()
                    .map(|v| {
                        (v.name.clone(), AnEnumVariantData {
                            name: v.name.clone(),
                            fields: v.fields.clone(),
                            field_names: v.field_names.clone(),
                        })
                    })
                    .collect(),
                restrictions: en.restrictions.clone(),
            };

            self.analyzed_program
                .analyzed_enums
                .insert(en.module_ref.clone(), ana_en);
        }

        for trt in m.traits.values() {
            let ana_trt = AnTraitData {
                name: trt.module_ref.clone(),
                generics: trt.generics.clone(),
                methods: trt
                    .functions
                    .iter()
                    .map(|(name, fun)| (name.clone(), Self::map_object_function(fun)))
                    .collect(),
                associated_tys: trt.associated_types.clone(),
                restrictions: trt.restrictions.clone(),
                impls: vec![],
            };

            self.analyzed_program
                .analyzed_traits
                .insert(trt.module_ref.clone(), ana_trt);

            for assoc_ty in trt.associated_types.keys() {
                self.analyzed_program
                    .associated_types_to_traits
                    .entry(assoc_ty.clone())
                    .or_default()
                    .push(trt.module_ref.clone());
            }

            for method in trt.functions.keys() {
                self.analyzed_program
                    .methods_to_traits
                    .entry(method.clone())
                    .or_default()
                    .push(trt.module_ref.clone());
            }
        }

        for imp in m.impls.values() {
            let ana_imp = AnImplData {
                impl_id: imp.impl_id,
                generics: imp.generics.clone(),
                methods: imp
                    .fns
                    .iter()
                    .map(|(name, fun)| (name.clone(), Self::map_object_function(fun)))
                    .collect(),
                trait_ty: imp.trait_ty.clone(),
                impl_ty: imp.impl_ty.clone(),
                restrictions: imp.restrictions.clone(),
                associated_tys: imp.associated_types.clone(),
                kind: AnImplKind::Regular,
            };

            if imp.trait_ty.is_none() {
                for method in imp.fns.keys() {
                    self.analyzed_program
                        .methods_to_anonymous_impls
                        .entry(method.clone())
                        .or_default()
                        .push(imp.impl_id);
                }
            }

            self.analyzed_program
                .analyzed_impls
                .insert(imp.impl_id, ana_imp);
        }

        for global in m.globals.values() {
            self.analyzed_program
                .analyzed_globals
                .insert(global.module_ref.clone(), global.ty.clone());
        }

        Ok(m)
    }

    // Need to do this on the way up.
    fn exit_ast_program(&mut self, p: AstProgram) -> PResult<AstProgram> {
        for m in &p.modules {
            for imp in m.impls.values() {
                if let Some(trait_ty) = &imp.trait_ty {
                    let trt_name = trait_ty.name.clone();
                    self.analyzed_program
                        .analyzed_traits
                        .get_mut(&trt_name)
                        .as_expected(imp.name_span, "trait", &trt_name.full_name())?
                        .impls
                        .push(imp.impl_id);
                }
            }
        }

        Ok(p)
    }
}
