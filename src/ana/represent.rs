use crate::{
    ana::analyze_modules::{ModuleItem, SharedModule},
    ast::*,
    util::{FileId, PResult},
};
use std::collections::{HashMap, VecDeque};

#[Adapter("crate::ana::represent_visitor::AnAdapter")]
#[derive(Debug, Clone, Visit)]
pub struct AnalyzedProgram {
    pub variable_ids: HashMap<VariableId, AstNamedVariable>,
    pub analyzed_functions: HashMap<ModuleRef, AnFunctionData>,
    pub analyzed_traits: HashMap<ModuleRef, AnTraitData>,
    pub analyzed_enums: HashMap<ModuleRef, AnEnumData>,
    pub analyzed_objects: HashMap<ModuleRef, AnObjectData>,
    pub analyzed_impls: HashMap<ImplId, AnImplData>,
    pub analyzed_globals: HashMap<ModuleRef, AstType>,

    pub associated_types_to_traits: HashMap<String, Vec<ModuleRef>>,
    pub methods_to_traits: HashMap<String, Vec<ModuleRef>>,
    pub methods_to_anonymous_impls: HashMap<String, Vec<ImplId>>,

    pub analyzed_modules: HashMap<FileId, SharedModule>,
    pub top_module: SharedModule,
}

impl AnalyzedProgram {
    pub fn construct_trt_ref(&self, trt_path: &str) -> PResult<ModuleRef> {
        let r = self.get_module_ref("trait", trt_path)?;

        if !self.analyzed_traits.contains_key(&r) {
            return perror!("ICE: `{}` is not a trait", r.full_name());
        } else {
            Ok(r)
        }
    }

    pub fn construct_enum_ref(&self, obj_path: &str) -> PResult<ModuleRef> {
        let r = self.get_module_ref("enum", obj_path)?;

        if !self.analyzed_enums.contains_key(&r) {
            return perror!("ICE: `{}` is not an enum", r.full_name());
        } else {
            Ok(r)
        }
    }

    pub fn construct_fn_ref(&self, fn_path: &str) -> PResult<ModuleRef> {
        let r = self.get_module_ref("function", fn_path)?;

        if !self.analyzed_functions.contains_key(&r) {
            return perror!("ICE: `{}` is not a trait", r.full_name());
        } else {
            Ok(r)
        }
    }

    fn get_module_ref(&self, what: &str, path: &str) -> PResult<ModuleRef> {
        let mut path: VecDeque<&str> = path.split("::").collect();
        let name = path.pop_back().unwrap();

        let mut traversed_path = vec![];
        let mut submodule = self.top_module.clone();

        path.push_front("std");
        for name in path {
            let submodule_ref = submodule.clone();
            let child = (*submodule_ref)
                .borrow_mut()
                .get_child(name, &traversed_path)?;

            if let ModuleItem::Submodule(new_submodule) = child {
                submodule = new_submodule;
            } else {
                return perror!(
                    "ICE: submodule `{}` does not exist in module `{}`",
                    name,
                    traversed_path.join("::"),
                );
            }

            traversed_path.push(name.to_string());
        }

        let submodule_ref = submodule.clone();
        let r = if let ModuleItem::Symbol(file_id) = (*submodule_ref)
            .borrow_mut()
            .get_child(name, &traversed_path)?
        {
            ModuleRef::Normalized(file_id, name.to_string())
        } else {
            return perror!(
                "ICE: {} {} does not exist in module `{}`",
                what,
                name,
                traversed_path.join("::")
            );
        };

        Ok(r)
    }
}

#[Adapter("crate::ana::represent_visitor::AnAdapter")]
#[derive(Debug, Clone, Visit)]
pub struct AnObjectData {
    pub name: ModuleRef,
    pub self_ty: AstType,
    pub generics: Vec<AstGeneric>,
    pub member_tys: HashMap<String, AstType>,
    pub member_indices: HashMap<String, usize>,
    pub restrictions: Vec<AstTypeRestriction>,
}

#[Adapter("crate::ana::represent_visitor::AnAdapter")]
#[derive(Debug, Clone, Visit)]
pub struct AnEnumData {
    pub name: ModuleRef,
    pub self_ty: AstType,
    pub generics: Vec<AstGeneric>,
    pub variants: HashMap<String, AnEnumVariantData>,
    pub restrictions: Vec<AstTypeRestriction>,
}

#[Adapter("crate::ana::represent_visitor::AnAdapter")]
#[derive(Debug, Clone, Visit)]
pub struct AnEnumVariantData {
    pub name: String,
    pub fields: Vec<AstType>,
    pub field_names: Option<HashMap<String, usize>>,
}

#[Adapter("crate::ana::represent_visitor::AnAdapter")]
#[derive(Debug, Clone, Visit)]
pub struct AnTraitData {
    pub name: ModuleRef,
    pub generics: Vec<AstGeneric>,
    pub methods: HashMap<String, AnFunctionData>,
    pub associated_tys: HashMap<String, AstAssociatedType>,
    pub restrictions: Vec<AstTypeRestriction>,
    pub impls: Vec<ImplId>,
}

#[Adapter("crate::ana::represent_visitor::AnAdapter")]
#[derive(Debug, Clone, Eq, PartialEq, Visit)]
pub struct AnFunctionData {
    pub name: Option<ModuleRef>,
    pub generics: Vec<AstGeneric>,
    pub parameters: Vec<AstType>,
    pub return_type: AstType,
    pub restrictions: Vec<AstTypeRestriction>,
    pub has_self: bool,
}

#[Adapter("crate::ast::ast_visitor::AstAdapter")]
#[derive(Debug, Clone, Eq, PartialEq, VisitAnonymous)]
pub enum AnImplKind {
    Regular,
    DynamicDispatch,
    DynamicCoersion,
    DynamicDowncast,
    Dummy,
}

#[Adapter("crate::ana::represent_visitor::AnAdapter")]
#[derive(Debug, Clone, Eq, PartialEq, Visit)]
pub struct AnImplData {
    pub impl_id: ImplId,
    pub generics: Vec<AstGeneric>,
    pub methods: HashMap<String, AnFunctionData>,
    pub trait_ty: Option<AstTraitType>,
    pub impl_ty: AstType,
    pub restrictions: Vec<AstTypeRestriction>,
    pub associated_tys: HashMap<String, AstType>,

    pub kind: AnImplKind,
}

impl AnImplData {
    pub fn is_regular(&self) -> bool {
        return self.kind == AnImplKind::Regular;
    }

    pub fn is_dynamic_dispatch(&self) -> bool {
        self.kind == AnImplKind::DynamicCoersion
    }

    pub fn is_dynamic_cast(&self) -> bool {
        match self.kind {
            AnImplKind::DynamicDispatch | AnImplKind::DynamicDowncast => true,
            _ => false,
        }
    }

    pub fn is_dummy(&self) -> bool {
        return self.kind == AnImplKind::Dummy;
    }
}
