use crate::{ana::analyze_modules::SharedModule, parser::ast::*, util::FileId};
use std::collections::HashMap;

#[Adapter("crate::ana::represent_visitor::AnAdapter")]
#[derive(Debug, Clone, Visit)]
pub struct AnalyzedProgram {
    pub variable_ids: HashMap<VariableId, AstNamedVariable>,
    pub analyzed_functions: HashMap<ModuleRef, AnFunctionData>,
    pub analyzed_traits: HashMap<ModuleRef, AnTraitData>,
    pub analyzed_enums: HashMap<ModuleRef, AnEnumData>,
    pub analyzed_objects: HashMap<ModuleRef, AnObjectData>,
    pub analyzed_impls: HashMap<ImplId, AnImplData>,
    pub analyzed_modules: HashMap<FileId, SharedModule>,
    pub analyzed_globals: HashMap<ModuleRef, AstType>,
    pub associated_types_to_traits: HashMap<String, Vec<ModuleRef>>,
    pub methods_to_traits: HashMap<String, Vec<ModuleRef>>,
    pub methods_to_anonymous_impls: HashMap<String, Vec<ImplId>>,
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

    pub is_dummy: bool,
}
