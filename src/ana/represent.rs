use crate::parser::ast::*;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct AnalyzedProgram {
    pub variable_ids: HashMap<VariableId, AstNamedVariable>,
    pub analyzed_functions: HashMap<ModuleRef, AnFunctionData>,
    pub analyzed_traits: HashMap<ModuleRef, AnTraitData>,
    pub analyzed_objects: HashMap<ModuleRef, AnObjectData>,
    pub analyzed_impls: HashMap<ImplId, AnImplData>,
}

#[derive(Debug, Clone)]
pub struct AnObjectData {
    pub name: ModuleRef,
    pub self_type: AstType,
    pub generics: Vec<GenericId>,
    pub member_tys: HashMap<String, AstType>,
    pub member_indices: HashMap<String, usize>,
    pub restrictions: Vec<AstTypeRestriction>,
}

#[derive(Debug, Clone)]
pub struct AnTraitData {
    pub name: ModuleRef,
    pub generics: Vec<GenericId>,
    pub methods: HashMap<String, AnFunctionData>,
    pub associated_tys: HashMap<String, AstAssociatedType>,
    pub restrictions: Vec<AstTypeRestriction>,
    pub impls: Vec<ImplId>,
}

#[derive(Debug, Clone)]
pub struct AnFunctionData {
    pub name: Option<ModuleRef>,
    pub generics: Vec<GenericId>,
    pub parameters: Vec<AstType>,
    pub return_type: AstType,
    pub restrictions: Vec<AstTypeRestriction>,
}

#[derive(Debug, Clone)]
pub struct AnImplData {
    pub impl_id: ImplId,
    pub generics: Vec<GenericId>,
    pub methods: HashMap<String, AnFunctionData>,
    pub trait_ty: AstTraitType,
    pub impl_ty: AstType,
    pub restrictions: Vec<AstTypeRestriction>,
    pub associated_tys: HashMap<String, AstType>,

    pub is_dummy: bool,
}
