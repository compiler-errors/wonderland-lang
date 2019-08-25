use crate::parser::ast::*;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct AnalyzedFile {
    pub variable_ids: HashMap<VariableId, AstNamedVariable>,
    pub analyzed_functions: HashMap<String, AnFunctionData>,
    pub analyzed_traits: HashMap<String, AnTraitData>,
    pub analyzed_objects: HashMap<String, AnObjectData>,
    pub analyzed_impls: HashMap<ImplId, AnImplData>,
}

#[derive(Debug, Clone)]
pub struct AnObjectData {
    pub generics: Vec<GenericId>,
    pub members: HashMap<String, AstType>,
    pub restrictions: Vec<AstTypeRestriction>,
}

#[derive(Debug, Clone)]
pub struct AnTraitData {
    pub generics: Vec<GenericId>,
    pub methods: HashMap<String, AnFunctionData>,
    pub associated_tys: HashMap<String, AstAssociatedType>,
    pub restrictions: Vec<AstTypeRestriction>,
    pub impls: Vec<ImplId>,
}

#[derive(Debug, Clone)]
pub struct AnFunctionData {
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
