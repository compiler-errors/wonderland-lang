use crate::parser::ast::*;
use crate::parser::ast_visitor::Adapter;
use crate::util::result::*;
use std::collections::HashMap;

#[derive(Debug)]
pub struct AnalyzedFile {
    pub parsed_file: ParsedFile,
    pub variable_ids: HashMap<VariableId, AstNamedVariable>,
    pub analyzed_functions: HashMap<String, AnFunctionData>,
    pub analyzed_traits: HashMap<String, AnTraitData>,
    pub analyzed_objects: HashMap<String, AnObjectData>,
    pub analyzed_impls: HashMap<ImplId, AnImplData>,
}

#[derive(Debug)]
pub struct AnObjectData {
    pub generics: Vec<GenericId>,
    pub members: HashMap<String, AstType>,
    pub restrictions: Vec<AstTypeRestriction>,
}

#[derive(Debug)]
pub struct AnTraitData {
    pub generics: Vec<GenericId>,
    pub methods: HashMap<String, AnFunctionData>,
    pub associated_tys: HashMap<String, AstAssociatedType>,
    pub restrictions: Vec<AstTypeRestriction>,
    pub impls: Vec<ImplId>,
}

#[derive(Debug)]
pub struct AnFunctionData {
    pub generics: Vec<GenericId>,
    pub parameters: Vec<AstType>,
    pub return_type: AstType,
    pub restrictions: Vec<AstTypeRestriction>,
}

#[derive(Debug)]
pub struct AnImplData {
    pub impl_id: ImplId,
    pub generics: Vec<GenericId>,
    pub trait_ty: AstTraitType,
    pub impl_ty: AstType,
    pub restrictions: Vec<AstTypeRestriction>,
    pub associated_tys: HashMap<String, AstType>,
}
