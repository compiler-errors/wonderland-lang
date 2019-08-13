use crate::parser::{AstNamedVariable, AstType, AstTypeRestriction, ParsedFile};
use std::collections::HashMap;

pub struct AnalyzedFile {
    pub parsed_file: ParsedFile,
    pub variable_ids: HashMap<usize, AstNamedVariable>,
    pub analyzed_functions: HashMap<String, AnFunctionData>,
    pub analyzed_traits: HashMap<String, AnTraitData>,
    pub analyzed_objects: HashMap<String, AnObjectData>,
    pub analyzed_impls: Vec<AnImplData>,
}

pub struct AnObjectData {
    pub generics: Vec<usize>,
    pub members: HashMap<String, AstType>,
    pub restrictions: Vec<AstTypeRestriction>,
}

pub struct AnTraitData {
    pub generics: Vec<usize>,
    pub methods: HashMap<String, AnFunctionData>,
    pub restrictions: Vec<AstTypeRestriction>,
}

pub struct AnFunctionData {
    pub generics: Vec<usize>,
    pub parameters: Vec<AstType>,
    pub return_type: AstType,
    pub restrictions: Vec<AstTypeRestriction>,
}

pub struct AnImplData {
    pub generics: Vec<usize>,
    pub trait_ty: AstType,
    pub impl_ty: AstType,
    pub restrictions: Vec<AstTypeRestriction>,
}
