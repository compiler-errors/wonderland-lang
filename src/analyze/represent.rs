use crate::parser::{AstType, AstTypeRestriction};
use std::collections::HashMap;

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
