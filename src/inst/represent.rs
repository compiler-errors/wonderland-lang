use crate::parser::ast::{AstTraitType, AstType, ImplId, ModuleRef};
use std::collections::HashMap;

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct InstFunctionSignature(pub ModuleRef, pub Vec<AstType>);

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct InstObjectSignature(pub ModuleRef, pub Vec<AstType>);

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct InstEnumSignature(pub ModuleRef, pub Vec<AstType>);

#[derive(Clone, Debug)]
pub struct InstEnumRepresentation {
    pub fields: Vec<AstType>,
    pub variants: HashMap<String, Vec<usize>>,
    pub discriminants: HashMap<String, u64>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct InstObjectFunctionSignature(pub AstType, pub AstTraitType, pub String, pub Vec<AstType>);

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct InstImplSignature(pub ImplId, pub Vec<AstType>);
