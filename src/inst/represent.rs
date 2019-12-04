use crate::parser::ast::{AstTraitType, AstType, ImplId, ModuleRef};

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct InstFunctionSignature(pub ModuleRef, pub Vec<AstType>);

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct InstObjectSignature(pub ModuleRef, pub Vec<AstType>);

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct InstEnumSignature(pub ModuleRef, pub Vec<AstType>);

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct InstObjectFunctionSignature(pub AstType, pub AstTraitType, pub String, pub Vec<AstType>);

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct InstImplSignature(pub ImplId, pub Vec<AstType>);
