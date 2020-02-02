use crate::{
    parser::{
        ast::{AstObjectFunction, AstTraitType, AstType},
        ast_visitor::AstAdapter,
    },
    util::PResult,
};

#[Adapter("crate::tyck::tyck_represent::TyckAdapter")]
#[derive(Debug, Clone, Eq, PartialEq, Visit)]
pub struct TyckInstantiatedObjectFunction {
    pub fun: AstObjectFunction,
    pub impl_ty: AstType,
    pub trait_ty: Option<AstTraitType>,
    pub fn_generics: Vec<AstType>,
}

pub trait TyckAdapter: AstAdapter {
    fn enter_tyck_instantiated_object_function(
        &mut self,
        i: TyckInstantiatedObjectFunction,
    ) -> PResult<TyckInstantiatedObjectFunction> {
        Ok(i)
    }
    fn exit_tyck_instantiated_object_function(
        &mut self,
        i: TyckInstantiatedObjectFunction,
    ) -> PResult<TyckInstantiatedObjectFunction> {
        Ok(i)
    }
}
