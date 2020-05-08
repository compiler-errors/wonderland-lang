use crate::{
    ast::{ast_visitor::AstAdapter, AstObjectFunction, AstTraitType, AstType},
    util::PResult,
};

pub trait TyckAdapter: AstAdapter {
    fn enter_tyck_instantiated_object_function(
        &mut self,
        i: TyckInstantiatedObjectFunction,
    ) -> PResult<TyckInstantiatedObjectFunction> {
        Ok(i)
    }

    fn enter_tyck_instantiated_impl(
        &mut self,
        i: TyckInstantiatedImpl,
    ) -> PResult<TyckInstantiatedImpl> {
        Ok(i)
    }

    fn exit_tyck_instantiated_object_function(
        &mut self,
        i: TyckInstantiatedObjectFunction,
    ) -> PResult<TyckInstantiatedObjectFunction> {
        Ok(i)
    }

    fn exit_tyck_instantiated_impl(
        &mut self,
        i: TyckInstantiatedImpl,
    ) -> PResult<TyckInstantiatedImpl> {
        Ok(i)
    }
}

#[Adapter("crate::tyck::tyck_represent::TyckAdapter")]
#[derive(Debug, Clone, Eq, PartialEq, Visit)]
pub struct TyckInstantiatedObjectFunction {
    pub fun: AstObjectFunction,
    pub impl_ty: AstType,
    pub trait_ty: Option<AstTraitType>,
    pub fn_generics: Vec<AstType>,
}

#[Adapter("crate::tyck::tyck_represent::TyckAdapter")]
#[derive(Debug, Clone, Eq, PartialEq, Visit)]
pub struct TyckInstantiatedImpl {
    pub impl_ty: AstType,
    pub trait_ty: AstTraitType,
}
