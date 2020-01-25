use crate::{
    parser::{
        ast::{AstObjectFunction, AstTraitType, AstType},
        ast_visitor::AstAdapter,
    },
    util::{PResult, Visit},
};

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct TyckInstantiatedObjectFunction {
    pub fun: AstObjectFunction,
    pub impl_ty: AstType,
    pub trait_ty: Option<AstTraitType>,
    pub fn_generics: Vec<AstType>,
}

pub trait TyckAdapter: AstAdapter {
    fn enter_tyck_object_fn(
        &mut self,
        i: TyckInstantiatedObjectFunction,
    ) -> PResult<TyckInstantiatedObjectFunction> {
        Ok(i)
    }
    fn exit_tyck_object_fn(
        &mut self,
        i: TyckInstantiatedObjectFunction,
    ) -> PResult<TyckInstantiatedObjectFunction> {
        Ok(i)
    }
}

impl<T: TyckAdapter> Visit<T> for TyckInstantiatedObjectFunction {
    fn visit(self, adapter: &mut T) -> PResult<TyckInstantiatedObjectFunction> {
        let TyckInstantiatedObjectFunction {
            fun,
            impl_ty,
            trait_ty,
            fn_generics,
        } = adapter.enter_tyck_object_fn(self)?;

        let i = TyckInstantiatedObjectFunction {
            fun: fun.visit(adapter)?,
            impl_ty: impl_ty.visit(adapter)?,
            trait_ty: trait_ty.visit(adapter)?,
            fn_generics: fn_generics.visit(adapter)?,
        };

        adapter.exit_tyck_object_fn(i)
    }
}
