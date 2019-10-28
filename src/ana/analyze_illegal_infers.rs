use crate::ana::represent_visitor::AstAnalysisPass;
use crate::parser::ast::{
    AstFunction, AstGlobalVariable, AstImpl, AstObject, AstObjectFunction, AstTrait, AstType,
};
use crate::parser::ast_visitor::AstAdapter;
use crate::util::{IntoError, PResult, Visit};

pub struct AnalyzeIllegalInfers;

impl AstAnalysisPass for AnalyzeIllegalInfers {
    fn new() -> AnalyzeIllegalInfers {
        AnalyzeIllegalInfers
    }
}

impl AstAdapter for AnalyzeIllegalInfers {
    fn enter_function(&mut self, mut f: AstFunction) -> PResult<AstFunction> {
        f.return_type = f.return_type.visit(&mut DenyInfer)?;
        f.parameter_list = f.parameter_list.visit(&mut DenyInfer)?;
        f.restrictions = f.restrictions.visit(&mut DenyInfer)?;

        Ok(f)
    }

    fn enter_object(&mut self, mut o: AstObject) -> PResult<AstObject> {
        o.restrictions = o.restrictions.visit(&mut DenyInfer)?;
        o.members = o.members.visit(&mut DenyInfer)?;

        Ok(o)
    }

    fn enter_object_function(&mut self, mut o: AstObjectFunction) -> PResult<AstObjectFunction> {
        o.return_type = o.return_type.visit(&mut DenyInfer)?;
        o.parameter_list = o.parameter_list.visit(&mut DenyInfer)?;
        o.restrictions = o.restrictions.visit(&mut DenyInfer)?;

        Ok(o)
    }

    fn enter_trait(&mut self, mut t: AstTrait) -> PResult<AstTrait> {
        t.restrictions = t.restrictions.visit(&mut DenyInfer)?;
        t.associated_types = t.associated_types.visit(&mut DenyInfer)?;

        Ok(t)
    }

    fn enter_impl(&mut self, mut i: AstImpl) -> PResult<AstImpl> {
        i.restrictions = i.restrictions.visit(&mut DenyInfer)?;
        i.impl_ty = i.impl_ty.visit(&mut DenyInfer)?;
        i.trait_ty = i.trait_ty.visit(&mut DenyInfer)?;
        i.associated_types = i.associated_types.visit(&mut DenyInfer)?;

        Ok(i)
    }

    fn enter_global_variable(&mut self, mut g: AstGlobalVariable) -> PResult<AstGlobalVariable> {
        g.ty = g.ty.visit(&mut DenyInfer)?;

        Ok(g)
    }
}

struct DenyInfer;

impl AstAdapter for DenyInfer {
    fn enter_type(&mut self, t: AstType) -> PResult<AstType> {
        if let AstType::Infer(_) = t {
            PResult::error(format!("The `_` type is not allowed in this environment"))
        } else {
            Ok(t)
        }
    }
}
