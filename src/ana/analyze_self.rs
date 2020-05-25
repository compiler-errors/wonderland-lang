use crate::{
    ana::{
        represent::*,
        represent_visitor::{AnAdapter, DirtyAnalysisPass},
    },
    ast::{visitor::AstAdapter, AstEnum, AstFunction, AstImpl, AstObject, AstType},
    util::{PResult, Visit},
};

pub struct AnalyzeSelf;

impl DirtyAnalysisPass for AnalyzeSelf {
    fn new(_: &AnalyzedProgram) -> PResult<AnalyzeSelf> {
        Ok(AnalyzeSelf)
    }
}

impl AnAdapter for AnalyzeSelf {
    fn enter_an_object_data(&mut self, o: AnObjectData) -> PResult<AnObjectData> {
        let self_type = o.self_ty.clone();

        o.visit(&mut ReplaceSelf(self_type))
    }

    fn enter_an_impl_data(&mut self, i: AnImplData) -> PResult<AnImplData> {
        // No `Self` type in the impl_ty!
        let impl_ty = i.impl_ty.clone().visit(&mut DenySelf)?;

        i.visit(&mut ReplaceSelf(impl_ty))
    }
}

impl AstAdapter for AnalyzeSelf {
    fn enter_ast_function(&mut self, f: AstFunction) -> PResult<AstFunction> {
        f.visit(&mut DenySelf)
    }

    fn enter_ast_object(&mut self, o: AstObject) -> PResult<AstObject> {
        let self_type = AstType::Object(
            o.module_ref.clone(),
            o.generics.iter().map(|g| g.clone().into()).collect(),
        );

        o.visit(&mut ReplaceSelf(self_type))
    }

    fn enter_ast_enum(&mut self, e: AstEnum) -> PResult<AstEnum> {
        let self_type = AstType::Enum(
            e.module_ref.clone(),
            e.generics.iter().map(|g| g.clone().into()).collect(),
        );

        e.visit(&mut ReplaceSelf(self_type))
    }

    fn enter_ast_impl(&mut self, i: AstImpl) -> PResult<AstImpl> {
        // No `Self` type in the impl_ty.
        let impl_ty = i.impl_ty.clone().visit(&mut DenySelf)?;

        i.visit(&mut ReplaceSelf(impl_ty))
    }
}

/* --- --- --- --- --- ---  --- --- --- --- --- --- */
/* --- --- --- --- Helper adapters  --- --- --- --- */
/* --- --- --- --- --- ---  --- --- --- --- --- --- */

struct ReplaceSelf(AstType);

impl AnAdapter for ReplaceSelf {}

impl AstAdapter for ReplaceSelf {
    fn enter_ast_type(&mut self, t: AstType) -> PResult<AstType> {
        if t == AstType::SelfType {
            Ok(self.0.clone())
        } else {
            Ok(t)
        }
    }
}

struct DenySelf;

impl AnAdapter for DenySelf {}

impl AstAdapter for DenySelf {
    fn enter_ast_type(&mut self, t: AstType) -> PResult<AstType> {
        if t == AstType::SelfType {
            perror!("The `Self` type is not allowed in this environment")
        } else {
            Ok(t)
        }
    }
}
