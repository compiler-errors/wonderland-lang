use crate::{
    ana::represent::*,
    ast::{ast_visitor::AstAdapter, AstProgram},
    util::{PResult, Visit},
};

pub type AnalysisPassFn =
    Box<dyn FnOnce(AnalyzedProgram, AstProgram) -> PResult<(AnalyzedProgram, AstProgram)>>;

pub trait AstAnalysisPass: AstAdapter + Sized {
    fn new() -> Self;

    fn analyze(a: AnalyzedProgram, p: AstProgram) -> PResult<(AnalyzedProgram, AstProgram)> {
        let p = p.visit(&mut Self::new())?;

        Ok((a, p))
    }
}

pub trait PureAnalysisPass: AstAdapter + Sized {
    fn new(a: AnalyzedProgram) -> PResult<Self>;

    // Recovers the analyzed program which is contained in this analysis pass
    fn drop(self) -> AnalyzedProgram;

    fn analyze(a: AnalyzedProgram, p: AstProgram) -> PResult<(AnalyzedProgram, AstProgram)> {
        let mut pass = Self::new(a)?;
        let p = p.visit(&mut pass)?;
        let a = pass.drop();

        Ok((a, p))
    }
}

// An analysis pass which mutably affects the AnalyzedProgram.
pub trait DirtyAnalysisPass: AnAdapter + Sized {
    fn new(a: &AnalyzedProgram) -> PResult<Self>;

    fn analyze(a: AnalyzedProgram, p: AstProgram) -> PResult<(AnalyzedProgram, AstProgram)> {
        let mut pass = Self::new(&a)?;
        let a = a.visit(&mut pass)?;
        let p = p.visit(&mut pass)?;

        Ok((a, p))
    }
}

pub trait AnAdapter: AstAdapter {
    fn enter_analyzed_program(&mut self, p: AnalyzedProgram) -> PResult<AnalyzedProgram> {
        Ok(p)
    }

    fn enter_an_function_data(&mut self, f: AnFunctionData) -> PResult<AnFunctionData> {
        Ok(f)
    }

    fn enter_an_trait_data(&mut self, t: AnTraitData) -> PResult<AnTraitData> {
        Ok(t)
    }

    fn enter_an_object_data(&mut self, o: AnObjectData) -> PResult<AnObjectData> {
        Ok(o)
    }

    fn enter_an_enum_data(&mut self, e: AnEnumData) -> PResult<AnEnumData> {
        Ok(e)
    }

    fn enter_an_enum_variant_data(&mut self, e: AnEnumVariantData) -> PResult<AnEnumVariantData> {
        Ok(e)
    }

    fn enter_an_impl_data(&mut self, i: AnImplData) -> PResult<AnImplData> {
        Ok(i)
    }

    fn exit_analyzed_program(&mut self, p: AnalyzedProgram) -> PResult<AnalyzedProgram> {
        Ok(p)
    }

    fn exit_an_function_data(&mut self, f: AnFunctionData) -> PResult<AnFunctionData> {
        Ok(f)
    }

    fn exit_an_trait_data(&mut self, t: AnTraitData) -> PResult<AnTraitData> {
        Ok(t)
    }

    fn exit_an_object_data(&mut self, o: AnObjectData) -> PResult<AnObjectData> {
        Ok(o)
    }

    fn exit_an_enum_data(&mut self, e: AnEnumData) -> PResult<AnEnumData> {
        Ok(e)
    }

    fn exit_an_enum_variant_data(&mut self, e: AnEnumVariantData) -> PResult<AnEnumVariantData> {
        Ok(e)
    }

    fn exit_an_impl_data(&mut self, i: AnImplData) -> PResult<AnImplData> {
        Ok(i)
    }
}
