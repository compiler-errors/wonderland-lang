use crate::ana::represent::AnalyzedProgram;
use crate::ana::represent_visitor::PureAnalysisPass;
use crate::parser::ast::AstImpl;
use crate::parser::ast_visitor::AstAdapter;
use crate::util::{IntoError, PResult};
use std::collections::HashSet;

pub struct AnalyzeImpls(AnalyzedProgram);

impl PureAnalysisPass for AnalyzeImpls {
    fn new(analyzed_program: AnalyzedProgram) -> PResult<AnalyzeImpls> {
        Ok(AnalyzeImpls(analyzed_program))
    }

    fn drop(self) -> AnalyzedProgram {
        self.0
    }
}

impl AstAdapter for AnalyzeImpls {
    fn enter_impl(&mut self, i: AstImpl) -> PResult<AstImpl> {
        let info = &self.0.analyzed_traits[&i.trait_ty.0];

        compare(
            "method",
            info.methods.keys().cloned().collect(),
            i.fns.keys().cloned().collect(),
        )?;

        compare(
            "associated type",
            info.associated_tys.keys().cloned().collect(),
            i.associated_types.keys().cloned().collect(),
        )?;

        Ok(i)
    }
}

fn compare(what: &str, expected: HashSet<String>, given: HashSet<String>) -> PResult<()> {
    for k in &given {
        if !expected.contains(k) {
            return PResult::error(format!(
                "Impl provides {} `{}`, but it is not expected.",
                what, k
            ));
        }
    }

    for k in &expected {
        if !given.contains(k) {
            return PResult::error(format!("Expected {} `{}`, but not found in impl.", what, k));
        }
    }

    Ok(())
}
