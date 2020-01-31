use crate::{
    ana::{represent::AnalyzedProgram, represent_visitor::PureAnalysisPass},
    parser::{ast::AstImpl, ast_visitor::AstAdapter},
    util::{PResult, Span},
};
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
    fn enter_ast_impl(&mut self, i: AstImpl) -> PResult<AstImpl> {
        if let Some(trait_ty) = &i.trait_ty {
            let info = &self.0.analyzed_traits[&trait_ty.name];

            compare(
                i.name_span,
                "method",
                info.methods.keys().cloned().collect(),
                i.fns.keys().cloned().collect(),
            )?;

            compare(
                i.name_span,
                "associated type",
                info.associated_tys.keys().cloned().collect(),
                i.associated_types.keys().cloned().collect(),
            )?;
        } else {
            if !i.associated_types.is_empty() {
                return perror_at!(
                    i.name_span,
                    "Did not expect anonymous impl to have associated types"
                );
            }
        }

        Ok(i)
    }
}

fn compare(
    span: Span,
    what: &str,
    expected: HashSet<String>,
    given: HashSet<String>,
) -> PResult<()> {
    for k in &given {
        if !expected.contains(k) {
            return perror_at!(
                span,
                "Impl provides {} `{}`, but it is not expected.",
                what,
                k
            );
        }
    }

    for k in &expected {
        if !given.contains(k) {
            return perror_at!(span, "Expected {} `{}`, but not found in impl.", what, k);
        }
    }

    Ok(())
}
