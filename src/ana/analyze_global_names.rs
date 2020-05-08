use crate::{
    ana::{represent::AnalyzedProgram, represent_visitor::PureAnalysisPass},
    ast::{ast_visitor::AstAdapter, AstExpression, AstExpressionData},
    util::PResult,
};

pub struct AnalyzeGlobalNames(AnalyzedProgram);

impl PureAnalysisPass for AnalyzeGlobalNames {
    fn new(a: AnalyzedProgram) -> PResult<AnalyzeGlobalNames> {
        Ok(AnalyzeGlobalNames(a))
    }

    fn drop(self) -> AnalyzedProgram {
        self.0
    }
}

impl AstAdapter for AnalyzeGlobalNames {
    fn enter_ast_expression(&mut self, e: AstExpression) -> PResult<AstExpression> {
        match &e.data {
            AstExpressionData::FnCall { fn_name, .. } => {
                if !self.0.analyzed_functions.contains_key(fn_name) {
                    return perror_at!(e.span, "No such function named `{}`.", fn_name.full_name());
                }
            },
            AstExpressionData::GlobalVariable { name } => {
                if self.0.analyzed_globals.contains_key(name) {
                    // Okay. Do nothing.
                } else if self.0.analyzed_functions.contains_key(name) {
                    if self.0.analyzed_functions[name].generics.len() > 0 {
                        return perror_at!(
                            e.span,
                            "Cannot reference global fn `{}` since it has generics!",
                            name.full_name()
                        );
                    }

                    return Ok(AstExpression::global_fn(e.span, name.clone()));
                } else {
                    return perror_at!(
                        e.span,
                        "No such global variable named `{}`.",
                        name.full_name()
                    );
                }
            },
            _ => {},
        }

        Ok(e)
    }
}
