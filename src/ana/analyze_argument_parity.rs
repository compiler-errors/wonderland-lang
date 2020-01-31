use crate::{
    ana::{represent::AnalyzedProgram, represent_visitor::PureAnalysisPass},
    parser::{
        ast::{AstExpression, AstExpressionData},
        ast_visitor::AstAdapter,
    },
    util::PResult,
};

pub struct AnalyzeArgumentParity(AnalyzedProgram);

impl PureAnalysisPass for AnalyzeArgumentParity {
    fn new(a: AnalyzedProgram) -> PResult<AnalyzeArgumentParity> {
        Ok(AnalyzeArgumentParity(a))
    }

    fn drop(self) -> AnalyzedProgram {
        self.0
    }
}

impl AstAdapter for AnalyzeArgumentParity {
    fn enter_ast_expression(&mut self, e: AstExpression) -> PResult<AstExpression> {
        match &e.data {
            AstExpressionData::FnCall { fn_name, args, .. } => {
                let expected = self.0.analyzed_functions[&fn_name].parameters.len();
                let provided = args.len();

                if expected != provided {
                    return perror_at!(
                        e.span,
                        "Expected {} arguments, got {}.",
                        expected,
                        provided
                    );
                }
            },
            _ => {},
        }

        Ok(e)
    }
}
