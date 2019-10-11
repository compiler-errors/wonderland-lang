use crate::ana::represent::AnalyzedProgram;
use crate::ana::represent_visitor::PureAnalysisPass;
use crate::parser::ast::{AstExpression, AstExpressionData};
use crate::parser::ast_visitor::AstAdapter;
use crate::util::{IntoError, PResult};

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
    fn enter_expression(&mut self, e: AstExpression) -> PResult<AstExpression> {
        match &e.data {
            AstExpressionData::Call { fn_name, args, .. } => {
                let expected = self.0.analyzed_functions[&fn_name].parameters.len();
                let provided = args.len();

                if expected != provided {
                    return PResult::error_at(
                        e.span,
                        format!(
                            "In function `{}`, expected {} arguments, got {}.",
                            fn_name.full_name()?,
                            expected,
                            provided
                        ),
                    );
                }
            }
            _ => {}
        }

        Ok(e)
    }
}
