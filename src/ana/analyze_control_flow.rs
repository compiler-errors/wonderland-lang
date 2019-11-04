use crate::ana::represent_visitor::AstAnalysisPass;
use crate::parser::ast::{AstExpression, AstExpressionData, AstStatement};
use crate::parser::ast_visitor::AstAdapter;
use crate::util::{IntoError, PResult};

pub struct AnalyzeControlFlow(Vec<bool>);

impl AstAnalysisPass for AnalyzeControlFlow {
    fn new() -> AnalyzeControlFlow {
        AnalyzeControlFlow(vec![false])
    }
}

impl AstAdapter for AnalyzeControlFlow {
    fn enter_statement(&mut self, s: AstStatement) -> PResult<AstStatement> {
        match &s {
            AstStatement::While { .. } => {
                self.0.push(true);
            }
            AstStatement::Break | AstStatement::Continue => {
                if !(self.0).last().unwrap() {
                    return PResult::error(format!(
                        "Cannot `break` or `continue` in a non-loop context."
                    ));
                }
            }
            // Fn call, save the useful stuff and clone everything above...
            _ => {}
        }

        Ok(s)
    }

    fn enter_expression(&mut self, e: AstExpression) -> PResult<AstExpression> {
        match e.data {
            AstExpressionData::Closure { .. } => {
                (self.0).push(false);
            }
            _ => {}
        }

        Ok(e)
    }

    fn exit_statement(&mut self, s: AstStatement) -> PResult<AstStatement> {
        match &s {
            AstStatement::While { .. } => {
                self.0.pop();
            }
            _ => {}
        }

        Ok(s)
    }
}
