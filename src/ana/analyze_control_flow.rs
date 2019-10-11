use crate::ana::represent::AnalyzedProgram;
use crate::ana::represent_visitor::AstAnalysisPass;
use crate::parser::ast::{AstBlock, AstProgram, AstStatement};
use crate::parser::ast_visitor::AstAdapter;
use crate::util::{IntoError, PResult, Visit};

pub struct AnalyzeControlFlow(usize);

impl AstAnalysisPass for AnalyzeControlFlow {
    fn new() -> AnalyzeControlFlow {
        AnalyzeControlFlow(0)
    }
}

impl AstAdapter for AnalyzeControlFlow {
    fn enter_block(&mut self, b: AstBlock) -> PResult<AstBlock> {
        self.0 += 1;

        Ok(b)
    }

    fn enter_statement(&mut self, s: AstStatement) -> PResult<AstStatement> {
        match &s {
            AstStatement::Break | AstStatement::Continue => {
                if self.0 == 0 {
                    return PResult::error(format!(
                        "Cannot `break` or `continue` in a non-loop context."
                    ));
                }
            }
            _ => {}
        }

        Ok(s)
    }

    fn exit_block(&mut self, b: AstBlock) -> PResult<AstBlock> {
        self.0 -= 1;

        Ok(b)
    }
}
