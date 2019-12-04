use crate::ana::represent_visitor::AstAnalysisPass;
use crate::parser::ast::{AstBlock, AstExpression, AstMatchPattern, AstStatement, AstType};
use crate::parser::ast_visitor::AstAdapter;
use crate::util::PResult;

pub struct AnalyzeForLoops;

impl AstAnalysisPass for AnalyzeForLoops {
    fn new() -> Self {
        AnalyzeForLoops
    }
}

impl AstAdapter for AnalyzeForLoops {
    fn enter_statement(&mut self, s: AstStatement) -> PResult<AstStatement> {
        match s {
            AstStatement::For {
                span,
                identifier,
                iterable,
                mut block,
            } => {
                let iter_name = String::from("$iter");
                let iter_object = AstExpression::identifier(span, iter_name.clone());
                block.statements.insert(
                    0,
                    AstStatement::let_statement(
                        AstMatchPattern::identifier(span, identifier, AstType::infer()),
                        AstExpression::object_call(
                            span,
                            iter_object.clone(),
                            "next".into(),
                            vec![],
                            vec![],
                        ),
                    ),
                );

                Ok(AstStatement::expression_statement(AstExpression::block(
                    span,
                    AstBlock::new(
                        vec![
                            AstStatement::let_statement(
                                AstMatchPattern::identifier(
                                    span,
                                    iter_name.clone(),
                                    AstType::infer(),
                                ),
                                AstExpression::object_call(
                                    span,
                                    iterable,
                                    "iterator".into(),
                                    vec![],
                                    vec![],
                                ),
                            ),
                            AstStatement::while_loop(
                                AstExpression::object_call(
                                    span,
                                    iter_object.clone(),
                                    "has_next".into(),
                                    vec![],
                                    vec![],
                                ),
                                block,
                            ),
                        ],
                        AstExpression::nothing(span),
                    ),
                )))
            }
            s => Ok(s),
        }
    }
}
