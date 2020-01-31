use crate::{
    ana::represent_visitor::AstAnalysisPass,
    parser::{
        ast::{AstBlock, AstExpression, AstExpressionData, AstMatchPattern, AstStatement, AstType},
        ast_visitor::AstAdapter,
    },
    util::PResult,
};

pub struct AnalyzeForLoops;

impl AstAnalysisPass for AnalyzeForLoops {
    fn new() -> Self {
        AnalyzeForLoops
    }
}

impl AstAdapter for AnalyzeForLoops {
    fn enter_ast_expression(&mut self, e: AstExpression) -> PResult<AstExpression> {
        match e {
            AstExpression {
                data:
                    AstExpressionData::For {
                        label,
                        pattern,
                        iterable,
                        mut block,
                        else_block,
                    },
                ty: _,
                span,
            } => {
                let iter_name = String::from("$iter");
                let iter_object = AstExpression::identifier(span, iter_name.clone());

                block.statements.insert(
                    0,
                    AstStatement::let_statement(
                        pattern,
                        AstExpression::object_call(
                            span,
                            iter_object.clone(),
                            "next".into(),
                            vec![],
                            vec![],
                        ),
                    ),
                );

                Ok(AstExpression::block(
                    span,
                    AstBlock::new(
                        vec![AstStatement::let_statement(
                            AstMatchPattern::identifier(span, iter_name.clone(), AstType::infer()),
                            AstExpression::object_call(
                                span,
                                *iterable,
                                "iterator".into(),
                                vec![],
                                vec![],
                            ),
                        )],
                        AstExpression::while_loop(
                            span,
                            label,
                            AstExpression::object_call(
                                span,
                                iter_object.clone(),
                                "has_next".into(),
                                vec![],
                                vec![],
                            ),
                            block,
                            else_block,
                        ),
                    ),
                ))
            },
            s => Ok(s),
        }
    }
}
