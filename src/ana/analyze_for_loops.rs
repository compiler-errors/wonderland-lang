use crate::{
    ana::represent_visitor::AstAnalysisPass,
    ast::{
        ast_visitor::AstAdapter, AstBlock, AstExpression, AstExpressionData, AstMatchPattern,
        AstStatement, AstType,
    },
    util::PResult,
};
use std::sync::RwLock;

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
                let mut id_ref = FOR_LOOP_ID_COUNTER.write().unwrap();
                *id_ref += 1;

                let iter_name = format!("$iter{}", id_ref);
                let item_name = format!("$item{}", id_ref);
                let next_name = format!("$next{}", id_ref);

                let iter_object = AstExpression::identifier(span, iter_name.clone());
                let item_object = AstExpression::identifier(span, item_name.clone());
                let next_object = AstExpression::identifier(span, next_name.clone());

                block.statements.insert(
                    0,
                    AstStatement::let_statement(
                        pattern,
                        AstExpression::object_call(
                            span,
                            item_object.clone(),
                            "unwrap".into(),
                            vec![],
                            vec![],
                        ),
                    ),
                );

                block.statements.extend(vec![
                    AstStatement::let_statement(
                        AstMatchPattern::identifier(span, next_name.clone(), AstType::infer()),
                        AstExpression::object_call(
                            span,
                            iter_object.clone(),
                            "next".into(),
                            vec![],
                            vec![],
                        ),
                    ),
                    AstStatement::expression_statement(AstExpression::assign(
                        span,
                        item_object.clone(),
                        AstExpression::tuple_access(span, next_object.clone(), 0),
                    )),
                    AstStatement::expression_statement(AstExpression::assign(
                        span,
                        iter_object.clone(),
                        AstExpression::tuple_access(span, next_object, 1),
                    )),
                ]);

                Ok(AstExpression::block(
                    span,
                    AstBlock::new(
                        vec![AstStatement::let_statement(
                            AstMatchPattern::tuple(
                                span,
                                vec![
                                    AstMatchPattern::identifier(
                                        span,
                                        item_name.clone(),
                                        AstType::infer(),
                                    ),
                                    AstMatchPattern::identifier(
                                        span,
                                        iter_name.clone(),
                                        AstType::infer(),
                                    ),
                                ],
                                AstType::infer(),
                            ),
                            AstExpression::object_call(
                                span,
                                AstExpression::object_call(
                                    span,
                                    *iterable,
                                    "iterator".into(),
                                    vec![],
                                    vec![],
                                ),
                                "next".into(),
                                vec![],
                                vec![],
                            ),
                        )],
                        AstExpression::while_loop(
                            span,
                            label,
                            AstExpression::object_call(
                                span,
                                item_object.clone(),
                                "is_some".into(),
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

lazy_static! {
    static ref FOR_LOOP_ID_COUNTER: RwLock<usize> = RwLock::new(0);
}
