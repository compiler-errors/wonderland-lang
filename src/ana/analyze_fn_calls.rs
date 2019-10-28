use crate::ana::represent_visitor::AstAnalysisPass;
use crate::parser::ast::{AstExpression, AstExpressionData};
use crate::parser::ast_visitor::AstAdapter;
use crate::util::PResult;

pub struct AnalyzeFnCalls;

impl AstAnalysisPass for AnalyzeFnCalls {
    fn new() -> Self {
        AnalyzeFnCalls
    }
}

impl AstAdapter for AnalyzeFnCalls {
    fn enter_expression(&mut self, e: AstExpression) -> PResult<AstExpression> {
        let AstExpression { data, ty, span } = e;

        let data = match data {
            AstExpressionData::ExprCall { expr, args } => {
                println!("FN CALL OF EXPR {:?}", expr);match expr.data {
                AstExpressionData::GlobalVariable { name } => AstExpressionData::FnCall {
                    fn_name: name,
                    generics: vec![],
                    args,
                },
                _ => AstExpressionData::ExprCall { expr, args },
            }},
            e => e,
        };

        Ok(AstExpression { data, ty, span })
    }
}