use crate::{
    ana::represent_visitor::AstAnalysisPass,
    parser::{
        ast::{
            AstBlock, AstExpression, AstExpressionData, AstFunction, AstObjectFunction,
            AstStatement,
        },
        ast_visitor::AstAdapter,
    },
    util::PResult,
};

pub struct AnalyzeReturns;

impl AstAnalysisPass for AnalyzeReturns {
    fn new() -> Self {
        AnalyzeReturns
    }
}

impl AstAdapter for AnalyzeReturns {
    fn enter_ast_function(&mut self, mut f: AstFunction) -> PResult<AstFunction> {
        f.definition = f.definition.map(lift_returns);

        Ok(f)
    }

    fn enter_ast_object_function(
        &mut self,
        mut o: AstObjectFunction,
    ) -> PResult<AstObjectFunction> {
        o.definition = o.definition.map(lift_returns);

        Ok(o)
    }
}

fn lift_returns(block: AstBlock) -> AstBlock {
    let should_lift_return =
        is_unit(&block.expression) && block.statements.last().map(is_return).unwrap_or(false);

    if should_lift_return {
        let AstBlock { mut statements, .. } = block;

        let last = statements.pop().unwrap();
        let expression = get_return_value(last);

        AstBlock {
            statements,
            expression: Box::new(expression),
        }
    } else {
        block
    }
}

fn is_unit(e: &AstExpression) -> bool {
    if let AstExpressionData::Tuple { values } = &e.data {
        values.is_empty()
    } else {
        false
    }
}

fn is_return(s: &AstStatement) -> bool {
    if let AstStatement::Return { .. } = s {
        true
    } else {
        false
    }
}

fn get_return_value(s: AstStatement) -> AstExpression {
    if let AstStatement::Return { value } = s {
        value
    } else {
        unreachable!()
    }
}
