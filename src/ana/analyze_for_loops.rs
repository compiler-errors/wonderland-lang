use super::represent::AnalyzedProgram;
use crate::{
    ana::represent_visitor::PureAnalysisPass,
    ast::{
        visitor::AstAdapter, AstExpression, AstExpressionData, AstLabel, AstMatchPattern, AstType,
    },
    cheshire_quote,
    util::PResult,
};
use std::sync::RwLock;

pub struct AnalyzeForLoops {
    program: AnalyzedProgram,
}

impl PureAnalysisPass for AnalyzeForLoops {
    fn new(program: AnalyzedProgram) -> PResult<Self> {
        Ok(AnalyzeForLoops { program })
    }

    fn drop(self) -> AnalyzedProgram {
        self.program
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
                        block,
                        else_block,
                    },
                ty: _,
                span,
            } => {
                let id = {
                    let mut id_ref = FOR_LOOP_ID_COUNTER.write().unwrap();
                    *id_ref += 1;
                    *id_ref
                };

                let label = AstLabel(label.unwrap_or_else(|| format!("dummy_label{}", id)));
                let iter_expr = AstExpression::identifier(span, format!("iter{}", id));
                let item_expr = AstExpression::identifier(span, format!("item{}", id));
                let next_expr = AstExpression::identifier(span, format!("next{}", id));
                let iter_pat =
                    AstMatchPattern::identifier(span, format!("iter{}", id), AstType::infer());
                let item_pat =
                    AstMatchPattern::identifier(span, format!("item{}", id), AstType::infer());
                let next_pat =
                    AstMatchPattern::identifier(span, format!("next{}", id), AstType::infer());

                let for_loop = cheshire_quote!(
                    &mut self.program,
                    "{{
                        let ({item_pat}, {iter_pat}) = {iterable}:iterator():next().
                        {label} while {item_expr}:is_some() {{
                            let {pattern} = {item_expr}:unwrap().
                            {block}
                            let {next_pat} = {iter_expr}:next().
                            {item_expr} = {next_expr}:0.
                            {iter_expr} = {next_expr}:1.
                        }} else {else_block}
                    }}",
                    iterable = *iterable,
                    label = label,
                    iter_expr = iter_expr,
                    item_expr = item_expr,
                    next_expr = next_expr,
                    iter_pat = iter_pat,
                    item_pat = item_pat,
                    next_pat = next_pat,
                    pattern = pattern,
                    block = block,
                    else_block = else_block,
                );

                Ok(for_loop)
            },
            e => Ok(e),
        }
    }
}

lazy_static! {
    static ref FOR_LOOP_ID_COUNTER: RwLock<usize> = RwLock::new(0);
}
