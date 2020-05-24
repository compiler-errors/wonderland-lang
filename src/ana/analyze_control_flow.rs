use crate::{
    ana::represent_visitor::AstAnalysisPass,
    ast::{ast_visitor::AstAdapter, AstExpression, AstExpressionData, LoopId},
    util::{PResult, Visit},
};

pub struct AnalyzeControlFlow(Vec<Option<(Option<String>, LoopId)>>);

impl AstAnalysisPass for AnalyzeControlFlow {
    fn new() -> AnalyzeControlFlow {
        AnalyzeControlFlow(vec![None])
    }
}

impl AnalyzeControlFlow {
    fn find(&self, label: Option<&str>) -> PResult<LoopId> {
        for x in self.0.iter().rev() {
            if let Some((name, id)) = x {
                if label.is_none() || label == name.as_deref() {
                    return Ok(*id);
                }
            } else {
                break;
            }
        }

        if let Some(label) = label {
            perror!("Couldn't find loop with label `[{}]`", label)
        } else {
            perror!("Couldn't find loop to break/continue")
        }
    }
}

impl AstAdapter for AnalyzeControlFlow {
    fn enter_ast_expression(&mut self, e: AstExpression) -> PResult<AstExpression> {
        let AstExpression { data, ty, span } = e;

        let data = match data {
            AstExpressionData::While {
                label,
                id,
                condition,
                block,
                else_block,
            } => {
                self.0.push(Some((label.clone(), id)));
                let block = block.visit(self)?;
                self.0.pop();

                AstExpressionData::While {
                    label,
                    id,
                    condition,
                    block,
                    else_block,
                }
            },
            AstExpressionData::Break {
                id: None,
                label,
                value,
            } => AstExpressionData::Break {
                id: Some(self.find(label.as_deref())?),
                label,
                value,
            },
            AstExpressionData::Continue { id: None, label } => AstExpressionData::Continue {
                id: Some(self.find(label.as_deref())?),
                label,
            },
            c @ AstExpressionData::Closure { .. } => {
                self.0.push(None);
                c
            },
            e => e,
        };

        Ok(AstExpression { data, ty, span })
    }

    fn exit_ast_expression(&mut self, e: AstExpression) -> PResult<AstExpression> {
        match e.data {
            AstExpressionData::Closure { .. } => {
                self.0.pop();
            },
            _ => {},
        }

        Ok(e)
    }
}
