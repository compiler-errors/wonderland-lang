use crate::{
    ana::{represent::AnalyzedProgram, represent_visitor::PureAnalysisPass},
    ast::{
        visitor::AstAdapter, AstExpression, AstExpressionData, AstFunction, AstObjectFunction,
        AstProgram, LoopId,
    },
    util::{PResult, Span, Visit},
};

pub struct AnalyzeControlFlow {
    program: AnalyzedProgram,
    loop_context: Vec<Option<(Option<String>, LoopId)>>,
    await_context: Vec<bool>,
    return_context: Vec<bool>,
    is_fragment: bool,
}

impl PureAnalysisPass for AnalyzeControlFlow {
    fn new(program: AnalyzedProgram) -> PResult<AnalyzeControlFlow> {
        Ok(AnalyzeControlFlow {
            program,
            loop_context: vec![],
            await_context: vec![],
            return_context: vec![],
            // We do this so we can analyze pure expressions (i.e. in a quasiquote)
            // We can explicitly deny AstGlobals...
            is_fragment: true,
        })
    }

    fn drop(self) -> AnalyzedProgram {
        self.program
    }
}

impl AnalyzeControlFlow {
    fn find_loop_label(&self, span: Span, label: Option<&str>) -> PResult<LoopId> {
        for x in self.loop_context.iter().rev() {
            if let Some((name, id)) = x {
                if label.is_none() || label == name.as_deref() {
                    return Ok(*id);
                }
            } else {
                break;
            }
        }

        if let Some(label) = label {
            perror_at!(span, "Couldn't find loop with label `[{}]`", label)
        } else {
            perror_at!(span, "Couldn't find loop to break/continue")
        }
    }
}

impl AstAdapter for AnalyzeControlFlow {
    fn enter_ast_program(&mut self, p: AstProgram) -> PResult<AstProgram> {
        // TODO: I might need to do this for fns, etc.
        // This is probably fine, bc any time we haven't processed a WHOLE AstProgram,
        // we're in a quote, which means I'm probably being a bit more careful...
        self.is_fragment = false;
        Ok(p)
    }

    fn enter_ast_function(&mut self, f: AstFunction) -> PResult<AstFunction> {
        self.return_context.push(true);
        Ok(f)
    }

    fn enter_ast_object_function(&mut self, o: AstObjectFunction) -> PResult<AstObjectFunction> {
        self.return_context.push(true);
        Ok(o)
    }

    fn exit_ast_function(&mut self, f: AstFunction) -> PResult<AstFunction> {
        self.return_context.pop();
        Ok(f)
    }

    fn exit_ast_object_function(&mut self, o: AstObjectFunction) -> PResult<AstObjectFunction> {
        self.return_context.pop();
        Ok(o)
    }

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
                self.loop_context.push(Some((label.clone(), id)));
                let block = block.visit(self)?;
                self.loop_context.pop();

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
                id: Some(self.find_loop_label(span, label.as_deref())?),
                label,
                value,
            },
            AstExpressionData::Continue { id: None, label } => AstExpressionData::Continue {
                id: Some(self.find_loop_label(span, label.as_deref())?),
                label,
            },
            c @ AstExpressionData::Closure { .. } => {
                self.loop_context.push(None);
                self.return_context.push(true);
                self.await_context.push(false);
                c
            },
            c @ AstExpressionData::Async { .. } => {
                self.loop_context.push(None);
                self.return_context.push(true);
                self.await_context.push(true);
                c
            },
            r @ AstExpressionData::Return { .. } => {
                if !*self.return_context.last().unwrap_or(&self.is_fragment) {
                    return perror_at!(span, "Cannot return in this context");
                }
                r
            },
            t @ AstExpressionData::Throw { .. } => {
                if !*self.return_context.last().unwrap_or(&self.is_fragment) {
                    return perror_at!(span, "Cannot throw (`?`) in this context");
                }
                t
            },
            a @ AstExpressionData::Await { .. } => {
                if !*self.await_context.last().unwrap_or(&self.is_fragment) {
                    return perror_at!(span, "Cannot await in this context");
                }
                a
            },
            e => e,
        };

        Ok(AstExpression { data, ty, span })
    }

    fn exit_ast_expression(&mut self, e: AstExpression) -> PResult<AstExpression> {
        match e.data {
            AstExpressionData::Closure { .. } => {
                self.loop_context.pop();
                self.await_context.pop();
                self.return_context.pop();
            },
            AstExpressionData::Async { .. } => {
                self.loop_context.pop();
                self.await_context.pop();
                self.return_context.pop();
            },
            _ => {},
        }

        Ok(e)
    }
}
