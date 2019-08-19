use crate::parser::ast::*;
use crate::parser::ast_visitor::Adapter;
use crate::util::result::*;
use std::collections::HashMap;

pub struct InferAdapter;

impl InferAdapter {
    pub fn new() -> InferAdapter {
        InferAdapter
    }
}

impl Adapter for InferAdapter {
    fn enter_expression(&mut self, mut t: AstExpression) -> PResult<AstExpression> {
        if t.ty.is_none() {
            t.ty = Some(AstType::infer());
        }

        Ok(t)
    }

    // TODO: Detect `_` in traits, impls, and fn signatures. Should be straightforward with a subordinate adapter NoInferAdapter...
    // Make sure to attach the span to the error, and also maybe add a .comment with the whole type?
    // Maybe also properly implement Display for type?
}
