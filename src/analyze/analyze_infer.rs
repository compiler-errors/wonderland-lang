use crate::parser::{Adapter, AstExpression, AstType};
use crate::util::result::PResult;
use crate::util::Counter;

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
}
