use crate::parser::{Adapter, AstType};
use crate::util::result::PResult;
use crate::util::Counter;

pub struct InferAdapter {
    id_counter: Counter,
}

impl InferAdapter {
    pub fn new() -> InferAdapter {
        InferAdapter {
            id_counter: Counter::new(0),
        }
    }
}

impl Adapter for InferAdapter {
    fn enter_type(&mut self, t: AstType) -> PResult<AstType> {
        Ok(match t {
            AstType::Infer => AstType::InferPlaceholder(self.id_counter.next()),
            t => t,
        })
    }
}
