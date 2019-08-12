use crate::parser::{Adapter, AstType};
use crate::util::result::PResult;
use crate::util::Counter;

struct InferAdapter {
    id_counter: Counter,
}

impl Adapter for InferAdapter {
    fn enter_type(&mut self, t: AstType) -> PResult<AstType> {
        Ok(match t {
            AstType::Infer => AstType::InferPlaceholder(self.id_counter.next()),
            t => t,
        })
    }
}
