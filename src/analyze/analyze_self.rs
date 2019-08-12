use crate::parser::*;
use crate::util::result::{PError, PResult};

struct SelfAdapter {
    nested: usize,
}

impl Adapter for SelfAdapter {
    fn enter_type(&mut self, t: AstType) -> PResult<AstType> {
        match t {
            AstType::SelfType => {
                if self.nested == 0 {
                    PError::new(0, format!("No `Self` type in non-object environment"))?;
                }
            }
            _ => {}
        }

        Ok(t)
    }

    fn enter_object(&mut self, o: AstObject) -> PResult<AstObject> {
        self.nested += 1;

        Ok(o)
    }

    fn enter_trait(&mut self, t: AstTrait) -> PResult<AstTrait> {
        self.nested += 1;

        Ok(t)
    }

    fn enter_impl(&mut self, i: AstImpl) -> PResult<AstImpl> {
        self.nested += 1;

        Ok(i)
    }

    fn exit_object(&mut self, o: AstObject) -> PResult<AstObject> {
        self.nested -= 1;

        Ok(o)
    }

    fn exit_trait(&mut self, t: AstTrait) -> PResult<AstTrait> {
        self.nested -= 1;

        Ok(t)
    }

    fn exit_impl(&mut self, i: AstImpl) -> PResult<AstImpl> {
        self.nested -= 1;

        Ok(i)
    }
}
