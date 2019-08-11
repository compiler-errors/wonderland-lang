use crate::parser::*;

struct SelfAdapter {
    nested: usize,
}

impl Adapter for SelfAdapter {
    fn enter_type(&mut self, t: AstType) -> AstType {
        match t {
            AstType::SelfType => {
                if self.nested == 0 {
                    panic!("TODO: Error");
                }
            }
            _ => {}
        }

        t
    }

    fn enter_object(&mut self, o: AstObject) -> AstObject {
        self.nested += 1;
        o
    }

    fn enter_trait(&mut self, t: AstTrait) -> AstTrait {
        self.nested += 1;
        t
    }

    fn enter_impl(&mut self, i: AstImpl) -> AstImpl {
        self.nested += 1;
        i
    }

    fn exit_object(&mut self, o: AstObject) -> AstObject {
        self.nested -= 1;
        o
    }

    fn exit_trait(&mut self, t: AstTrait) -> AstTrait {
        self.nested -= 1;
        t
    }

    fn exit_impl(&mut self, i: AstImpl) -> AstImpl {
        self.nested -= 1;
        i
    }
}
