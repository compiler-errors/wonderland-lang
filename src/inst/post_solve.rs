use crate::ana::represent::AnalyzedProgram;
use crate::parser::ast::{AstExpression, AstExpressionData, AstType};
use crate::parser::ast_visitor::AstAdapter;
use crate::util::{IntoError, PResult};
use std::rc::Rc;

pub struct PostSolveAdapter(pub Rc<AnalyzedProgram>);

impl AstAdapter for PostSolveAdapter {
    fn exit_type(&mut self, t: AstType) -> PResult<AstType> {
        match &t {
            AstType::AssociatedType { .. }
            | AstType::Dummy(..)
            | AstType::DummyGeneric(..)
            | AstType::GenericPlaceholder(..)
            | AstType::SelfType => unreachable!(),
            AstType::Infer(_) => {
                return PResult::error("Insufficient information to infer types".into());
            }
            _ => {}
        }

        Ok(t)
    }

    fn exit_expression(&mut self, t: AstExpression) -> PResult<AstExpression> {
        let AstExpression { span, ty, data } = t;

        let data = match data {
            AstExpressionData::ObjectAccess {
                object,
                mem_name,
                mem_idx: None,
            } => {
                if let AstType::Object(obj_name, _) = &object.ty {
                    let mem_idx = self.0.analyzed_objects[obj_name].member_indices[&mem_name];

                    AstExpressionData::ObjectAccess {
                        object,
                        mem_name,
                        mem_idx: Some(mem_idx),
                    }
                } else {
                    unreachable!()
                }
            }
            e => e,
        };

        Ok(AstExpression { data, ty, span })
    }
}
