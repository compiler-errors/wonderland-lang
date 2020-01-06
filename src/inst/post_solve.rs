use crate::ana::represent::AnalyzedProgram;
use crate::parser::ast::{AstExpression, AstExpressionData, AstType};
use crate::parser::ast_visitor::AstAdapter;
use crate::tyck::TyckSolution;
use crate::util::{IntoError, PResult};
use std::rc::Rc;

pub struct PostSolveAdapter(pub TyckSolution, pub Rc<AnalyzedProgram>);

impl AstAdapter for PostSolveAdapter {
    fn exit_type(&mut self, t: AstType) -> PResult<AstType> {
        let ty = self.0.normalize_ty(&t)?;

        match &ty {
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

        Ok(ty)
    }

    fn exit_expression(&mut self, t: AstExpression) -> PResult<AstExpression> {
        let AstExpression { span, ty, data } = t;

        let data = match data {
            AstExpressionData::StaticCall {
                call_type,
                associated_trait,
                impl_signature,
                fn_name,
                fn_generics,
                args,
            } => {
                let expected_signature = self.0.get_impl_signature(
                    span,
                    &call_type,
                    &associated_trait.as_ref().unwrap().trt,
                )?;

                if let Some(given_signature) = impl_signature {
                    if given_signature != expected_signature {
                        panic!("TODO: Error");
                    }
                }

                AstExpressionData::StaticCall {
                    call_type,
                    fn_name,
                    fn_generics,
                    args,
                    associated_trait,
                    impl_signature: Some(expected_signature),
                }
            }
            AstExpressionData::ObjectAccess {
                object,
                mem_name,
                mem_idx: None,
            } => {
                if let AstType::Object(obj_name, _) = &object.ty {
                    let mem_idx = self.1.analyzed_objects[obj_name].member_indices[&mem_name];

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
