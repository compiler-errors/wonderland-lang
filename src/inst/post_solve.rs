use crate::parser::ast::{AstExpression, AstExpressionData, AstType};
use crate::parser::ast_visitor::Adapter;
use crate::tyck::TyckSolution;
use crate::util::result::{PError, PResult};
use crate::util::Span;
use crate::analyze::represent::AnalyzedFile;
use std::rc::Rc;

pub struct PostSolveAdapter(pub TyckSolution, pub Rc<AnalyzedFile>);

impl Adapter for PostSolveAdapter {
    fn exit_type(&mut self, t: AstType) -> PResult<AstType> {
        let ty = self.0.normalize_ty(&t)?;

        match &ty {
            AstType::AssociatedType { .. }
            | AstType::Dummy(..)
            | AstType::DummyGeneric(..)
            | AstType::GenericPlaceholder(..)
            | AstType::SelfType => unreachable!(),
            AstType::Infer(..) => {
                return PError::new(
                    Span::new(0, 0),
                    "Insufficient information to infer types".to_string(),
                );
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
                    associated_trait.as_ref().unwrap(),
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
            },
            AstExpressionData::ObjectAccess { object, mem_name, mem_idx: None } => {
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
            },
            e => e,
        };

        Ok(AstExpression { data, ty, span })
    }
}
