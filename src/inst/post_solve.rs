use crate::parser::ast_visitor::Adapter;
use crate::parser::ast::{AstType, AstExpression, AstExpressionData};
use crate::util::result::PResult;
use crate::tyck::TyckSolution;

pub struct PostSolveAdapter(pub TyckSolution);

impl Adapter for PostSolveAdapter {
    fn exit_type(&mut self, t: AstType) -> PResult<AstType> {
        self.0.normalize_ty(&t)
    }

    fn exit_expression(&mut self, t: AstExpression) -> PResult<AstExpression> {
        let AstExpression { span, ty, data } = t;

        let data = match data {
            AstExpressionData::StaticCall { call_type, associated_trait, impl_signature, fn_name, fn_generics, args } => {
                let expected_signature = self.0.get_impl_signature(span, &call_type, associated_trait.as_ref().unwrap())?;

                if let Some(given_signature) = impl_signature {
                    if given_signature != expected_signature {
                        panic!("TODO: Error");
                    }
                }

                AstExpressionData::StaticCall { call_type, fn_name, fn_generics, args, associated_trait, impl_signature: Some(expected_signature) }
            }
            e => e,
        };

        Ok(AstExpression {
            data,
            ty,
            span
        })
    }
}