use crate::{
    ana::{
        represent::AnalyzedProgram,
        represent_visitor::{AnAdapter, DirtyAnalysisPass},
    },
    parser::{
        ast::{AstExpression, AstExpressionData, AstType},
        ast_visitor::AstAdapter,
    },
    util::PResult,
};

pub struct AnalyzeElaborations;

impl DirtyAnalysisPass for AnalyzeElaborations {
    fn new(_: &AnalyzedProgram) -> PResult<AnalyzeElaborations> {
        Ok(AnalyzeElaborations)
    }
}

impl AnAdapter for AnalyzeElaborations {}

impl AstAdapter for AnalyzeElaborations {
    fn enter_expression(&mut self, e: AstExpression) -> PResult<AstExpression> {
        let AstExpression { data, ty, span } = e;

        let data = match data {
            AstExpressionData::StaticCall {
                call_type: AstType::ElaboratedType { obj_ty, trait_ty },
                fn_name,
                fn_generics,
                args,
                associated_trait,
                impl_signature,
            } => {
                if associated_trait.is_some() && associated_trait.unwrap() != trait_ty {
                    return perror_at!(
                        span,
                        "Static call has incongruous trait type with its elaborated calling type."
                    );
                }

                AstExpressionData::StaticCall {
                    call_type: *obj_ty,
                    fn_name,
                    fn_generics,
                    args,
                    associated_trait: Some(trait_ty),
                    impl_signature,
                }
            },
            e => e,
        };

        Ok(AstExpression { data, ty, span })
    }

    fn enter_type(&mut self, t: AstType) -> PResult<AstType> {
        match t {
            AstType::AssociatedType {
                obj_ty,
                trait_ty: associated_trait_ty,
                name,
            } => match *obj_ty {
                AstType::ElaboratedType {
                    obj_ty,
                    trait_ty: elaborated_trait_ty,
                } => {
                    if associated_trait_ty.is_some()
                        && associated_trait_ty.unwrap() != elaborated_trait_ty
                    {
                        return perror!(
                            "Associated type has incongruous trait type with its elaborated type."
                        );
                    }

                    Ok(AstType::AssociatedType {
                        obj_ty,
                        trait_ty: Some(elaborated_trait_ty),
                        name,
                    })
                },
                obj_ty => Ok(AstType::AssociatedType {
                    obj_ty: Box::new(obj_ty),
                    trait_ty: associated_trait_ty,
                    name,
                }),
            },
            AstType::ElaboratedType { .. } => perror!(
                "Elaborated type not expected here! Only expected in associated types and static \
                 calls!"
            ),
            t => Ok(t),
        }
    }
}
