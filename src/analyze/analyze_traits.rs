use crate::parser::ast::*;
use crate::parser::ast_visitor::*;
use crate::util::result::*;
use crate::util::Span;
use std::collections::HashMap;

pub struct TraitsAdapter<'a> {
    function_generic_count: &'a HashMap<String, usize>,
    trait_generic_count: &'a HashMap<String, usize>,
    trait_method_generic_count: &'a HashMap<(String, String), usize>,
    object_generic_count: &'a HashMap<String, usize>,
    method_to_trait: &'a HashMap<String, String>,
    type_to_trait: &'a HashMap<String, String>,
}

impl<'a> TraitsAdapter<'a> {
    pub fn new(
        function_generic_count: &'a HashMap<String, usize>,
        trait_generic_count: &'a HashMap<String, usize>,
        trait_method_generic_count: &'a HashMap<(String, String), usize>,
        object_generic_count: &'a HashMap<String, usize>,
        method_to_trait: &'a HashMap<String, String>,
        type_to_trait: &'a HashMap<String, String>,
    ) -> TraitsAdapter<'a> {
        TraitsAdapter {
            function_generic_count,
            trait_generic_count,
            trait_method_generic_count,
            object_generic_count,
            method_to_trait,
            type_to_trait,
        }
    }

    fn expect_generics(
        span: Span,
        generics: Vec<AstType>,
        expected: usize,
    ) -> PResult<Vec<AstType>> {
        let got = generics.len();

        if got == expected {
            Ok(generics.clone())
        } else if got == 0 {
            Ok((0..expected).map(|_| AstType::infer()).collect())
        } else {
            PError::new(
                span,
                format!("Mismatched generics: Expected {}, got {}.", expected, got),
            )
        }
    }
}

impl<'a> Adapter for TraitsAdapter<'a> {
    fn enter_type(&mut self, t: AstType) -> PResult<AstType> {
        match t {
            AstType::Object(name, generics) => {
                let expected = self.object_generic_count.get(&name).is_expected(
                    Span::new(0, 0),
                    "object",
                    &name,
                )?;

                Ok(AstType::Object(
                    name,
                    Self::expect_generics(Span::new(0, 0), generics, *expected)?,
                ))
            }
            AstType::AssociatedType {
                obj_ty,
                trait_ty,
                name,
            } => {
                let trait_ty = if trait_ty.is_none() {
                    let trait_name = self.type_to_trait.get(&name).is_expected(
                        Span::new(0, 0),
                        "associated type",
                        &name,
                    )?;
                    Some(AstTraitType(trait_name.clone(), Vec::new()))
                } else {
                    trait_ty
                };

                Ok(AstType::AssociatedType {
                    obj_ty,
                    trait_ty,
                    name,
                })
            }
            t => Ok(t),
        }
    }

    fn enter_expression(&mut self, e: AstExpression) -> PResult<AstExpression> {
        let AstExpression { data, ty, span } = e;

        let data = match data {
            AstExpressionData::Call {
                name,
                generics,
                args,
            } => {
                let expected = self
                    .function_generic_count
                    .get(&name)
                    .is_expected(e.span, "function", &name)?;
                let generics = Self::expect_generics(e.span, generics, *expected)?;

                AstExpressionData::Call {
                    name,
                    generics,
                    args,
                }
            }
            AstExpressionData::ObjectCall {
                object,
                fn_name,
                generics,
                mut args,
            } => {
                // Insert 'self' parameter.
                args.insert(0, *object);

                // Recursively visits static call
                return AstExpression::static_call(span, AstType::infer(), fn_name, generics, args)
                    .visit(self);
            }
            AstExpressionData::StaticCall {
                call_type,
                fn_name,
                fn_generics,
                args,
                associated_trait,
                impl_signature,
            } => {
                let trait_name = self
                    .method_to_trait
                    .get(&fn_name)
                    .is_expected(e.span, "trait method", &fn_name)?
                    .clone();

                if let Some(AstTraitType( expected_trait_name, ..)) = associated_trait {
                    if expected_trait_name != trait_name {
                        PError::new(span, format!("Expected trait `{}` to be assocated with static call `{}`, got `{}`.", expected_trait_name, fn_name, trait_name))?;
                    }
                }

                let expected_fn = self
                    .trait_method_generic_count
                    .get(&(trait_name.clone(), fn_name.clone()))
                    .is_expected(e.span, "method", &fn_name)?;
                let fn_generics = Self::expect_generics(e.span, fn_generics, *expected_fn)?;

                AstExpressionData::StaticCall {
                    call_type,
                    fn_name,
                    fn_generics,
                    args,
                    associated_trait: Some(AstTraitType(
                        trait_name,
                        Vec::new(), /* Infer later. */
                    )),
                    impl_signature,
                }
            }
            t => t,
        };

        Ok(AstExpression { data, ty, span })
    }

    fn enter_trait_type(&mut self, i: AstTraitType) -> PResult<AstTraitType> {
        let AstTraitType(name, generics) = i;

        let expected =
            self.trait_generic_count
                .get(&name)
                .is_expected(Span::new(0, 0), "trait", &name)?;

        let generics = Self::expect_generics(Span::new(0, 0), generics, *expected)?;

        Ok(AstTraitType(name, generics))
    }
}
