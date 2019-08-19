use crate::analyze::represent::*;
use crate::parser::ast::*;
use crate::parser::ast_visitor::*;
use crate::util::result::*;
use crate::util::Span;
use std::collections::HashMap;

pub struct TraitsAdapter<'a> {
    functions: &'a HashMap<String, AnFunctionData>,
    traits: &'a HashMap<String, AnTraitData>,
    objects: &'a HashMap<String, AnObjectData>,
    method_to_trait: &'a HashMap<String, String>,
    type_to_trait: &'a HashMap<String, String>,
}

impl<'a> TraitsAdapter<'a> {
    pub fn new(
        functions: &'a HashMap<String, AnFunctionData>,
        traits: &'a HashMap<String, AnTraitData>,
        objects: &'a HashMap<String, AnObjectData>,
        method_to_trait: &'a HashMap<String, String>,
        type_to_trait: &'a HashMap<String, String>,
    ) -> TraitsAdapter<'a> {
        TraitsAdapter {
            functions,
            traits,
            objects,
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
                let expected = if self.traits.contains_key(&name) {
                    self.traits[&name].generics.len()
                } else if self.objects.contains_key(&name) {
                    self.objects[&name].generics.len()
                } else {
                    PError::new(Span::new(0, 0), format!("Unknown type `{}`", name))?
                };

                Ok(AstType::Object(
                    name,
                    Self::expect_generics(Span::new(0, 0), generics, expected)?,
                ))
            }
            AstType::AssociatedType {
                obj_ty,
                trait_ty,
                name,
            } => {
                if let &AstType::Object(ref name, _) = obj_ty.as_ref() {
                    if self.traits.contains_key(name) {
                        PError::new(
                            Span::new(0, 0),
                            format!("Trait type on LHS of associated type: `{}`", name),
                        )?;
                    }
                }

                let trait_ty = if trait_ty.is_none() {
                    let trait_name = self.type_to_trait.get(&name).expected(
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
                let fun = self
                    .functions
                    .get(&name)
                    .expected(e.span, "function", &name)?;
                let expected = fun.generics.len(); // TODO: this is ugly.
                let generics = Self::expect_generics(e.span, generics, expected)?;

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
            } => {
                let trait_name = self
                    .method_to_trait
                    .get(&fn_name)
                    .expected(e.span, "trait method", &fn_name)?
                    .clone();

                if let &Some(AstTraitType(ref expected_obj_name, ..)) = &associated_trait {
                    if *expected_obj_name != trait_name {
                        PError::new(span, format!("Expected trait `{}` to be assocated with static call `{}`, got `{}`.", expected_obj_name, fn_name, trait_name))?;
                    }
                }

                if let AstType::Object(ref obj_name, ..) = call_type {
                    if self.traits.contains_key(obj_name) {
                        PError::new(
                            span,
                            format!(
                                "LHS `{}` of static call is not an object name, but a trait name.",
                                obj_name
                            ),
                        )?;
                    }
                }

                let trt = self
                    .traits
                    .get(&trait_name)
                    .expected(e.span, "trait", &trait_name)?;
                let fun = trt
                    .methods
                    .get(&fn_name)
                    .expected(e.span, "method", &fn_name)?;
                let expected_fn = fun.generics.len();
                let fn_generics = Self::expect_generics(e.span, fn_generics, expected_fn)?;

                AstExpressionData::StaticCall {
                    call_type,
                    fn_name,
                    fn_generics,
                    args,
                    associated_trait: Some(AstTraitType(
                        trait_name,
                        Vec::new(), /* Infer later. */
                    )),
                }
            }
            t => t,
        };

        Ok(AstExpression { data, ty, span })
    }

    fn enter_trait_type(&mut self, i: AstTraitType) -> PResult<AstTraitType> {
        let name = &i.0;
        self.traits
            .get(name)
            .expected(Span::new(0, 0), "trait", name)?;
        Ok(i)
    }
}
