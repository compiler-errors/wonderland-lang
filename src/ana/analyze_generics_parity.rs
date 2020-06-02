use crate::{
    ana::{
        represent::AnalyzedProgram,
        represent_visitor::{AnAdapter, DirtyAnalysisPass},
    },
    ast::{visitor::AstAdapter, *},
    util::{PResult, Span},
};
use std::collections::HashMap;

pub struct AnalyzeGenericsParity {
    parity: HashMap<ModuleRef, usize>,
}

impl AnalyzeGenericsParity {
    fn check_generics(
        &self,
        span: Option<Span>,
        m: &ModuleRef,
        generics: Vec<AstType>,
    ) -> PResult<Vec<AstType>> {
        let expected = self.parity[m];

        if generics.len() == expected {
            Ok(generics)
        } else if generics.len() == 0 {
            Ok((0..expected).map(|_| AstType::infer()).collect())
        } else if let Some(span) = span {
            perror_at!(
                span,
                "Incorrect number of generics for symbol `{}`. Expected {}, found {}.",
                m.full_name(),
                expected,
                generics.len()
            )
        } else {
            perror!(
                // Unspanned
                "Incorrect number of generics for symbol `{}`. Expected {}, found {}.",
                m.full_name(),
                expected,
                generics.len()
            )
        }
    }
}

impl DirtyAnalysisPass for AnalyzeGenericsParity {
    fn new(a: &AnalyzedProgram) -> PResult<AnalyzeGenericsParity> {
        Ok(AnalyzeGenericsParity {
            parity: ((a.analyzed_functions)
                .iter()
                .map(|(r, i)| (r.clone(), i.generics.len())))
            .chain(
                a.analyzed_objects
                    .iter()
                    .map(|(r, i)| (r.clone(), i.generics.len())),
            )
            .chain(
                a.analyzed_enums
                    .iter()
                    .map(|(r, i)| (r.clone(), i.generics.len())),
            )
            .chain(
                a.analyzed_traits
                    .iter()
                    .map(|(r, i)| (r.clone(), i.generics.len())),
            )
            .collect(),
        })
    }
}

impl AnAdapter for AnalyzeGenericsParity {}

impl AstAdapter for AnalyzeGenericsParity {
    fn enter_ast_type(&mut self, t: AstType) -> PResult<AstType> {
        match t {
            AstType::Object(object, generics) => {
                let generics = self.check_generics(None, &object, generics)?;
                Ok(AstType::Object(object, generics))
            },
            AstType::Enum(enumerable, generics) => {
                let generics = self.check_generics(None, &enumerable, generics)?;
                Ok(AstType::Enum(enumerable, generics))
            },
            t => Ok(t),
        }
    }

    fn enter_ast_trait_type(&mut self, mut t: AstTraitType) -> PResult<AstTraitType> {
        t.generics = self.check_generics(None, &t.name, t.generics)?;
        Ok(t)
    }

    fn enter_ast_expression(&mut self, e: AstExpression) -> PResult<AstExpression> {
        let AstExpression { data, ty, span } = e;

        let data = match data {
            AstExpressionData::FnCall {
                fn_name,
                generics,
                args,
            } => {
                let generics = self.check_generics(Some(span), &fn_name, generics)?;
                AstExpressionData::FnCall {
                    fn_name,
                    args,
                    generics,
                }
            },
            AstExpressionData::PlainEnum {
                enumerable,
                generics,
                variant,
            } => {
                let generics = self.check_generics(Some(span), &enumerable, generics)?;
                AstExpressionData::PlainEnum {
                    enumerable,
                    generics,
                    variant,
                }
            },
            AstExpressionData::PositionalEnum {
                enumerable,
                generics,
                variant,
                children,
            } => {
                let generics = self.check_generics(Some(span), &enumerable, generics)?;
                AstExpressionData::PositionalEnum {
                    enumerable,
                    generics,
                    variant,
                    children,
                }
            },
            AstExpressionData::NamedEnum {
                enumerable,
                generics,
                variant,
                children,
            } => {
                let generics = self.check_generics(Some(span), &enumerable, generics)?;
                AstExpressionData::NamedEnum {
                    enumerable,
                    generics,
                    variant,
                    children,
                }
            },
            AstExpressionData::AllocateObject {
                object,
                generics,
                children,
                children_idxes,
            } => {
                let generics = self.check_generics(Some(span), &object, generics)?;
                AstExpressionData::AllocateObject {
                    object,
                    generics,
                    children,
                    children_idxes,
                }
            },
            d => d,
        };

        Ok(AstExpression { data, ty, span })
    }

    fn enter_ast_match_pattern(&mut self, p: AstMatchPattern) -> PResult<AstMatchPattern> {
        let AstMatchPattern { span, data, ty } = p;

        let data = match data {
            AstMatchPatternData::PlainEnum {
                enumerable,
                generics,
                variant,
            } => {
                let generics = self.check_generics(None, &enumerable, generics)?;
                AstMatchPatternData::PlainEnum {
                    enumerable,
                    generics,
                    variant,
                }
            },
            AstMatchPatternData::PositionalEnum {
                enumerable,
                generics,
                variant,
                children,
                ignore_rest,
            } => {
                let generics = self.check_generics(None, &enumerable, generics)?;
                AstMatchPatternData::PositionalEnum {
                    enumerable,
                    generics,
                    variant,
                    children,
                    ignore_rest,
                }
            },
            AstMatchPatternData::NamedEnum {
                enumerable,
                generics,
                variant,
                children,
                ignore_rest,
            } => {
                let generics = self.check_generics(None, &enumerable, generics)?;
                AstMatchPatternData::NamedEnum {
                    enumerable,
                    generics,
                    variant,
                    children,
                    ignore_rest,
                }
            },
            p => p,
        };

        Ok(AstMatchPattern { span, data, ty })
    }
}
