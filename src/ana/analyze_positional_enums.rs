use crate::ana::represent::AnalyzedProgram;
use crate::ana::represent_visitor::PureAnalysisPass;
use crate::parser::ast::{AstExpression, AstExpressionData, AstMatchPattern};
use crate::parser::ast_visitor::AstAdapter;
use crate::util::PResult;
use std::collections::HashMap;

pub struct AnalyzePositionalEnums(AnalyzedProgram);

impl PureAnalysisPass for AnalyzePositionalEnums {
    fn new(a: AnalyzedProgram) -> PResult<AnalyzePositionalEnums> {
        Ok(AnalyzePositionalEnums(a))
    }

    fn drop(self) -> AnalyzedProgram {
        self.0
    }
}

impl AstAdapter for AnalyzePositionalEnums {
    fn enter_expression(&mut self, e: AstExpression) -> PResult<AstExpression> {
        let AstExpression { data, ty, span } = e;

        let data = match data {
            AstExpressionData::PlainEnum {
                enumerable,
                generics,
                variant,
            } => AstExpressionData::PositionalEnum {
                enumerable,
                generics,
                variant,
                children: Vec::new(),
            },
            AstExpressionData::NamedEnum {
                enumerable,
                generics,
                variant,
                children,
            } => {
                let ordering = self.0.analyzed_enums[&enumerable].variants[&variant]
                    .field_names
                    .as_ref()
                    .unwrap();

                AstExpressionData::PositionalEnum {
                    enumerable,
                    generics,
                    variant,
                    children: arrange(ordering, children, None),
                }
            }
            e => e,
        };

        Ok(AstExpression { data, ty, span })
    }

    fn enter_pattern(&mut self, p: AstMatchPattern) -> PResult<AstMatchPattern> {
        match p {
            AstMatchPattern::PlainEnum {
                enumerable,
                generics,
                variant,
            } => Ok(AstMatchPattern::PositionalEnum {
                enumerable,
                generics,
                variant,
                children: Vec::new(),
                ignore_rest: false,
            }),
            AstMatchPattern::PositionalEnum {
                enumerable,
                variant,
                generics,
                children,
                ignore_rest: true,
            } => {
                let expected = self.0.analyzed_enums[&enumerable].variants[&variant]
                    .fields
                    .len();

                Ok(AstMatchPattern::PositionalEnum {
                    enumerable,
                    generics,
                    variant,
                    children: augment(children, expected, AstMatchPattern::Underscore),
                    ignore_rest: false,
                })
            }
            AstMatchPattern::NamedEnum {
                enumerable,
                generics,
                variant,
                children,
                ..
            } => {
                let ordering = self.0.analyzed_enums[&enumerable].variants[&variant]
                    .field_names
                    .as_ref()
                    .unwrap();

                Ok(AstMatchPattern::PositionalEnum {
                    enumerable,
                    generics,
                    variant,
                    children: arrange(ordering, children, Some(AstMatchPattern::Underscore)),
                    ignore_rest: false,
                })
            }
            p => Ok(p),
        }
    }
}

fn arrange<T: Clone>(
    ordering: &HashMap<String, usize>,
    items: HashMap<String, T>,
    fill_none: Option<T>,
) -> Vec<T> {
    let mut out: Vec<Option<T>> = (0..ordering.len()).map(|_| None).collect();

    for (name, item) in items {
        out[ordering[&name]] = Some(item);
    }

    out.into_iter()
        .map(|x| x.or_else(|| fill_none.clone()).unwrap())
        .collect()
}

fn augment<T: Clone>(mut given: Vec<T>, expected: usize, augment: T) -> Vec<T> {
    for _ in given.len()..expected {
        given.push(augment.clone());
    }

    given
}
