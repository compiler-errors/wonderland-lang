use crate::{
    ana::{represent::AnalyzedProgram, represent_visitor::PureAnalysisPass},
    parser::{
        ast::{AstExpression, AstExpressionData, AstMatchPattern, AstMatchPatternData},
        ast_visitor::AstAdapter,
    },
    util::PResult,
};
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
    fn enter_ast_expression(&mut self, e: AstExpression) -> PResult<AstExpression> {
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
            },
            e => e,
        };

        Ok(AstExpression { data, ty, span })
    }

    fn enter_ast_match_pattern(&mut self, p: AstMatchPattern) -> PResult<AstMatchPattern> {
        let AstMatchPattern { data, ty } = p;

        let data = match data {
            AstMatchPatternData::PlainEnum {
                enumerable,
                generics,
                variant,
            } => AstMatchPatternData::PositionalEnum {
                enumerable,
                generics,
                variant,
                children: Vec::new(),
                ignore_rest: false,
            },
            AstMatchPatternData::PositionalEnum {
                enumerable,
                variant,
                generics,
                children,
                ignore_rest: true,
            } => {
                let expected = self.0.analyzed_enums[&enumerable].variants[&variant]
                    .fields
                    .len();

                AstMatchPatternData::PositionalEnum {
                    enumerable,
                    generics,
                    variant,
                    children: augment(children, expected, AstMatchPattern::underscore()),
                    ignore_rest: false,
                }
            },
            AstMatchPatternData::NamedEnum {
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

                AstMatchPatternData::PositionalEnum {
                    enumerable,
                    generics,
                    variant,
                    children: arrange(ordering, children, Some(AstMatchPattern::underscore())),
                    ignore_rest: false,
                }
            },
            p => p,
        };

        Ok(AstMatchPattern { data, ty })
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
