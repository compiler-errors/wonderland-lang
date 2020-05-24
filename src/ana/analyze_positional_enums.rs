use crate::{
    ana::{represent::AnalyzedProgram, represent_visitor::PureAnalysisPass},
    ast::{
        ast_visitor::AstAdapter, AstExpression, AstExpressionData, AstMatchPattern,
        AstMatchPatternData, AstType,
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
                children: vec![],
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
                    children: arrange(ordering, children, || None),
                }
            },
            e => e,
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
            } => AstMatchPatternData::PositionalEnum {
                enumerable,
                generics,
                variant,
                children: vec![],
                ignore_rest: false,
            },
            AstMatchPatternData::PositionalEnum {
                enumerable,
                variant,
                generics,
                children,
                ignore_rest,
            } => {
                let expected = self.0.analyzed_enums[&enumerable].variants[&variant]
                    .fields
                    .len();

                if !ignore_rest && expected != children.len() {
                    return perror_at!(
                        span,
                        "In enum `{}!{}`, expected `{}` members, but found `{}`.",
                        enumerable.full_name(),
                        variant,
                        expected,
                        children.len()
                    );
                }

                AstMatchPatternData::PositionalEnum {
                    enumerable,
                    generics,
                    variant,
                    children: augment(children, expected, || {
                        AstMatchPattern::underscore(span, AstType::infer())
                    }),
                    ignore_rest: false,
                }
            },
            AstMatchPatternData::NamedEnum {
                enumerable,
                generics,
                variant,
                children,
                ignore_rest,
                ..
            } => {
                let expected = self.0.analyzed_enums[&enumerable].variants[&variant]
                    .fields
                    .len();
                let ordering = self.0.analyzed_enums[&enumerable].variants[&variant]
                    .field_names
                    .as_ref()
                    .unwrap();

                if !ignore_rest && expected != children.len() {
                    return perror_at!(
                        span,
                        "In enum `{}!{}`, expected `{}` members, but found `{}`.",
                        enumerable.full_name(),
                        variant,
                        expected,
                        children.len()
                    );
                }

                AstMatchPatternData::PositionalEnum {
                    enumerable,
                    generics,
                    variant,
                    children: arrange(ordering, children, || {
                        if ignore_rest {
                            Some(AstMatchPattern::underscore(span, AstType::infer()))
                        } else {
                            None
                        }
                    }),
                    ignore_rest: false,
                }
            },
            p => p,
        };

        Ok(AstMatchPattern { span, data, ty })
    }
}

fn arrange<T, F>(
    ordering: &HashMap<String, usize>,
    items: HashMap<String, T>,
    fill_none: F,
) -> Vec<T>
where
    F: Copy + Fn() -> Option<T>,
{
    let mut out: Vec<Option<T>> = (0..ordering.len()).map(|_| None).collect();

    for (name, item) in items {
        out[ordering[&name]] = Some(item);
    }

    out.into_iter()
        .map(|x| x.or_else(fill_none).unwrap())
        .collect()
}

fn augment<T, F>(mut given: Vec<T>, expected: usize, fill: F) -> Vec<T>
where
    F: Fn() -> T,
{
    for _ in given.len()..expected {
        given.push(fill());
    }

    given
}
