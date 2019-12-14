use crate::ana::represent::AnalyzedProgram;
use crate::ana::represent_visitor::PureAnalysisPass;
use crate::parser::ast::{AstExpression, AstExpressionData, AstMatchPattern, AstMatchPatternData};
use crate::parser::ast_visitor::AstAdapter;
use crate::util::{IntoError, PResult};
use std::collections::HashMap;

pub struct AnalyzeConstructorFields(AnalyzedProgram);

impl PureAnalysisPass for AnalyzeConstructorFields {
    fn new(a: AnalyzedProgram) -> PResult<AnalyzeConstructorFields> {
        Ok(AnalyzeConstructorFields(a))
    }

    fn drop(self) -> AnalyzedProgram {
        self.0
    }
}

impl AstAdapter for AnalyzeConstructorFields {
    fn enter_expression(&mut self, e: AstExpression) -> PResult<AstExpression> {
        match &e.data {
            AstExpressionData::PlainEnum {
                enumerable,
                variant,
                ..
            } => {
                if !self.0.analyzed_enums[enumerable].variants[variant]
                    .fields
                    .is_empty()
                {
                    return PResult::error(format!(
                        "Trying to construct `{}!{}`, but the variant expects fields",
                        enumerable.full_name()?,
                        variant
                    ));
                }
            }
            AstExpressionData::PositionalEnum {
                enumerable,
                variant,
                children,
                ..
            } => {
                let var_info = &self.0.analyzed_enums[enumerable].variants[variant];
                let found = children.len();
                let expected = var_info.fields.len();

                if found != expected {
                    return PResult::error(format!(
                        "Missing fields from constructor `{}!{}`: found {}, expected {}.",
                        enumerable.full_name()?,
                        variant,
                        found,
                        expected
                    ));
                } else if var_info.field_names.is_some() {
                    return PResult::error(format!(
                        "Trying to construct a positional enum \
                         variant `{}!{}`, but the variant is named",
                        enumerable.full_name()?,
                        variant
                    ));
                }
            }
            AstExpressionData::NamedEnum {
                enumerable,
                variant,
                children,
                ..
            } => {
                let var_info = &self.0.analyzed_enums[enumerable].variants[variant];

                if var_info.field_names.is_none() {
                    return PResult::error(format!(
                        "Trying to construct a positional enum \
                         variant `{}!{}`, but the variant is named",
                        enumerable.full_name()?,
                        variant
                    ));
                }

                check_named_fields(
                    var_info.field_names.as_ref().unwrap(),
                    &children,
                    &format!("{}!{}", enumerable.full_name()?, variant),
                )?;
            }
            AstExpressionData::AllocateObject {
                object, children, ..
            } => {
                let obj_info = &self.0.analyzed_objects[object];
                check_named_fields(&obj_info.member_tys, &children, &object.full_name()?)?;
            }
            _ => {}
        }

        Ok(e)
    }

    fn enter_pattern(&mut self, p: AstMatchPattern) -> PResult<AstMatchPattern> {
        match &p.data {
            AstMatchPatternData::PlainEnum {
                enumerable,
                variant,
                ..
            } => {
                if !self.0.analyzed_enums[enumerable].variants[variant]
                    .fields
                    .is_empty()
                {
                    return PResult::error(format!(
                        "Trying to construct `{}!{}`, but the variant expects fields",
                        enumerable.full_name()?,
                        variant
                    ));
                }
            }
            AstMatchPatternData::PositionalEnum {
                enumerable,
                variant,
                children,
                ignore_rest,
                ..
            } => {
                let var_info = &self.0.analyzed_enums[enumerable].variants[variant];
                let found = children.len();
                let expected = var_info.fields.len();

                if *ignore_rest && found >= expected {
                    return PResult::error(format!(
                        "Too many fields in pattern `{}!{}`: found {}, \
                         expected < {} (due to ellipsis).",
                        enumerable.full_name()?,
                        variant,
                        found,
                        expected
                    ));
                } else if !*ignore_rest && found != expected {
                    return PResult::error(format!(
                        "Missing fields from pattern `{}!{}`: found {}, expected {}.",
                        enumerable.full_name()?,
                        variant,
                        found,
                        expected
                    ));
                } else if var_info.field_names.is_some() {
                    return PResult::error(format!(
                        "Trying to construct a positional enum pattern `{}!{}`, \
                         but the variant is named",
                        enumerable.full_name()?,
                        variant
                    ));
                }
            }
            AstMatchPatternData::NamedEnum {
                enumerable,
                variant,
                children,
                ignore_rest,
                ..
            } => {
                let var_info = &self.0.analyzed_enums[enumerable].variants[variant];
                let found = children.len();
                let expected = var_info.fields.len();

                if var_info.field_names.is_none() {
                    return PResult::error(format!(
                        "Trying to construct a positional enum pattern `{}!{}`, \
                         but the variant is named",
                        enumerable.full_name()?,
                        variant
                    ));
                }

                let fields = var_info.field_names.as_ref().unwrap();

                if !ignore_rest {
                    for field in fields.keys() {
                        if !children.contains_key(field) {
                            return PResult::error(format!(
                                "Missing field {} from pattern `{}!{}`.",
                                field,
                                enumerable.full_name()?,
                                variant
                            ));
                        }
                    }
                } else if found == expected
                /* && ignore_rest */
                {
                    return PResult::error(format!(
                        "In pattern `{}!{}`, all fields are specified, \
                         so the ellipsis is eliding 0 arguments",
                        enumerable.full_name()?,
                        variant
                    ));
                }

                for field in children.keys() {
                    if !fields.contains_key(field) {
                        return PResult::error(format!(
                            "Unexpected field {} in pattern `{}!{}`.",
                            field,
                            enumerable.full_name()?,
                            variant
                        ));
                    }
                }
            }
            _ => {}
        }

        Ok(p)
    }
}

fn check_named_fields<V, V2>(
    expected: &HashMap<String, V>,
    given: &HashMap<String, V2>,
    name: &str,
) -> PResult<()> {
    for field in expected.keys() {
        if !given.contains_key(field) {
            return PResult::error(format!(
                "Missing field `{}` from constructor `{}`.",
                field, name
            ));
        }
    }

    for field in given.keys() {
        if !expected.contains_key(field) {
            return PResult::error(format!(
                "Unexpected field `{}` in constructor `{}`.",
                field, name
            ));
        }
    }

    Ok(())
}
