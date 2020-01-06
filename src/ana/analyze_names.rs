use crate::ana::represent::{AnEnumData, AnObjectData, AnTraitData, AnalyzedProgram};
use crate::ana::represent_visitor::{AnAdapter, DirtyAnalysisPass};
use crate::parser::ast::{AstExpression, AstExpressionData, AstTraitType, AstType, ModuleRef};
use crate::parser::ast_visitor::AstAdapter;
use crate::util::{IntoError, PResult};
use std::collections::HashMap;

pub struct AnalyzeNames {
    analyzed_objects: HashMap<ModuleRef, AnObjectData>,
    analyzed_enums: HashMap<ModuleRef, AnEnumData>,
    analyzed_traits: HashMap<ModuleRef, AnTraitData>,
}

impl DirtyAnalysisPass for AnalyzeNames {
    fn new(a: &AnalyzedProgram) -> PResult<AnalyzeNames> {
        Ok(AnalyzeNames {
            analyzed_objects: a.analyzed_objects.clone(),
            analyzed_enums: a.analyzed_enums.clone(),
            analyzed_traits: a.analyzed_traits.clone(),
        })
    }
}

impl AnAdapter for AnalyzeNames {}

impl AstAdapter for AnalyzeNames {
    fn enter_type(&mut self, t: AstType) -> PResult<AstType> {
        match t {
            AstType::ObjectEnum(name, generics) => {
                if self.analyzed_objects.contains_key(&name) {
                    Ok(AstType::object(name, generics))
                } else if self.analyzed_enums.contains_key(&name) {
                    Ok(AstType::enumerable(name, generics))
                } else {
                    PResult::error(format!(
                        "No such object or enum named `{}`.",
                        name.full_name()?
                    ))
                }
            }
            t => Ok(t),
        }
    }

    fn enter_trait_type(&mut self, t: AstTraitType) -> PResult<AstTraitType> {
        if !self.analyzed_traits.contains_key(&t.name) {
            return PResult::error(format!("No such trait named `{}`.", t.name.full_name()?));
        }

        Ok(t)
    }

    fn enter_expression(&mut self, e: AstExpression) -> PResult<AstExpression> {
        match &e.data {
            AstExpressionData::AllocateObject { object, .. } => {
                if !self.analyzed_objects.contains_key(object) {
                    return PResult::error(format!(
                        "No such object named `{}`.",
                        object.full_name()?
                    ));
                }
            }
            _ => {}
        }

        Ok(e)
    }
}
