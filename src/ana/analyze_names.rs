use crate::{
    ana::{
        represent::{AnEnumData, AnObjectData, AnTraitData, AnalyzedProgram},
        represent_visitor::{AnAdapter, DirtyAnalysisPass},
    },
    ast::{
        ast_visitor::AstAdapter, AstExpression, AstExpressionData, AstTraitType, AstType, ModuleRef,
    },
    util::PResult,
};
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
    fn enter_ast_type(&mut self, t: AstType) -> PResult<AstType> {
        match t {
            AstType::ObjectEnum(name, generics) =>
                if self.analyzed_objects.contains_key(&name) {
                    Ok(AstType::object(name, generics))
                } else if self.analyzed_enums.contains_key(&name) {
                    Ok(AstType::enumerable(name, generics))
                } else {
                    perror!("No such object or enum named `{}`.", name.full_name())
                },
            t => Ok(t),
        }
    }

    fn enter_ast_trait_type(&mut self, t: AstTraitType) -> PResult<AstTraitType> {
        if !self.analyzed_traits.contains_key(&t.name) {
            return perror!("No such trait named `{}`.", t.name.full_name());
        }

        Ok(t)
    }

    fn enter_ast_expression(&mut self, e: AstExpression) -> PResult<AstExpression> {
        match &e.data {
            AstExpressionData::AllocateObject { object, .. } => {
                if !self.analyzed_objects.contains_key(object) {
                    return perror_at!(e.span, "No such object named `{}`.", object.full_name());
                }
            },
            _ => {},
        }

        Ok(e)
    }
}
