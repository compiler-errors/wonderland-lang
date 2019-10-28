use crate::ana::represent::AnalyzedProgram;
use crate::ana::represent_visitor::PureAnalysisPass;
use crate::parser::ast::{AstExpression, AstExpressionData, AstTraitType, AstType};
use crate::parser::ast_visitor::AstAdapter;
use crate::util::{IntoError, PResult};

pub struct AnalyzeNames(AnalyzedProgram);

impl PureAnalysisPass for AnalyzeNames {
    fn new(a: AnalyzedProgram) -> PResult<AnalyzeNames> {
        Ok(AnalyzeNames(a))
    }

    fn drop(self) -> AnalyzedProgram {
        self.0
    }
}

impl AstAdapter for AnalyzeNames {
    fn enter_type(&mut self, t: AstType) -> PResult<AstType> {
        if let AstType::Object(name, ..) = &t {
            if !self.0.analyzed_objects.contains_key(name) {
                return PResult::error(format!("No such object named `{}`.", name.full_name()?));
            }
        }

        Ok(t)
    }

    fn enter_trait_type(&mut self, t: AstTraitType) -> PResult<AstTraitType> {
        if !self.0.analyzed_traits.contains_key(&t.0) {
            return PResult::error(format!("No such trait named `{}`.", t.0.full_name()?));
        }

        Ok(t)
    }

    fn enter_expression(&mut self, e: AstExpression) -> PResult<AstExpression> {
        match &e.data {
            AstExpressionData::FnCall { fn_name, .. } => {
                if !self.0.analyzed_functions.contains_key(fn_name) {
                    return PResult::error_at(
                        e.span,
                        format!("No such function named `{}`.", fn_name.full_name()?),
                    );
                }
            }
            AstExpressionData::GlobalVariable { name } => {
                if self.0.analyzed_globals.contains_key(name) {
                    // Okay. Do nothing.
                } else if self.0.analyzed_functions.contains_key(name) {
                    // Okay. Do nothing either.
                } else {
                    return PResult::error_at(
                        e.span,
                        format!("No such global variable named `{}`.", name.full_name()?),
                    );
                }
            }
            _ => {}
        }

        Ok(e)
    }
}
