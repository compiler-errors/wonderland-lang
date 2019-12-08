use crate::ana::represent::AnalyzedProgram;
use crate::ana::represent_visitor::PureAnalysisPass;
use crate::parser::ast::{AstMatchPattern, AstMatchPatternData, AstStatement};
use crate::parser::ast_visitor::AstAdapter;
use crate::util::{IntoError, PResult};

pub struct AnalyzeInfallibleEnums(AnalyzedProgram);

impl PureAnalysisPass for AnalyzeInfallibleEnums {
    fn new(a: AnalyzedProgram) -> PResult<AnalyzeInfallibleEnums> {
        Ok(AnalyzeInfallibleEnums(a))
    }

    fn drop(self) -> AnalyzedProgram {
        self.0
    }
}

impl AnalyzeInfallibleEnums {
    fn is_infallible(&self, pattern: &AstMatchPattern) -> bool {
        match &pattern.data {
            AstMatchPatternData::Underscore | AstMatchPatternData::Identifier(..) => true,
            AstMatchPatternData::Literal(..) => false,
            AstMatchPatternData::Tuple(children) => children.iter().all(|x| self.is_infallible(x)),
            AstMatchPatternData::NamedEnum {
                enumerable,
                children,
                ..
            } => {
                self.0.analyzed_enums[enumerable].variants.len() == 1
                    && children.values().all(|x| self.is_infallible(x))
            }
            AstMatchPatternData::PositionalEnum {
                enumerable,
                children,
                ..
            } => {
                self.0.analyzed_enums[enumerable].variants.len() == 1
                    && children.iter().all(|x| self.is_infallible(x))
            }
            AstMatchPatternData::PlainEnum { enumerable, .. } => {
                self.0.analyzed_enums[enumerable].variants.len() == 1
            }
        }
    }
}

impl AstAdapter for AnalyzeInfallibleEnums {
    fn enter_statement(&mut self, s: AstStatement) -> PResult<AstStatement> {
        if let AstStatement::Let { pattern, .. } = &s {
            if !self.is_infallible(pattern) {
                return PResult::error(format!(
                    "The match pattern is not infallible, perhaps use a `match` block"
                ));
            }
        }

        Ok(s)
    }
}