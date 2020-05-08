use crate::{
    ana::{represent::AnalyzedProgram, represent_visitor::PureAnalysisPass},
    ast::{
        ast_visitor::AstAdapter, AstExpression, AstExpressionData, AstMatchPattern,
        AstMatchPatternData, AstStatement,
    },
    util::PResult,
};

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
            } =>
                self.0.analyzed_enums[enumerable].variants.len() == 1
                    && children.values().all(|x| self.is_infallible(x)),
            AstMatchPatternData::PositionalEnum {
                enumerable,
                children,
                ..
            } =>
                self.0.analyzed_enums[enumerable].variants.len() == 1
                    && children.iter().all(|x| self.is_infallible(x)),
            AstMatchPatternData::PlainEnum { enumerable, .. } =>
                self.0.analyzed_enums[enumerable].variants.len() == 1,
        }
    }
}

impl AstAdapter for AnalyzeInfallibleEnums {
    fn enter_ast_statement(&mut self, s: AstStatement) -> PResult<AstStatement> {
        if let AstStatement::Let { pattern, .. } = &s {
            if !self.is_infallible(pattern) {
                // FIXME: This should be a spanned error, once the statement is turned into an
                // expr.
                return perror!("The match pattern is not infallible, perhaps use a `match` block");
            }
        }

        Ok(s)
    }

    fn enter_ast_expression(&mut self, e: AstExpression) -> PResult<AstExpression> {
        if let AstExpressionData::Closure { params, .. } = &e.data {
            for param in params {
                if !self.is_infallible(param) {
                    // FIXME: This should be a spanned error, once the statement is turned into an
                    // expr.
                    return perror!(
                        "The match pattern is not infallible. Closures must receive infallible \
                         patterns as arguments!"
                    );
                }
            }
        }

        Ok(e)
    }
}
