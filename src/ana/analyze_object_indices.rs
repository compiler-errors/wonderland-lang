use crate::{
    ana::{represent::AnalyzedProgram, represent_visitor::PureAnalysisPass},
    ast::{visitor::AstAdapter, AstExpression, AstExpressionData},
    util::PResult,
};

pub struct AnalyzeObjectIndices(AnalyzedProgram);

impl PureAnalysisPass for AnalyzeObjectIndices {
    fn new(a: AnalyzedProgram) -> PResult<AnalyzeObjectIndices> {
        Ok(AnalyzeObjectIndices(a))
    }

    fn drop(self) -> AnalyzedProgram {
        self.0
    }
}

impl AstAdapter for AnalyzeObjectIndices {
    fn enter_ast_expression(&mut self, e: AstExpression) -> PResult<AstExpression> {
        let AstExpression { data, ty, span } = e;

        let data = match data {
            AstExpressionData::AllocateObject {
                object,
                children_idxes: None,
                generics,
                children,
            } => AstExpressionData::AllocateObject {
                children_idxes: Some(self.0.analyzed_objects[&object].member_indices.clone()),
                object,
                generics,
                children,
            },
            e => e,
        };

        Ok(AstExpression { data, ty, span })
    }
}
