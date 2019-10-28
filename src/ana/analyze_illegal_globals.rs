use crate::ana::represent_visitor::AstAnalysisPass;
use crate::parser::ast::{
    AstExpression, AstExpressionData, AstFunction, AstGlobalVariable, AstImpl, AstObject,
    AstObjectFunction, AstTrait, AstType, ModuleRef,
};
use crate::parser::ast_visitor::AstAdapter;
use crate::util::{IntoError, PResult, Visit};

pub struct AnalyzeIllegalGlobals;

impl AstAnalysisPass for AnalyzeIllegalGlobals {
    fn new() -> AnalyzeIllegalGlobals {
        AnalyzeIllegalGlobals
    }
}

impl AstAdapter for AnalyzeIllegalGlobals {
    fn enter_global_variable(&mut self, mut g: AstGlobalVariable) -> PResult<AstGlobalVariable> {
        g.init = g.init.visit(&mut DenyGlobal(g.module_ref.clone()))?;
        Ok(g)
    }
}

struct DenyGlobal(ModuleRef);

impl AstAdapter for DenyGlobal {
    fn enter_expression(&mut self, e: AstExpression) -> PResult<AstExpression> {
        match &e.data {
            AstExpressionData::GlobalVariable { name } => {
                return PResult::error(format!(
                    "Cannot reference global variable `{}` \
                     from within initialization for global variable `{}`.",
                    name.full_name()?,
                    self.0.full_name()?
                ));
            }
            _ => {}
        }

        Ok(e)
    }
}
