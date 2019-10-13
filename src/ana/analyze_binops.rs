use crate::ana::represent::AnalyzedProgram;
use crate::ana::represent_visitor::AstAnalysisPass;
use crate::parser::ast::{
    AstExpression, AstExpressionData, AstProgram, AstTraitType, AstType, BinOpKind, ModuleRef,
};
use crate::parser::ast_visitor::AstAdapter;
use crate::util::{FileId, FileRegistry, IntoError, PError, PResult, Visit};

pub struct AnalyzeBinops {
    analyzed_program: AnalyzedProgram,
    operators_file: FileId,
}

impl AnalyzeBinops {
    pub fn analyze(
        analyzed_program: AnalyzedProgram,
        mut p: AstProgram,
    ) -> PResult<(AnalyzedProgram, AstProgram)> {
        let mut operators_file = None;

        for m in &p.modules {
            if FileRegistry::mod_path(m.id)? == vec!["std", "operators"] {
                operators_file = Some(m.id);
                break;
            }
        }

        if operators_file.is_none() {
            return PResult::error(format!(
                "Cannot find `std::operations` file. This should never happen!"
            ));
        }

        let mut pass = AnalyzeBinops {
            analyzed_program,
            operators_file: operators_file.unwrap(),
        };

        let p = p.visit(&mut pass)?;

        Ok((pass.analyzed_program, p))
    }

    fn construct_ref(&self, trt_name: &str) -> PResult<ModuleRef> {
        let r = ModuleRef::Normalized(self.operators_file, trt_name.into());

        if !self.analyzed_program.analyzed_traits.contains_key(&r) {
            PResult::error(format!(
                "Error realizing operator: trait {} does not exist in module `{}`",
                trt_name,
                FileRegistry::mod_path(self.operators_file)?.join("::")
            ))
        } else {
            Ok(r)
        }
    }

    fn verify_fn(&self, trt: &ModuleRef, fn_name: &str) -> PResult<()> {
        if !self.analyzed_program.analyzed_traits[trt]
            .methods
            .contains_key(fn_name)
        {
            PResult::error(format!(
                "Error realizing operator: trait {} does not contain function `{}`",
                trt.full_name()?,
                fn_name
            ))
        } else {
            Ok(())
        }
    }
}

impl AnalyzeBinops {
    fn get_call(
        &self,
        kind: BinOpKind,
        lhs: Box<AstExpression>,
        rhs: Box<AstExpression>,
    ) -> PResult<AstExpressionData> {
        // TODO: It's gross to have these and not be able to verify they
        // actually exist unless they're used in the code itself.
        let (trt_name, fn_name) = match kind {
            BinOpKind::Multiply => ("Multiply", "mul"),
            BinOpKind::Divide => ("Divide", "div"),
            BinOpKind::Modulo => ("Modulo", "mod"),
            BinOpKind::Add => ("Add", "add"),
            BinOpKind::Subtract => ("Subtract", "sub"),
            BinOpKind::Greater => ("Compare", "gt"),
            BinOpKind::Less => ("Compare", "lt"),
            BinOpKind::GreaterEqual => ("Compare", "ge"),
            BinOpKind::LessEqual => ("Compare", "le"),
            BinOpKind::EqualsEquals => ("Equals", "eq"),
            BinOpKind::NotEqual => ("Equals", "ne"),
            BinOpKind::And => ("And", "and"),
            BinOpKind::Or => ("Or", "or"),
        };

        let associated_trait = self.construct_ref(trt_name)?;
        self.verify_fn(&associated_trait, fn_name)?;

        Ok(AstExpressionData::StaticCall {
            call_type: AstType::infer(),
            fn_name: fn_name.into(),
            fn_generics: vec![],
            args: vec![*lhs, *rhs],
            associated_trait: Some(AstTraitType(associated_trait, vec![AstType::infer()])),
            impl_signature: None,
        })
    }
}

impl AstAdapter for AnalyzeBinops {
    fn enter_expression(&mut self, e: AstExpression) -> PResult<AstExpression> {
        let AstExpression { data, ty, span } = e;

        let data = match data {
            AstExpressionData::BinOp { kind, lhs, rhs } => self.get_call(kind, lhs, rhs)?,
            e => e,
        };

        Ok(AstExpression { data, ty, span })
    }
}
