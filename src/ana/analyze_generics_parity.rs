use crate::ana::represent::AnalyzedProgram;
use crate::ana::represent_visitor::{AnAdapter, DirtyAnalysisPass};
use crate::parser::ast::*;
use crate::parser::ast_visitor::AstAdapter;
use crate::util::IntoError;
use crate::util::PResult;
use std::collections::HashMap;


pub struct AnalyzeGenericsParity {
    parity: HashMap<ModuleRef, usize>,
}

impl AnalyzeGenericsParity {
    fn check_generics(&self, m: &ModuleRef, generics: Vec<AstType>) -> PResult<Vec<AstType>> {
        let expected = self.parity[m];

        if generics.len() == expected {
            Ok(generics)
        } else if generics.len() == 0 {
            Ok((0..expected).map(|_| AstType::infer()).collect())
        } else {
            PResult::error(format!(
                "Incorrect number of generics for symbol `{}`. Expected {}, found {}.",
                m.full_name()?,
                expected,
                generics.len()
            ))
        }
    }
}

impl DirtyAnalysisPass for AnalyzeGenericsParity {
    fn new(a: &AnalyzedProgram) -> PResult<AnalyzeGenericsParity> {
        Ok(AnalyzeGenericsParity {
            parity: ((a.analyzed_functions)
                .iter()
                .map(|(r, i)| (r.clone(), i.generics.len())))
            .chain(
                a.analyzed_objects
                    .iter()
                    .map(|(r, i)| (r.clone(), i.generics.len())),
            )
            .chain(
                a.analyzed_traits
                    .iter()
                    .map(|(r, i)| (r.clone(), i.generics.len())),
            )
            .collect(),
        })
    }
}

impl AnAdapter for AnalyzeGenericsParity {}

impl AstAdapter for AnalyzeGenericsParity {
    fn enter_type(&mut self, t: AstType) -> PResult<AstType> {
        match t {
            AstType::Object(object, generics) => {
                let generics = self.check_generics(&object, generics)?;
                Ok(AstType::Object(object, generics))
            }
            t => Ok(t),
        }
    }

    fn enter_trait_type(&mut self, mut t: AstTraitType) -> PResult<AstTraitType> {
        t.1 = self.check_generics(&t.0, t.1)?;
        Ok(t)
    }

    fn enter_expression(&mut self, e: AstExpression) -> PResult<AstExpression> {
        let AstExpression { data, ty, span } = e;

        let data = match data {
            AstExpressionData::Call {
                fn_name,
                generics,
                args,
            } => {
                let generics = self.check_generics(&fn_name, generics)?;
                AstExpressionData::Call {
                    fn_name,
                    args,
                    generics,
                }
            }
            d => d,
        };

        Ok(AstExpression { data, ty, span })
    }
}
