use crate::ana::represent::AnalyzedProgram;
use crate::ana::represent_visitor::PureAnalysisPass;
use crate::parser::ast::{AstExpression, AstExpressionData, AstTraitType, AstType, ModuleRef};
use crate::parser::ast_visitor::AstAdapter;
use crate::util::{IntoError, PError, PResult};
use std::collections::HashMap;

pub struct AnalyzeObjectFunctions {
    analyzed_program: AnalyzedProgram,
    function_map: HashMap<String, MethodInfo>,
}

struct MethodInfo {
    trt: ModuleRef,
    generics: usize,
    args: usize,
}

impl PureAnalysisPass for AnalyzeObjectFunctions {
    fn new(analyzed_program: AnalyzedProgram) -> PResult<AnalyzeObjectFunctions> {
        let mut function_map: HashMap<String, MethodInfo> = HashMap::new();

        for (name, trt) in &analyzed_program.analyzed_traits {
            for (fun_name, fun) in &trt.methods {
                if function_map.contains_key(fun_name) {
                    return PResult::error(format!(
                        "Duplicated method `{}`: \
                         First found in `{}`, also found in `{}`.",
                        fun_name,
                        function_map[fun_name].trt.full_name()?,
                        name.full_name()?
                    ));
                }

                function_map.insert(
                    fun_name.clone(),
                    MethodInfo {
                        trt: name.clone(),
                        generics: fun.generics.len(),
                        args: fun.parameters.len(),
                    },
                );
            }
        }

        Ok(AnalyzeObjectFunctions {
            analyzed_program,
            function_map,
        })
    }

    fn drop(self) -> AnalyzedProgram {
        self.analyzed_program
    }
}

impl AnalyzeObjectFunctions {
    fn instantiate_trait(&self, name: &ModuleRef) -> AstTraitType {
        let generics = self.analyzed_program.analyzed_traits[name].generics.len();

        AstTraitType(
            name.clone(),
            (0..generics).map(|_| AstType::infer()).collect(),
        )
    }

    fn check_generics(
        &self,
        generics: Vec<AstType>,
        expected: usize,
        m: &str,
    ) -> PResult<Vec<AstType>> {
        if generics.len() == expected {
            Ok(generics)
        } else if generics.len() == 0 {
            Ok((0..expected).map(|_| AstType::infer()).collect())
        } else {
            PResult::error(format!(
                "Incorrect number of generics for symbol `{}`. Expected {}, found {}.",
                m,
                expected,
                generics.len()
            ))
        }
    }

    fn check_args(
        &self,
        args: Vec<AstExpression>,
        expected: usize,
        m: &str,
    ) -> PResult<Vec<AstExpression>> {
        if args.len() == expected {
            Ok(args)
        } else {
            PResult::error(format!(
                "Incorrect arguments of generics for function `{}`. Expected {}, found {}.",
                m,
                expected,
                args.len()
            ))
        }
    }
}

impl AstAdapter for AnalyzeObjectFunctions {
    fn enter_expression(&mut self, mut e: AstExpression) -> PResult<AstExpression> {
        let AstExpression { data, ty, span } = e;

        let data = match data {
            AstExpressionData::ObjectCall {
                object,
                fn_name,
                generics,
                mut args,
            } => {
                if self.function_map.contains_key(&fn_name) {
                    let info = &self.function_map[&fn_name];
                    let associated_trait = self.instantiate_trait(&info.trt);
                    args.insert(0, *object);

                    AstExpressionData::StaticCall {
                        call_type: AstType::infer(),
                        fn_generics: self.check_generics(generics, info.generics, &fn_name)?,
                        args: self.check_args(args, info.args, &fn_name)?,
                        fn_name,
                        associated_trait: Some(associated_trait),
                        impl_signature: None,
                    }
                } else {
                    return PResult::error(format!("No such associated typename `{}`.", fn_name));
                }
            }
            AstExpressionData::StaticCall {
                call_type,
                fn_name,
                fn_generics,
                args,
                associated_trait: None,
                impl_signature,
            } => {
                if self.function_map.contains_key(&fn_name) {
                    let info = &self.function_map[&fn_name];
                    let associated_trait = self.instantiate_trait(&info.trt);

                    AstExpressionData::StaticCall {
                        call_type,
                        fn_generics: self.check_generics(fn_generics, info.generics, &fn_name)?,
                        args: self.check_args(args, info.args, &fn_name)?,
                        fn_name,
                        associated_trait: Some(associated_trait),
                        impl_signature: None,
                    }
                } else {
                    return PResult::error(format!("No such associated typename `{}`.", fn_name));
                }
            }
            e => e,
        };

        Ok(AstExpression { data, ty, span })
    }
}
