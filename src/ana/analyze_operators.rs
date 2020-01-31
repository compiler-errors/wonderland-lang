use crate::{
    ana::represent::AnalyzedProgram,
    parser::{
        ast::{
            AstExpression, AstExpressionData, AstProgram, AstTraitTypeWithAssocs, AstType,
            BinOpKind, ModuleRef,
        },
        ast_visitor::AstAdapter,
    },
    util::{FileId, FileRegistry, PResult, Visit},
};
use std::collections::BTreeMap;

pub struct AnalyzeOperators {
    analyzed_program: AnalyzedProgram,
    operators_file: FileId,
}

impl AnalyzeOperators {
    pub fn analyze(
        analyzed_program: AnalyzedProgram,
        p: AstProgram,
    ) -> PResult<(AnalyzedProgram, AstProgram)> {
        let mut operators_file = None;

        for m in &p.modules {
            if FileRegistry::mod_path(m.id) == vec!["std", "operators"] {
                operators_file = Some(m.id);
                break;
            }
        }

        if operators_file.is_none() {
            return perror!("Cannot find `std::operations` file. This should never happen!");
        }

        let mut pass = AnalyzeOperators {
            analyzed_program,
            operators_file: operators_file.unwrap(),
        };

        let p = p.visit(&mut pass)?;

        Ok((pass.analyzed_program, p))
    }

    fn construct_ref(&self, trt_name: &str) -> PResult<ModuleRef> {
        let r = ModuleRef::Normalized(self.operators_file, trt_name.into());

        if !self.analyzed_program.analyzed_traits.contains_key(&r) {
            perror!(
                "ICE: Error realizing operator: trait {} does not exist in module `{}`",
                trt_name,
                FileRegistry::mod_path(self.operators_file).join("::")
            )
        } else {
            Ok(r)
        }
    }

    fn verify_fn(&self, trt: &ModuleRef, fn_name: &str) -> PResult<()> {
        if !self.analyzed_program.analyzed_traits[trt]
            .methods
            .contains_key(fn_name)
        {
            perror!(
                "ICE: Error realizing operator: trait {} does not contain function `{}`",
                trt.full_name(),
                fn_name
            )
        } else {
            Ok(())
        }
    }
}

impl AnalyzeOperators {
    fn get_binop_call(
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
            BinOpKind::Modulo => ("Modulo", "rem"),
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
            associated_trait: Some(AstTraitTypeWithAssocs::new(
                associated_trait,
                vec![AstType::infer()],
                BTreeMap::new(),
            )),
            impl_signature: None,
        })
    }
}

impl AstAdapter for AnalyzeOperators {
    fn enter_ast_expression(&mut self, e: AstExpression) -> PResult<AstExpression> {
        let AstExpression { data, ty, span } = e;

        let data = match data {
            AstExpressionData::Negate(expr) => AstExpressionData::StaticCall {
                call_type: AstType::infer(),
                fn_name: "negate".into(),
                fn_generics: vec![],
                args: vec![*expr],
                associated_trait: Some(AstTraitTypeWithAssocs::new(
                    self.construct_ref("Negate")?,
                    vec![],
                    BTreeMap::new(),
                )),
                impl_signature: None,
            },
            AstExpressionData::Not(expr) => AstExpressionData::StaticCall {
                call_type: AstType::infer(),
                fn_name: "not".into(),
                fn_generics: vec![],
                args: vec![*expr],
                associated_trait: Some(AstTraitTypeWithAssocs::new(
                    self.construct_ref("Not")?,
                    vec![],
                    BTreeMap::new(),
                )),
                impl_signature: None,
            },
            AstExpressionData::Assign { lhs, rhs } => {
                let AstExpression {
                    data: lhs_data,
                    ty,
                    span,
                } = *lhs;

                match lhs_data {
                    AstExpressionData::ArrayAccess { accessible, idx } =>
                        AstExpressionData::StaticCall {
                            call_type: AstType::infer(),
                            fn_name: "deref_assign".into(),
                            fn_generics: vec![],
                            args: vec![*accessible, *idx, *rhs],
                            associated_trait: Some(AstTraitTypeWithAssocs::new(
                                self.construct_ref("DerefAssign")?,
                                vec![],
                                BTreeMap::new(),
                            )),
                            impl_signature: None,
                        },
                    lhs_data => AstExpressionData::Assign {
                        lhs: Box::new(AstExpression {
                            data: lhs_data,
                            ty,
                            span,
                        }),
                        rhs,
                    },
                }
            },
            AstExpressionData::ArrayAccess { accessible, idx } => AstExpressionData::StaticCall {
                call_type: AstType::infer(),
                fn_name: "deref".into(),
                fn_generics: vec![],
                args: vec![*accessible, *idx],
                associated_trait: Some(AstTraitTypeWithAssocs::new(
                    self.construct_ref("Deref")?,
                    vec![],
                    BTreeMap::new(),
                )),
                impl_signature: None,
            },
            AstExpressionData::BinOp { kind, lhs, rhs } => self.get_binop_call(kind, lhs, rhs)?,
            AstExpressionData::ExprCall { expr, args } => {
                let arg_tys = args.iter().map(|a| a.ty.clone()).collect();
                let args = AstExpression::tuple_literal(span, args);

                AstExpressionData::StaticCall {
                    call_type: AstType::infer(),
                    fn_name: "call".into(),
                    fn_generics: vec![],
                    args: vec![*expr, args],
                    associated_trait: Some(AstTraitTypeWithAssocs::new(
                        self.construct_ref("Call")?,
                        vec![AstType::tuple(arg_tys)],
                        BTreeMap::new(),
                    )),
                    impl_signature: None,
                }
            },
            AstExpressionData::AllocateArray { object, size } => AstExpressionData::StaticCall {
                call_type: object.clone(),
                fn_name: "allocate_array".into(),
                fn_generics: vec![],
                args: vec![*size],
                associated_trait: Some(AstTraitTypeWithAssocs::new(
                    self.construct_ref("AllocateArray")?,
                    vec![],
                    BTreeMap::new(),
                )),
                impl_signature: None,
            },
            AstExpressionData::As { expression, ty } => AstExpressionData::StaticCall {
                call_type: AstType::infer(),
                fn_name: "into".into(),
                fn_generics: vec![],
                args: vec![*expression],
                associated_trait: Some(AstTraitTypeWithAssocs::new(
                    self.construct_ref("Into")?,
                    vec![ty],
                    BTreeMap::new(),
                )),
                impl_signature: None,
            },
            e => e,
        };

        Ok(AstExpression { data, ty, span })
    }
}
