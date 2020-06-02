use crate::{
    ana::represent::AnalyzedProgram,
    ast::{
        visitor::AstAdapter, AstExpression, AstExpressionData, AstLiteral, AstTraitTypeWithAssocs,
        AstType, BinOpKind, ModuleRef,
    },
    cheshire_quote,
    util::{FileRegistry, PResult, Span, Visit},
};
use std::collections::BTreeMap;

pub struct AnalyzeOperators {
    analyzed_program: AnalyzedProgram,
}

impl AnalyzeOperators {
    pub fn analyze<T: Visit<Self>>(
        analyzed_program: AnalyzedProgram,
        t: T,
    ) -> PResult<(AnalyzedProgram, T)> {
        let mut pass = AnalyzeOperators { analyzed_program };

        let t = t.visit(&mut pass)?;

        Ok((pass.analyzed_program, t))
    }

    fn verify_fn(&self, trt: &ModuleRef, fn_name: &str) -> PResult<()> {
        if !self.analyzed_program.analyzed_traits[trt]
            .methods
            .contains_key(fn_name)
        {
            unreachable!(
                "ICE: Error realizing operator: trait {} does not contain function `{}`",
                trt.full_name(),
                fn_name
            )
        } else {
            Ok(())
        }
    }

    fn construct_where_at(&self, span: Span) -> PResult<AstExpression> {
        let (name, row, col, _, _, line) = FileRegistry::location_info(span)?;
        let arrow = format!("{}^", "-".repeat(col - 1));
        let where_at_string = format!("{}:{} in module `{}`\n{}\n{}", row, col, name, line, arrow);

        Ok(AstExpression::literal(
            span,
            AstLiteral::String(where_at_string),
        ))
    }
}

impl AnalyzeOperators {
    fn get_binop_call(
        &self,
        kind: BinOpKind,
        lhs: Box<AstExpression>,
        rhs: Box<AstExpression>,
    ) -> PResult<AstExpressionData> {
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
            BinOpKind::Range => ("Range", "range"),
            BinOpKind::AndShort | BinOpKind::OrShort => unreachable!("ICE: Unexpected operator `{:?}` when lifting operator to trait", kind),
        };

        let associated_trait = self.analyzed_program.construct_trt_ref(trt_name)?;
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
            AstExpressionData::BinOp {
                kind: BinOpKind::AndShort,
                lhs,
                rhs,
            } => {
                let e: AstExpression = cheshire_quote!(
                    &mut self.analyzed_program,
                    "if {lhs} {{ {rhs} }} else {{ false }}",
                    lhs = lhs,
                    rhs = rhs,
                );
                e.data
            },
            AstExpressionData::BinOp {
                kind: BinOpKind::OrShort,
                lhs,
                rhs,
            } => {
                let e: AstExpression = cheshire_quote!(
                    &mut self.analyzed_program,
                    "if {lhs} {{ true }} else {{ {rhs} }}",
                    lhs = lhs,
                    rhs = rhs,
                );
                e.data
            },
            AstExpressionData::BinOp { kind, lhs, rhs } => self.get_binop_call(kind, lhs, rhs)?,

            AstExpressionData::Negate(expr) => AstExpressionData::StaticCall {
                call_type: AstType::infer(),
                fn_name: "negate".into(),
                fn_generics: vec![],
                args: vec![*expr],
                associated_trait: Some(AstTraitTypeWithAssocs::new(
                    self.analyzed_program.construct_trt_ref("Negate")?,
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
                    self.analyzed_program.construct_trt_ref("Not")?,
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
                                self.analyzed_program.construct_trt_ref("DerefAssign")?,
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
                    self.analyzed_program.construct_trt_ref("Deref")?,
                    vec![],
                    BTreeMap::new(),
                )),
                impl_signature: None,
            },
            AstExpressionData::ExprCall { expr, args } => {
                let arg_tys = args.iter().map(|a| a.ty.clone()).collect();
                let args = AstExpression::tuple_literal(span, args);

                AstExpressionData::StaticCall {
                    call_type: AstType::infer(),
                    fn_name: "call".into(),
                    fn_generics: vec![],
                    args: vec![*expr, args],
                    associated_trait: Some(AstTraitTypeWithAssocs::new(
                        self.analyzed_program.construct_trt_ref("Call")?,
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
                    self.analyzed_program.construct_trt_ref("AllocateArray")?,
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
                    self.analyzed_program.construct_trt_ref("Into")?,
                    vec![ty],
                    BTreeMap::new(),
                )),
                impl_signature: None,
            },
            AstExpressionData::Assert { condition } => AstExpressionData::FnCall {
                fn_name: self.analyzed_program.construct_fn_ref("assert_impl")?,
                generics: vec![],
                args: vec![*condition, self.construct_where_at(span)?],
            },
            AstExpressionData::Unimplemented => AstExpressionData::FnCall {
                fn_name: self.analyzed_program.construct_fn_ref("commalipses_impl")?,
                generics: vec![AstType::infer()],
                args: vec![self.construct_where_at(span)?],
            },
            e => e,
        };

        Ok(AstExpression { data, ty, span })
    }
}
