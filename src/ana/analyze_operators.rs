use crate::{
    ana::{analyze_modules::ModuleItem, represent::AnalyzedProgram},
    ast::{
        ast_visitor::AstAdapter, AstExpression, AstExpressionData, AstLiteral, AstProgram,
        AstTraitTypeWithAssocs, AstType, BinOpKind, ModuleRef,
    },
    util::{FileRegistry, PResult, Span, Visit},
};
use std::collections::{BTreeMap, VecDeque};

pub struct AnalyzeOperators {
    analyzed_program: AnalyzedProgram,
}

impl AnalyzeOperators {
    pub fn analyze(
        analyzed_program: AnalyzedProgram,
        p: AstProgram,
    ) -> PResult<(AnalyzedProgram, AstProgram)> {
        let mut pass = AnalyzeOperators { analyzed_program };

        let p = p.visit(&mut pass)?;

        Ok((pass.analyzed_program, p))
    }

    fn construct_trt_ref(&self, trt_path: &str) -> PResult<ModuleRef> {
        let mut trt_path: VecDeque<&str> = trt_path.split("::").collect();
        let trt_name = trt_path.pop_back().unwrap();

        let mut traversed_path = vec![];
        let mut submodule = self.analyzed_program.top_module.clone();

        // All operators start with std::
        trt_path.push_front("std");
        for name in trt_path {
            let submodule_ref = submodule.clone();
            let child = (*submodule_ref)
                .borrow_mut()
                .get_child(name, &traversed_path)?;

            if let ModuleItem::Submodule(new_submodule) = child {
                submodule = new_submodule;
            } else {
                return perror!(
                    "ICE: Error realizing operator: submodule `{}` does not exist in module `{}`",
                    trt_name,
                    traversed_path.join("::"),
                );
            }

            traversed_path.push(name.to_string());
        }

        let submodule_ref = submodule.clone();
        let r = if let ModuleItem::Symbol(file_id) = (*submodule_ref)
            .borrow_mut()
            .get_child(trt_name, &traversed_path)?
        {
            ModuleRef::Normalized(file_id, trt_name.to_string())
        } else {
            return perror!(
                "ICE: Error realizing operator: trait {} does not exist in module `{}`",
                trt_name,
                traversed_path.join("::")
            );
        };

        if !self.analyzed_program.analyzed_traits.contains_key(&r) {
            return perror!(
                "ICE: Error realizing operator: `{}` is not a trait",
                r.full_name()
            );
        } else {
            Ok(r)
        }
    }

    fn construct_fn_ref(&self, fn_path: &str) -> PResult<ModuleRef> {
        let mut fn_path: VecDeque<&str> = fn_path.split("::").collect();
        let fn_name = fn_path.pop_back().unwrap();

        let mut traversed_path = vec![];
        let mut submodule = self.analyzed_program.top_module.clone();

        fn_path.push_front("std");
        for name in fn_path {
            let submodule_ref = submodule.clone();
            let child = (*submodule_ref)
                .borrow_mut()
                .get_child(name, &traversed_path)?;

            if let ModuleItem::Submodule(new_submodule) = child {
                submodule = new_submodule;
            } else {
                return perror!(
                    "ICE: Error realizing operator: submodule `{}` does not exist in module `{}`",
                    fn_name,
                    traversed_path.join("::"),
                );
            }

            traversed_path.push(name.to_string());
        }

        let submodule_ref = submodule.clone();
        let r = if let ModuleItem::Symbol(file_id) = (*submodule_ref)
            .borrow_mut()
            .get_child(fn_name, &traversed_path)?
        {
            ModuleRef::Normalized(file_id, fn_name.to_string())
        } else {
            return perror!(
                "ICE: Error realizing operator: trait {} does not exist in module `{}`",
                fn_name,
                traversed_path.join("::")
            );
        };

        if !self.analyzed_program.analyzed_functions.contains_key(&r) {
            return perror!(
                "ICE: Error realizing operator: `{}` is not a trait",
                r.full_name()
            );
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

        let associated_trait = self.construct_trt_ref(trt_name)?;
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
                    self.construct_trt_ref("Negate")?,
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
                    self.construct_trt_ref("Not")?,
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
                                self.construct_trt_ref("DerefAssign")?,
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
                    self.construct_trt_ref("Deref")?,
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
                        self.construct_trt_ref("Call")?,
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
                    self.construct_trt_ref("AllocateArray")?,
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
                    self.construct_trt_ref("Into")?,
                    vec![ty],
                    BTreeMap::new(),
                )),
                impl_signature: None,
            },
            AstExpressionData::Assert { condition } => AstExpressionData::FnCall {
                fn_name: self.construct_fn_ref("assert_impl")?,
                generics: vec![],
                args: vec![*condition, self.construct_where_at(span)?],
            },
            AstExpressionData::Unimplemented => AstExpressionData::FnCall {
                fn_name: self.construct_fn_ref("commalipses_impl")?,
                generics: vec![AstType::infer()],
                args: vec![self.construct_where_at(span)?],
            },
            e => e,
        };

        Ok(AstExpression { data, ty, span })
    }
}
