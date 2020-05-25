use crate::ast::*;
use std::fmt::{Display, Formatter, Result as FResult};

pub trait CheshireFormattable {
    fn cheshire_fmt(&self, f: &mut Formatter<'_>) -> FResult;
}

impl<T> CheshireFormattable for Box<T>
where
    T: CheshireFormattable,
{
    fn cheshire_fmt(&self, f: &mut Formatter<'_>) -> FResult {
        (**self).cheshire_fmt(f)
    }
}

/// --------------------------------------------------------------------- ///

pub struct CheshireDisplay<'a, T: 'a>(pub &'a T);

impl<'a, T: CheshireFormattable + 'a> Display for CheshireDisplay<'a, T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        self.0.cheshire_fmt(f)
    }
}

pub struct CheshireDisplayMany<'a, T: 'a>(pub &'a [T]);

impl<'a, T: CheshireFormattable + 'a> Display for CheshireDisplayMany<'a, T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        for t in self.0 {
            t.cheshire_fmt(f)?;
            write!(f, ", ")?;
        }

        Ok(())
    }
}

pub struct CheshireDisplayGenerics<'a, T: 'a>(pub &'a [T], &'static str);

impl<'a, T: CheshireFormattable + 'a> Display for CheshireDisplayGenerics<'a, T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        if !self.0.is_empty() {
            write!(f, "{} ", self.1)?;
            for t in self.0 {
                t.cheshire_fmt(f)?;
                write!(f, ", ")?;
            }
            write!(f, " > ")?;
        }

        Ok(())
    }
}

/// --------------------------------------------------------------------- ///

impl CheshireFormattable for ModuleRef {
    fn cheshire_fmt(&self, f: &mut Formatter<'_>) -> FResult {
        match self {
            ModuleRef::Denormalized(parts) =>
                for (idx, part) in parts.iter().enumerate() {
                    if idx != 0 {
                        write!(f, "::")?;
                    }
                    write!(f, "{}", part)?;
                },
            ModuleRef::Normalized(id, name) => write!(f, "$[ModuleRef]({}, {})$", id.0, name)?,
        }

        Ok(())
    }
}

impl CheshireFormattable for AstBlock {
    fn cheshire_fmt(&self, f: &mut Formatter<'_>) -> FResult {
        match self {
            AstBlock {
                statements,
                expression,
                scope: None,
            } => {
                write!(f, "{{ ")?;

                for s in statements {
                    write!(f, "{} ", CheshireDisplay(s))?;
                }

                write!(f, "{} }} ", CheshireDisplay(expression))?;
            },
            AstBlock {
                statements,
                expression,
                scope: Some(scope),
            } => {
                write!(f, "$[Block]( {{ ")?;

                for s in statements {
                    write!(f, "{} ", CheshireDisplay(s))?;
                }

                write!(f, "{} }}, (", CheshireDisplay(expression))?;

                for (idx, (_, AstNamedVariable { span, name, ty, id })) in scope.iter().enumerate()
                {
                    if idx != 0 {
                        write!(f, ", ")?;
                    }

                    write!(
                        f,
                        "$[NamedVariable]({}, {}, {}, {}, {}, {})$",
                        span.file.0,
                        span.start,
                        span.end,
                        name,
                        CheshireDisplay(ty),
                        id.0
                    )?;
                }

                write!(f, ") )$")?;
            },
        }
        Ok(())
    }
}

impl CheshireFormattable for AstStatement {
    fn cheshire_fmt(&self, f: &mut Formatter<'_>) -> FResult {
        match self {
            AstStatement::Let { pattern, value } => write!(
                f,
                "let {} = {} . ",
                CheshireDisplay(pattern),
                CheshireDisplay(value)
            ),
            AstStatement::Expression { expression } =>
                write!(f, "{} . ", CheshireDisplay(expression)),
        }
    }
}

impl CheshireFormattable for AstExpression {
    fn cheshire_fmt(&self, f: &mut Formatter<'_>) -> FResult {
        write!(
            f,
            "$[SpannedExpr]({}, {}, {}, {}, ",
            self.span.file.0,
            self.span.start,
            self.span.end,
            CheshireDisplay(&self.ty),
        )?;
        match &self.data {
            AstExpressionData::Unimplemented => write!(f, ",,,")?,
            AstExpressionData::SelfRef => write!(f, "self")?,
            AstExpressionData::Literal(literal) => write!(f, "{}", CheshireDisplay(literal))?,
            AstExpressionData::Identifier {
                name,
                variable_id: Some(variable_id),
            } => write!(f, "$[Identifier]({}, {})$", name, variable_id.0)?,
            AstExpressionData::Identifier {
                name,
                variable_id: None,
            } => write!(f, "{}", name)?,
            AstExpressionData::Tuple { values } => write!(f, "({})", CheshireDisplayMany(&values))?,
            AstExpressionData::ArrayLiteral { elements } =>
                write!(f, "[{}]", CheshireDisplayMany(&elements))?,
            AstExpressionData::Closure {
                params,
                return_ty,
                expr,
                captured: None,
                scope: None,
            } => write!(
                f,
                "|{}| -> {} {{ {} }}",
                CheshireDisplayMany(&params),
                CheshireDisplay(return_ty),
                CheshireDisplay(expr)
            )?,
            AstExpressionData::FnCall {
                fn_name,
                generics,
                args,
            } => write!(
                f,
                "{}{}({})",
                CheshireDisplay(fn_name),
                CheshireDisplayGenerics(generics, ":<"),
                CheshireDisplayMany(&args)
            )?,
            AstExpressionData::ExprCall { expr, args } => write!(
                f,
                "{}({})",
                CheshireDisplay(expr),
                CheshireDisplayMany(&args)
            )?,
            AstExpressionData::ObjectCall {
                object,
                fn_name,
                generics,
                args,
            } => write!(
                f,
                "{}:{}{}({})",
                CheshireDisplay(object),
                fn_name,
                CheshireDisplayGenerics(generics, ":<"),
                CheshireDisplayMany(&args)
            )?,
            AstExpressionData::StaticCall {
                call_type,
                fn_name,
                fn_generics,
                args,
                associated_trait: None,
                impl_signature: None,
            } => write!(
                f,
                "<{}>:{}{}({})",
                CheshireDisplay(call_type),
                fn_name,
                CheshireDisplayGenerics(fn_generics, ":<"),
                CheshireDisplayMany(&args)
            )?,
            AstExpressionData::StaticCall {
                call_type,
                fn_name,
                fn_generics,
                args,
                associated_trait: Some(associated_trait),
                impl_signature: None,
            } => write!(
                f,
                "<{} as {}>:{}{}({})",
                CheshireDisplay(call_type),
                CheshireDisplay(associated_trait),
                fn_name,
                CheshireDisplayGenerics(fn_generics, ":<"),
                CheshireDisplayMany(&args)
            )?,
            AstExpressionData::StaticCall {
                call_type,
                fn_name,
                fn_generics,
                args,
                associated_trait: Some(associated_trait),
                impl_signature: Some(AstImplSignature { impl_id, generics }),
            } => write!(
                f,
                "$[StaticCall]({}, {}, {}, ({}), ({}), {}, ({}))$",
                CheshireDisplay(call_type),
                CheshireDisplay(associated_trait),
                fn_name,
                CheshireDisplayMany(&fn_generics),
                CheshireDisplayMany(&args),
                impl_id.0,
                CheshireDisplayMany(&generics),
            )?,
            AstExpressionData::ArrayAccess { accessible, idx } => write!(
                f,
                "{}[{}]",
                CheshireDisplay(accessible),
                CheshireDisplay(idx)
            )?,
            AstExpressionData::TupleAccess { accessible, idx } =>
                write!(f, "{}:{}", CheshireDisplay(accessible), *idx)?,
            AstExpressionData::ObjectAccess {
                object,
                mem_name,
                mem_idx: None,
            } => write!(f, "{}:{}", CheshireDisplay(object), mem_name)?,
            AstExpressionData::AllocateObject {
                object,
                generics,
                children,
                children_idxes: None,
            } => {
                write!(
                    f,
                    "allocate {}{} {{ ",
                    CheshireDisplay(object),
                    CheshireDisplayGenerics(generics, "<")
                )?;
                for (child, expr) in children {
                    write!(f, "{}: {}, ", child, CheshireDisplay(expr))?;
                }
                write!(f, " }}")?;
            },
            AstExpressionData::AllocateArray { object, size } => write!(
                f,
                "allocate [{}; {}]",
                CheshireDisplay(object),
                CheshireDisplay(size)
            )?,
            AstExpressionData::Not(expr) => write!(f, "!{}", CheshireDisplay(expr))?,
            AstExpressionData::Negate(expr) => write!(f, "-{}", CheshireDisplay(expr))?,
            AstExpressionData::Assign { lhs, rhs } =>
                write!(f, "{} = {}", CheshireDisplay(lhs), CheshireDisplay(rhs))?,
            AstExpressionData::BinOp { kind, lhs, rhs } => write!(
                f,
                "{} {} {}",
                CheshireDisplay(lhs),
                CheshireDisplay(kind),
                CheshireDisplay(rhs)
            )?,
            AstExpressionData::Block { block } => write!(f, "{}", CheshireDisplay(block))?,
            AstExpressionData::If {
                condition,
                block,
                else_block,
            } => write!(
                f,
                "if {} {} else {}",
                CheshireDisplay(condition),
                CheshireDisplay(block),
                CheshireDisplay(else_block)
            )?,
            AstExpressionData::Match {
                expression,
                branches,
            } => write!(
                f,
                "match {} {{ {} }}",
                CheshireDisplay(expression),
                CheshireDisplayMany(&branches)
            )?,
            AstExpressionData::While {
                label,
                id,
                condition,
                block,
                else_block,
            } => write!(
                f,
                "$[While]({}, {}, {}, {}, {})$",
                label.as_deref().unwrap_or("_"),
                id.0,
                CheshireDisplay(condition),
                CheshireDisplay(block),
                CheshireDisplay(else_block)
            )?,
            AstExpressionData::For {
                label,
                pattern,
                iterable,
                block,
                else_block,
            } => write!(
                f,
                "$[For]({}, {}, {}, {}, {})$",
                label.as_deref().unwrap_or("_"),
                CheshireDisplay(pattern),
                CheshireDisplay(iterable),
                CheshireDisplay(block),
                CheshireDisplay(else_block)
            )?,
            AstExpressionData::PlainEnum {
                enumerable,
                generics,
                variant,
            } => write!(
                f,
                "{}{}!{}",
                CheshireDisplay(enumerable),
                CheshireDisplayGenerics(&generics, "<"),
                variant
            )?,
            AstExpressionData::PositionalEnum {
                enumerable,
                generics,
                variant,
                children,
            } => write!(
                f,
                "{}{}!{}({})",
                CheshireDisplay(enumerable),
                CheshireDisplayGenerics(&generics, "<"),
                variant,
                CheshireDisplayMany(&children)
            )?,
            AstExpressionData::NamedEnum {
                enumerable,
                generics,
                variant,
                children,
            } => {
                write!(
                    f,
                    "{}{}!{} {{ ",
                    CheshireDisplay(enumerable),
                    CheshireDisplayGenerics(&generics, "<"),
                    variant
                )?;

                for (child, expr) in children {
                    write!(f, "{}: {}, ", child, CheshireDisplay(expr))?;
                }

                write!(f, "}}")?;
            },
            AstExpressionData::As { expression, ty } => write!(
                f,
                "{} as {}",
                CheshireDisplay(expression),
                CheshireDisplay(ty)
            )?,
            AstExpressionData::Instruction {
                instruction,
                arguments,
                output,
            } => write!(
                f,
                "instruction \"{}\" ({}) -> {}",
                instruction,
                CheshireDisplayMany(&arguments),
                CheshireDisplay(output)
            )?,
            AstExpressionData::Break { label, id, value } => write!(
                f,
                "$[Break]({}, {}, {})$",
                label.as_deref().unwrap_or("_"),
                id.map(|i| format!("{}", i.0))
                    .unwrap_or_else(|| "_".to_string()),
                CheshireDisplay(value)
            )?,
            AstExpressionData::Continue { label, id } => write!(
                f,
                "$[Continue]({}, {})$",
                label.as_deref().unwrap_or("_"),
                id.map(|i| format!("{}", i.0))
                    .unwrap_or_else(|| "_".to_string())
            )?,
            AstExpressionData::Return { value } => write!(f, "return {}", CheshireDisplay(value))?,
            AstExpressionData::Assert { condition } =>
                write!(f, "assert {}", CheshireDisplay(condition))?,
            AstExpressionData::ConditionalCompilation { branches } => {
                for (idx, (name, block)) in branches.iter().enumerate() {
                    if idx != 0 {
                        write!(f, " else ")?;
                    }
                    write!(f, "impl \"{}\" {}", name, CheshireDisplay(block))?;
                }
            },

            _ => todo!("Unsupported quasiquote: {:#?}", self),
        }
        write!(f, " )$")?;
        Ok(())
    }
}

impl CheshireFormattable for AstLabel {
    fn cheshire_fmt(&self, f: &mut Formatter<'_>) -> FResult {
        write!(f, ":{}", self.0)?;
        Ok(())
    }
}

impl CheshireFormattable for BinOpKind {
    fn cheshire_fmt(&self, f: &mut Formatter<'_>) -> FResult {
        match self {
            BinOpKind::Multiply => write!(f, "*")?,
            BinOpKind::Divide => write!(f, "/")?,
            BinOpKind::Modulo => write!(f, "%")?,
            BinOpKind::Add => write!(f, "+")?,
            BinOpKind::Subtract => write!(f, "-")?,
            BinOpKind::Greater => write!(f, ">")?,
            BinOpKind::Less => write!(f, "<")?,
            BinOpKind::GreaterEqual => write!(f, ">=")?,
            BinOpKind::LessEqual => write!(f, "<=")?,
            BinOpKind::EqualsEquals => write!(f, "==")?,
            BinOpKind::NotEqual => write!(f, "!=")?,
            BinOpKind::And => write!(f, "&")?,
            BinOpKind::Or => write!(f, "|")?,
        }

        Ok(())
    }
}

impl CheshireFormattable for AstLiteral {
    fn cheshire_fmt(&self, f: &mut Formatter<'_>) -> FResult {
        match self {
            AstLiteral::True => write!(f, "true")?,
            AstLiteral::False => write!(f, "false")?,
            AstLiteral::String(s) => {
                let mut string = String::new();

                for c in s.chars() {
                    match c {
                        '\r' => string.push_str("\\r"),
                        '\n' => string.push_str("\\n"),
                        '\t' => string.push_str("\\t"),
                        '\"' => string.push_str("\\\""),
                        '\'' => string.push_str("\\\'"),
                        '\\' => string.push_str("\\\\"),
                        c => string.push(c),
                    }
                }

                write!(f, "\"{}\"", string)?
            },
            AstLiteral::Int(i) => write!(f, "{}", i)?,
            AstLiteral::Float(i) => write!(f, "{}", i)?,
            AstLiteral::Char(c) => match c {
                '\r' => write!(f, "'\\r'")?,
                '\n' => write!(f, "'\\n'")?,
                '\t' => write!(f, "'\\t'")?,
                '\"' => write!(f, "'\\\"'")?,
                '\'' => write!(f, "'\\\''")?,
                '\\' => write!(f, "'\\\\'")?,
                c => write!(f, "'{}'", *c)?,
            },
        }

        Ok(())
    }
}

impl CheshireFormattable for InstructionArgument {
    fn cheshire_fmt(&self, f: &mut Formatter<'_>) -> FResult {
        match self {
            InstructionArgument::Expression(e) => write!(f, "{}", CheshireDisplay(e))?,
            InstructionArgument::Type(t) => write!(f, "_: {}", CheshireDisplay(t))?,
            InstructionArgument::Anonymous(a) => write!(f, "${}", a)?,
        }

        Ok(())
    }
}

impl CheshireFormattable for InstructionOutput {
    fn cheshire_fmt(&self, f: &mut Formatter<'_>) -> FResult {
        match self {
            InstructionOutput::Type(t) => write!(f, "{}", CheshireDisplay(t))?,
            InstructionOutput::Anonymous(a) => write!(f, "${}", a)?,
        }

        Ok(())
    }
}

impl CheshireFormattable for AstMatchPattern {
    fn cheshire_fmt(&self, f: &mut Formatter<'_>) -> FResult {
        write!(
            f,
            "$[SpannedPattern]({}, {}, {}, {}, ",
            self.span.file.0,
            self.span.start,
            self.span.end,
            CheshireDisplay(&self.ty),
        )?;

        match &self.data {
            AstMatchPatternData::Underscore => write!(f, "_")?,
            AstMatchPatternData::Identifier(AstNamedVariable { span, name, ty, id }) => write!(
                f,
                "$[NamedVariable]({}, {}, {}, {}, {}, {})$",
                span.file.0,
                span.start,
                span.end,
                name,
                CheshireDisplay(ty),
                id.0
            )?,
            AstMatchPatternData::Tuple(t) => write!(f, "({})", CheshireDisplayMany(&t))?,
            AstMatchPatternData::Literal(l) => write!(f, "{}", CheshireDisplay(l))?,
            AstMatchPatternData::PlainEnum {
                enumerable,
                generics,
                variant,
            } => write!(
                f,
                "{}{}!{}",
                CheshireDisplay(enumerable),
                CheshireDisplayGenerics(&generics, "<"),
                variant
            )?,
            AstMatchPatternData::PositionalEnum {
                enumerable,
                generics,
                variant,
                children,
                ignore_rest,
            } => write!(
                f,
                "{}{}!{}({}{})",
                CheshireDisplay(enumerable),
                CheshireDisplayGenerics(&generics, "<"),
                variant,
                CheshireDisplayMany(&children),
                if *ignore_rest { ".." } else { "" }
            )?,
            AstMatchPatternData::NamedEnum {
                enumerable,
                generics,
                variant,
                children,
                ignore_rest,
            } => {
                write!(
                    f,
                    "{}{}!{} {{",
                    CheshireDisplay(enumerable),
                    CheshireDisplayGenerics(&generics, "<"),
                    variant,
                )?;

                for (child, pattern) in children {
                    write!(f, "{}: {}, ", child, CheshireDisplay(pattern))?;
                }

                write!(f, "{} }}", if *ignore_rest { ".." } else { "" })?;
            },
        }

        write!(f, ")$")?;
        Ok(())
    }
}

impl CheshireFormattable for AstMatchBranch {
    fn cheshire_fmt(&self, f: &mut Formatter<'_>) -> FResult {
        match self {
            AstMatchBranch {
                pattern,
                expression,
                scope: None,
            } => {
                write!(
                    f,
                    "{} => {}",
                    CheshireDisplay(pattern),
                    CheshireDisplay(expression)
                )?;
            },
            AstMatchBranch {
                pattern,
                expression,
                scope: Some(scope),
            } => {
                write!(
                    f,
                    "$[MatchBranch]( {} => {} , (",
                    CheshireDisplay(pattern),
                    CheshireDisplay(expression)
                )?;

                for (idx, (_, AstNamedVariable { span, name, ty, id })) in scope.iter().enumerate()
                {
                    if idx != 0 {
                        write!(f, ", ")?;
                    }

                    write!(
                        f,
                        "$[NamedVariable]({}, {}, {}, {}, {}, {})$",
                        span.file.0,
                        span.start,
                        span.end,
                        name,
                        CheshireDisplay(ty),
                        id.0
                    )?;
                }

                write!(f, ") )$")?;
            },
        }

        Ok(())
    }
}

impl CheshireFormattable for AstType {
    fn cheshire_fmt(&self, f: &mut Formatter<'_>) -> FResult {
        write!(f, "< ")?;
        match self {
            AstType::Infer(i) => write!(f, "$[Infer]({})$", i.0)?,
            AstType::Int => write!(f, "Int")?,
            AstType::Float => write!(f, "Float")?,
            AstType::Char => write!(f, "Char")?,
            AstType::Bool => write!(f, "Bool")?,
            AstType::String => write!(f, "String")?,
            AstType::SelfType => write!(f, "Self")?,
            AstType::Generic(generic) => write!(f, "_{}", generic)?,
            AstType::Array { ty } => write!(f, "[{}]", CheshireDisplay(ty))?,
            AstType::Tuple { types } => write!(f, "({})", CheshireDisplayMany(&types))?,
            AstType::ObjectEnum(name, generics) => write!(
                f,
                "{}{}",
                CheshireDisplay(name),
                CheshireDisplayGenerics(&generics, "<")
            )?,
            AstType::Object(name, generics) => write!(
                f,
                "{}{}",
                CheshireDisplay(name),
                CheshireDisplayGenerics(&generics, "<")
            )?,
            AstType::Enum(name, generics) => write!(
                f,
                "{}{}",
                CheshireDisplay(name),
                CheshireDisplayGenerics(&generics, "<")
            )?,
            AstType::ClosureType { args, ret_ty } => write!(
                f,
                "|{}| -> {}",
                CheshireDisplayMany(&args),
                CheshireDisplay(ret_ty)
            )?,
            AstType::ClosureEnvType => write!(f, "ClosureEnvType")?,
            AstType::FnPointerType { args, ret_ty } => write!(
                f,
                "fn({}) -> {}",
                CheshireDisplayMany(&args),
                CheshireDisplay(ret_ty)
            )?,
            AstType::AssociatedType {
                obj_ty,
                trait_ty: Some(trait_ty),
                name,
            } => write!(
                f,
                "<{} as {}>::{}",
                CheshireDisplay(obj_ty),
                CheshireDisplay(trait_ty),
                name
            )?,
            AstType::AssociatedType {
                obj_ty,
                trait_ty: None,
                name,
            } => write!(f, "{}::{}", CheshireDisplay(obj_ty), name)?,
            AstType::ElaboratedType { obj_ty, trait_ty } => write!(
                f,
                "<{} as {}>",
                CheshireDisplay(obj_ty),
                CheshireDisplay(trait_ty)
            )?,
            AstType::DynamicType { trait_tys } =>
                if trait_tys.is_empty() {
                    write!(f, "<Dyn>")?;
                } else {
                    write!(f, "Dyn<")?;
                    for (idx, trait_ty) in trait_tys.iter().enumerate() {
                        if idx != 0 {
                            write!(f, " + ")?;
                        }
                        write!(f, "{}", CheshireDisplay(trait_ty))?;
                    }
                    write!(f, ">")?;
                },
            AstType::GenericPlaceholder(id, name) =>
                write!(f, "$[GenericPlaceholder]({}, {})$", id.0, name)?,
            AstType::DummyGeneric(id, name) => write!(f, "$[DummyGeneric]({}, {})$", id.0, name)?,
            AstType::Dummy(id) => write!(f, "$[Dummy]({})$", id.0)?,
        }

        write!(f, " >")?;
        Ok(())
    }
}

impl CheshireFormattable for AstTraitType {
    fn cheshire_fmt(&self, f: &mut Formatter<'_>) -> FResult {
        let AstTraitType { name, generics } = self;

        write!(
            f,
            "{}{}",
            CheshireDisplay(name),
            CheshireDisplayGenerics(generics, "<")
        )?;

        Ok(())
    }
}

impl CheshireFormattable for AstTraitTypeWithAssocs {
    fn cheshire_fmt(&self, f: &mut Formatter<'_>) -> FResult {
        let AstTraitTypeWithAssocs {
            trt: AstTraitType { name, generics },
            assoc_bindings,
        } = self;

        write!(f, "{}", CheshireDisplay(name))?;

        if !generics.is_empty() || !assoc_bindings.is_empty() {
            write!(f, "<")?;

            for (idx, generic) in generics.iter().enumerate() {
                if idx != 0 {
                    write!(f, ", ")?;
                }

                write!(f, "{}", CheshireDisplay(generic))?;
            }

            for (idx, (name, binding)) in assoc_bindings.iter().enumerate() {
                if idx != 0 || !generics.is_empty() {
                    write!(f, ", ")?;
                }

                write!(f, "::{} = {}", name, CheshireDisplay(binding))?;
            }

            write!(f, ">")?;
        }

        Ok(())
    }
}
