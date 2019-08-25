pub mod ast;
pub mod ast_visitor;

use self::ast::*;
use crate::lexer::*;
use crate::util::result::*;

use crate::util::Span;
use std::collections::HashMap;
use std::mem;
use std::str::FromStr;

pub struct Parser<'a> {
    lexer: &'a mut Lexer<'a>,
    next_span: Span,
    next_token: Token,
}

pub fn parse_file<'a>(lexer: &'a mut Lexer<'a>) -> PResult<ParsedFile> {
    let mut parser = Parser {
        lexer,
        next_span: Span::new(0, 0),
        next_token: Token::BOF,
    };

    parser.parse_file()
}

impl<'a> Parser<'a> {
    /// Report an error at the current position
    fn error_at<T>(&self, span: Span, error: String) -> PResult<T> {
        PError::new(span, error)
    }

    /// Report an error at the current position
    fn error_here<T>(&self, s: String) -> PResult<T> {
        self.error_at(self.next_span, s)
    }

    /// Move the parser forward one token
    fn bump(&mut self) -> PResult<()> {
        self.lexer.bump_token()?;
        self.next_span = self.lexer.peek_token_span();
        self.next_token = self.lexer.peek_token();

        Ok(())
    }

    /// Check for a token but don't consume it, otherwise signal an error
    fn expect(&self, token: Token) -> PResult<()> {
        let err_tok = format!("{}", token);
        if self.check(token) {
            Ok(())
        } else {
            self.error_here(format!(
                "Expected token `{}`, found `{}`.",
                err_tok, self.next_token
            ))
        }
    }

    /// Check for a token and consume it, otherwise signal an error
    fn expect_consume(&mut self, token: Token) -> PResult<()> {
        self.expect(token)?;
        self.bump()?;
        Ok(())
    }

    /// Test for a token, not consuming and return the status as a boolean
    fn check(&self, token: Token) -> bool {
        return token == self.next_token;
    }

    /// Test and consume a token (if it matches), returning the status as a boolean.
    fn check_consume(&mut self, token: Token) -> PResult<bool> {
        let x = self.check(token);
        if x {
            self.bump()?;
        }

        Ok(x)
    }

    fn check_identifier(&self) -> bool {
        if let Token::VariableName(_) = self.next_token {
            true
        } else {
            false
        }
    }

    fn check_typename(&self) -> bool {
        if let Token::TypeName(_) = self.next_token {
            true
        } else {
            false
        }
    }

    fn check_generic(&self) -> bool {
        if let Token::GenericName(_) = self.next_token {
            true
        } else {
            false
        }
    }

    fn check_number(&self) -> bool {
        if let Token::IntLiteral(_) = self.next_token {
            true
        } else {
            false
        }
    }

    fn expect_colon_push_lt(&mut self) -> PResult<()> {
        self.expect(Token::ColonLt)?;
        self.next_token = Token::Lt;
        Ok(())
    }

    /// `check_consume` but for an Identifier token.
    fn expect_consume_identifier(&mut self) -> PResult<String> {
        let tok = self.next_token.clone();
        if let Token::VariableName(ident) = tok {
            self.bump()?;
            Ok(ident)
        } else {
            self.error_here(format!("Expected token `VariableName`, found `{}`.", tok))
        }
    }

    /// `check_consume` but for an Identifier token.
    fn expect_consume_typename(&mut self) -> PResult<String> {
        let tok = self.next_token.clone();
        if let Token::TypeName(ident) = tok {
            self.bump()?;
            Ok(ident)
        } else {
            self.error_here(format!("Expected token `Typename`, found `{}`.", tok))
        }
    }

    /// `check_consume` but for an Identifier token.
    fn expect_consume_generic(&mut self) -> PResult<String> {
        let tok = self.next_token.clone();
        if let Token::GenericName(ident) = tok {
            self.bump()?;
            Ok(ident)
        } else {
            self.error_here(format!("Expected token `GenericType`, found `{}`.", tok))
        }
    }

    /// `check_consume` but for a Number token.
    fn expect_consume_number(&mut self) -> PResult<usize> {
        let tok = self.next_token.clone();
        if let Token::IntLiteral(num) = tok {
            self.bump()?;
            Ok(usize::from_str(&num).unwrap())
        } else {
            self.error_here(format!("Expected token `IntLiteral`, found `{}`.", tok))
        }
    }

    /// Parse a top level file.
    fn parse_file(&mut self) -> PResult<ParsedFile> {
        self.expect_consume(Token::BOF).unwrap();

        let mut functions = Vec::new();
        let mut objects = Vec::new();
        let mut traits = Vec::new();
        let mut impls = Vec::new();

        while !self.check_consume(Token::EOF)? {
            if self.check(Token::Fn) {
                let fun_result = self.parse_function(true)?;

                functions.push(fun_result);
            } else if self.check(Token::Object) {
                let obj_result = self.parse_object()?;

                objects.push(obj_result);
            } else if self.check(Token::Export) {
                self.expect_consume(Token::Export)?;
                let function = self.parse_function(false)?;

                functions.push(function);
            } else if self.check(Token::Trait) {
                let trait_result = self.parse_trait()?;

                traits.push(trait_result);
            } else if self.check(Token::Impl) {
                let impl_result = self.parse_impl()?;

                impls.push(impl_result);
            } else {
                // TODO: wonky
                self.error_here::<()>(format!(
                    "Expected `fn`, `export` or `object`, found `{}`",
                    self.next_token
                ))?;
            }
        }

        Ok(ParsedFile::new(functions, objects, traits, impls))
    }

    /// Parse a single function from the file.
    fn parse_function(&mut self, has_body: bool) -> PResult<AstFunction> {
        // TODO: merge with above fn
        self.expect_consume(Token::Fn)?;
        let name_span = self.next_span;
        let fn_name = self.expect_consume_identifier()?;
        let generics = self.try_parse_decl_generics()?;
        let parameter_list = self.parse_fn_parameter_list()?;
        let return_type = self.try_parse_return_type()?;
        let restrictions = self.try_parse_restrictions()?;

        let definition = if has_body {
            Some(self.parse_block()?)
        } else {
            self.expect_consume(Token::Dot)?;
            None
        };

        Ok(AstFunction::new(
            name_span,
            fn_name,
            generics,
            parameter_list,
            return_type,
            restrictions,
            definition,
        ))
    }

    fn try_parse_decl_generics(&mut self) -> PResult<Vec<AstGeneric>> {
        let mut span = self.next_span;

        if !self.check_consume(Token::Lt)? {
            return Ok(Vec::new());
        }

        let mut generics = Vec::new();

        span = span.unite(self.next_span);
        while !self.check_consume(Token::Gt)? || generics.len() == 0 {
            if generics.len() != 0 {
                self.expect_consume(Token::Comma)?;
            }

            let generic_name = self.expect_consume_generic()?;
            generics.push(AstGeneric::new(generic_name));

            span = span.unite(self.next_span);
        }

        if generics.len() == 0 {
            self.error_at(
                span,
                format!("Expected generics, got `<>` (empty generics list)"),
            )
        } else {
            Ok(generics)
        }
    }

    fn try_parse_restrictions(&mut self) -> PResult<Vec<AstTypeRestriction>> {
        if !self.check_consume(Token::Where)? {
            return Ok(Vec::new());
        }

        let mut restrictions = Vec::new();

        loop {
            let (ty, _) = self.parse_type()?;
            self.expect_consume(Token::Colon)?;

            loop {
                let (trt, _) = self.parse_trait_type()?;
                restrictions.push(AstTypeRestriction::new(ty.clone(), trt));

                if !self.check_consume(Token::Plus)? {
                    break;
                }
            }

            if !self.check_consume(Token::Comma)? {
                break;
            }
        }

        Ok(restrictions)
    }

    /// Parse a parameter list (including LParen and RParen) following a function.
    fn parse_fn_parameter_list(&mut self) -> PResult<Vec<AstNamedVariable>> {
        self.expect_consume(Token::LParen)?;
        let mut parameters = Vec::new();

        while !self.check_consume(Token::RParen)? {
            if parameters.len() != 0 {
                // If it's not the first, then we need a comma
                self.expect_consume(Token::Comma)?;
            }

            // name colon type
            let mut span = self.next_span;
            let param_name = self.expect_consume_identifier()?;
            self.expect_consume(Token::Colon)?;
            let (param_type, ty_span) = self.parse_type()?;
            span = span.unite(ty_span);
            // parameter types can't have infer(s) in them
            self.ensure_not_infer(&param_type, ty_span)?;
            parameters.push(AstNamedVariable::new(span, param_name, param_type));
        }

        Ok(parameters)
    }

    /// Parse a function return type (including RArrow) or return AstType::None
    /// If it is not present.
    fn try_parse_return_type(&mut self) -> PResult<AstType> {
        // We first check for a `->`
        if self.check_consume(Token::RArrow)? {
            let (ret, ret_span) = self.parse_type()?;
            // Also make the return type is not a `_`
            self.ensure_not_infer(&ret, ret_span)?;
            Ok(ret)
        } else {
            Ok(AstType::none())
        }
    }

    /// Try to parse an AstType.
    fn parse_type(&mut self) -> PResult<(AstType, Span)> {
        let (mut ty, mut span) = match &self.next_token {
            &Token::Int => {
                self.bump()?;
                (AstType::Int, self.next_span)
            }
            &Token::Bool => {
                self.bump()?;
                (AstType::Bool, self.next_span)
            }
            &Token::Char => {
                self.bump()?;
                (AstType::Char, self.next_span)
            }
            &Token::StringType => {
                self.bump()?;
                (AstType::String, self.next_span)
            }
            &Token::Infer => {
                self.bump()?;
                (AstType::infer(), self.next_span)
            }
            &Token::SelfType => {
                self.bump()?;
                (AstType::SelfType, self.next_span)
            }
            &Token::LSqBracket => self.parse_array_type()?,
            &Token::LParen => self.parse_tuple_type()?,
            &Token::TypeName(..) => self.parse_object_type()?,
            &Token::GenericName(..) => self.parse_generic_type()?,
            _ => self.error_here(format!(
                "Expected built-in type, `identifier` or `(`, found `{}`",
                self.next_token
            ))?,
        };

        while self.check_consume(Token::ColonColon)? {
            span = span.unite(self.next_span);
            let name = self.expect_consume_typename()?;

            ty = AstType::associated_type(ty, name);
        }

        Ok((ty, span))
    }

    fn parse_array_type(&mut self) -> PResult<(AstType, Span)> {
        let mut span = self.next_span;
        self.expect_consume(Token::LSqBracket)?;
        let (ty, _) = self.parse_type()?;
        span = span.unite(self.next_span);
        self.expect_consume(Token::RSqBracket)?;
        Ok((AstType::array(ty), span))
    }

    fn parse_tuple_type(&mut self) -> PResult<(AstType, Span)> {
        let mut span = self.next_span;
        self.expect_consume(Token::LParen)?;

        span = span.unite(self.next_span);
        if self.check_consume(Token::RParen)? {
            Ok((AstType::none(), span))
        } else {
            let mut types = Vec::new();

            while !self.check_consume(Token::RParen)? {
                if types.len() != 0 {
                    self.expect_consume(Token::Comma)?;

                    // Tuples of len 1 are like `(T,)`
                    if types.len() == 1 && self.check_consume(Token::RParen)? {
                        break;
                    }
                }

                let (subty, _) = self.parse_type()?;
                types.push(subty);

                // We don't want `(T)`, so we expect a comma if we have only one type.
                if types.len() == 1 {
                    self.expect(Token::Comma)?;
                }

                span = span.unite(self.next_span);
            }

            Ok((AstType::tuple(types), span))
        }
    }

    fn parse_object_type(&mut self) -> PResult<(AstType, Span)> {
        let mut span = self.next_span;
        let obj = self.expect_consume_typename()?;

        let generics = if self.check(Token::Lt) {
            let (generics, generics_span) = self.parse_expr_generics()?;
            span = span.unite(generics_span);
            generics
        } else {
            Vec::new()
        };

        Ok((AstType::object(obj, generics), span))
    }

    fn parse_trait_type(&mut self) -> PResult<(AstTraitType, Span)> {
        let mut span = self.next_span;
        let obj = self.expect_consume_typename()?;

        let generics = if self.check(Token::Lt) {
            let (generics, generics_span) = self.parse_expr_generics()?;
            span = span.unite(generics_span);
            generics
        } else {
            Vec::new()
        };

        Ok((AstTraitType(obj, generics), span))
    }

    fn parse_generic_type(&mut self) -> PResult<(AstType, Span)> {
        let span = self.next_span;
        let generic = self.expect_consume_generic()?;
        Ok((AstType::generic(generic), span))
    }

    // Parse a block of statements including LBrace and RBrace.
    fn parse_block(&mut self) -> PResult<AstBlock> {
        self.expect_consume(Token::LBrace)?;
        let mut statements = Vec::new();

        while !self.check_consume(Token::RBrace)? {
            while self.check_consume(Token::Dot)? {
                // Consume any dots.
            }

            statements.push(self.parse_statement()?);
        }

        Ok(AstBlock::new(statements))
    }

    /// Parse a statement.
    fn parse_statement(&mut self) -> PResult<AstStatement> {
        match &self.next_token {
            &Token::LBrace => self.parse_block_statement(),
            &Token::Let => self.parse_let_statement(),
            &Token::If => self.parse_if_statement(),
            &Token::While => self.parse_while_loop(),
            &Token::Break => {
                self.bump()?;
                self.expect_consume(Token::Dot)?;
                Ok(AstStatement::break_stmt())
            }
            &Token::Continue => {
                self.bump()?;
                self.expect_consume(Token::Dot)?;
                Ok(AstStatement::continue_stmt())
            }
            &Token::Return => self.parse_return_statement(),
            &Token::Assert => self.parse_assert_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_block_statement(&mut self) -> PResult<AstStatement> {
        let block = self.parse_block()?;
        Ok(AstStatement::block(block))
    }

    fn parse_let_statement(&mut self) -> PResult<AstStatement> {
        let mut span = self.next_span;

        self.expect_consume(Token::Let)?;
        let name_span = self.next_span;
        let var_name = self.expect_consume_identifier()?;

        let (ty, _) = if self.check_consume(Token::Colon)? {
            self.parse_type()?
        } else {
            (AstType::infer(), Span::new(0, 0))
        };

        self.expect_consume(Token::Equals)?;

        let value = self.parse_expression()?;

        span = span.unite(self.next_span);
        self.expect_consume(Token::Dot)?;

        //TODO: Use span.
        Ok(AstStatement::let_statement(name_span, var_name, ty, value))
    }

    fn parse_if_statement(&mut self) -> PResult<AstStatement> {
        self.expect_consume(Token::If)?;
        let condition = self.parse_expression()?;
        let block = self.parse_block()?;
        let else_block = if self.check_consume(Token::Else)? {
            if self.check(Token::If) {
                AstBlock::new(vec![self.parse_if_statement()?])
            } else {
                self.parse_block()?
            }
        } else {
            AstBlock::empty()
        };

        Ok(AstStatement::if_statement(condition, block, else_block))
    }

    fn parse_while_loop(&mut self) -> PResult<AstStatement> {
        self.expect_consume(Token::While)?;
        let condition = self.parse_expression()?;
        let block = self.parse_block()?;

        Ok(AstStatement::while_loop(condition, block))
    }

    fn parse_return_statement(&mut self) -> PResult<AstStatement> {
        self.expect_consume(Token::Return)?;

        if self.check(Token::Dot) {
            self.expect_consume(Token::Dot)?;
            Ok(AstStatement::return_nothing())
        } else {
            let value = self.parse_expression()?;
            self.expect_consume(Token::Dot)?;
            Ok(AstStatement::return_statement(value))
        }
    }

    fn parse_assert_statement(&mut self) -> PResult<AstStatement> {
        self.expect_consume(Token::Assert)?;
        let condition = self.parse_expression()?;
        self.expect_consume(Token::Dot)?;
        Ok(AstStatement::assert_statement(condition))
    }

    fn parse_expression_statement(&mut self) -> PResult<AstStatement> {
        let expr = self.parse_expression()?;

        match &expr.data {
            &AstExpressionData::BinOp {
                kind: BinOpKind::Set,
                ..
            }
            | &AstExpressionData::Call { .. }
            | &AstExpressionData::ObjectCall { .. }
            | &AstExpressionData::StaticCall { .. }
            | &AstExpressionData::Unimplemented => {}
            _ => {
                return self.error_at(expr.span, format!("Expected expression statement."));
            }
        }

        self.expect_consume(Token::Dot)?;
        Ok(AstStatement::expression_statement(expr))
    }

    fn parse_expression(&mut self) -> PResult<AstExpression> {
        self.parse_expr(0)
    }

    fn check_operator(&self) -> bool {
        match &self.next_token {
            &Token::Star
            | &Token::Slash
            | &Token::Modulo
            | &Token::Plus
            | &Token::Minus
            | &Token::Lt
            | &Token::Gt
            | &Token::LessEqual
            | &Token::GreaterEqual
            | &Token::EqualsEquals
            | &Token::NotEquals
            | &Token::And
            | &Token::Pipe
            | &Token::Equals
            | &Token::Colon
            | &Token::LSqBracket
            | &Token::LParen => true,
            _ => false,
        }
    }

    fn get_precedence(&self, token: &Token, span: Span) -> PResult<usize> {
        match token {
            &Token::Star | &Token::Slash | &Token::Modulo => Ok(7),
            &Token::Plus | &Token::Minus => Ok(6),
            &Token::Lt
            | &Token::Gt
            | &Token::LessEqual
            | &Token::GreaterEqual
            | &Token::EqualsEquals
            | &Token::NotEquals => Ok(4),
            &Token::And => Ok(2),
            &Token::Pipe => Ok(1),
            &Token::Equals => Ok(0),
            _ => self.error_at(span, format!("Uknown token `{}`", token)),
        }
    }

    fn parse_expr(&mut self, prec: usize) -> PResult<AstExpression> {
        let mut lhs = self.parse_expr_initial()?;
        let mut span = lhs.span;

        while self.check_operator() {
            let op_span = self.next_span;
            let op = self.next_token.clone();

            match op {
                Token::LSqBracket => {
                    self.bump()?;
                    let idx = self.parse_expr(0)?;

                    span = span.unite(self.next_span);
                    self.expect_consume(Token::RSqBracket)?;

                    lhs = AstExpression::access(span, lhs, idx);
                    continue;
                }
                Token::Colon => {
                    self.bump()?;
                    if self.check_identifier() {
                        span = span.unite(self.next_span);

                        let name = self.expect_consume_identifier()?;
                        if self.check(Token::ColonLt) {
                            self.expect_colon_push_lt()?;
                            let (generics, generics_span) = self.parse_expr_generics()?;
                            let (args, args_span) = self.parse_expr_args()?;
                            span = span.unite(generics_span).unite(args_span);
                            lhs = AstExpression::object_call(span, lhs, name, generics, args);
                        } else if self.check(Token::LParen) {
                            let (args, args_span) = self.parse_expr_args()?;
                            span = span.unite(args_span);
                            lhs = AstExpression::object_call(span, lhs, name, Vec::new(), args);
                        } else {
                            lhs = AstExpression::object_access(span, lhs, name);
                        }

                        span = span.unite(lhs.span);
                        continue;
                    } else if self.check_number() {
                        span = span.unite(span);
                        let idx = self.expect_consume_number()?;

                        lhs = AstExpression::tuple_access(span, lhs, idx);
                        continue;
                    } else {
                        self.error_here(format!(
                            "Expected either number or identifier after `:`, \
                             found {}",
                            self.next_token
                        ))?;
                    }
                }
                _ => {}
            }

            let new_prec = self.get_precedence(&op, op_span)?;
            if new_prec < prec {
                break;
            }

            span = span.unite(span);
            self.bump()?; // Okay, consume the op

            let rhs = if op == Token::Equals {
                self.ensure_lval(&lhs)?;
                self.parse_expr(new_prec)
            } else {
                self.parse_expr(new_prec + 1)
            }?;

            span = span.unite(rhs.span);

            let op_kind = get_kind(op);
            lhs = AstExpression::binop(span, lhs, rhs, op_kind);
        }

        Ok(lhs)
    }

    fn parse_expr_initial(&mut self) -> PResult<AstExpression> {
        let mut span = self.next_span;

        match &self.next_token {
            &Token::Commalipses => {
                self.bump()?;
                Ok(AstExpression::unimplemented(span))
            }
            &Token::Not => {
                self.bump()?;
                let e = self.parse_expr(9)?;
                span = span.unite(e.span);
                Ok(AstExpression::not(span, e))
            }
            &Token::Minus => {
                self.bump()?;
                let e = self.parse_expr(9)?;
                span = span.unite(e.span);
                Ok(AstExpression::neg(span, e))
            }
            &Token::Allocate => {
                self.bump()?;
                let (ty, ty_span) = self.parse_object_type()?;
                span = span.unite(ty_span);
                Ok(AstExpression::allocate(span, ty))
            }
            &Token::LParen => self.parse_paren_expr(),
            &Token::LSqBracket => self.parse_array_literal(),
            &Token::VariableName(_) => self.parse_identifier_expr(),
            &Token::TypeName(_)
            | &Token::GenericName(_)
            | &Token::Infer
            | Token::SelfType
            | Token::Lt => self.parse_static_call(),
            &Token::True => {
                self.bump()?;
                Ok(AstExpression::true_lit(span))
            }
            &Token::False => {
                self.bump()?;
                Ok(AstExpression::false_lit(span))
            }
            &Token::Null => {
                self.bump()?;
                Ok(AstExpression::null_lit(span))
            }
            &Token::SelfRef => {
                self.bump()?;
                Ok(AstExpression::self_ref(span))
            }
            _ => {
                // This is wonky, but we don't want to ALWAYS clone...
                match self.next_token.clone() {
                    Token::String(ref string, len) => {
                        self.bump()?;
                        Ok(AstExpression::string_literal(span, string.clone(), len))
                    }
                    Token::IntLiteral(ref num) => {
                        self.bump()?;
                        Ok(AstExpression::int_literal(span, num.clone()))
                    }
                    Token::CharLiteral(ch) => {
                        self.bump()?;
                        Ok(AstExpression::char_literal(span, ch))
                    }
                    _ => self.error_here(format!(
                        "Expected literal, identifier, `new` or `(`, found \
                         `{}`",
                        self.next_token
                    )),
                }
            }
        }
    }

    fn parse_identifier_expr(&mut self) -> PResult<AstExpression> {
        let mut span = self.next_span;
        let identifier = self.expect_consume_identifier()?;

        if self.check(Token::LParen) {
            span = span.unite(self.next_span);
            let (args, args_span) = self.parse_expr_args()?;
            span = span.unite(args_span);
            Ok(AstExpression::call(span, identifier, Vec::new(), args))
        } else if self.check(Token::ColonLt) {
            span = span.unite(self.next_span);
            self.expect_colon_push_lt()?;
            let (generics, generics_span) = self.parse_expr_generics()?;
            let (args, args_span) = self.parse_expr_args()?;
            span = span.unite(generics_span).unite(args_span);
            Ok(AstExpression::call(span, identifier, generics, args))
        } else {
            Ok(AstExpression::identifier(span, identifier))
        }
    }

    fn parse_static_call(&mut self) -> PResult<AstExpression> {
        let mut span = self.next_span;
        let bracketed = self.check_consume(Token::Lt)?;
        let (ty, _) = self.parse_type()?;

        if bracketed {
            self.expect_consume(Token::Gt)?;
        }

        self.expect_consume(Token::Colon)?;
        let identifier = self.expect_consume_identifier()?;

        span = span.unite(self.next_span);
        if self.check(Token::LParen) {
            let (args, args_span) = self.parse_expr_args()?;
            span = span.unite(args_span);

            Ok(AstExpression::static_call(
                span,
                ty,
                identifier,
                Vec::new(),
                args,
            ))
        } else if self.check(Token::ColonLt) {
            self.expect_colon_push_lt()?;
            let (fn_generics, generics_span) = self.parse_expr_generics()?;
            let (args, args_span) = self.parse_expr_args()?;
            span = span.unite(generics_span).unite(args_span);

            Ok(AstExpression::static_call(
                span,
                ty,
                identifier,
                fn_generics,
                args,
            ))
        } else {
            self.error_here(format!("Expected `(` or `:<`, found `{}`", self.next_token))
        }
    }

    fn parse_array_literal(&mut self) -> PResult<AstExpression> {
        let mut span = self.next_span;
        self.expect_consume(Token::LSqBracket)?;

        span = span.unite(self.next_span);
        if self.check_consume(Token::RSqBracket)? {
            return Ok(AstExpression::empty_array_literal(span));
        }

        let first = self.parse_expr(0)?;

        span = span.unite(self.next_span);
        if self.check(Token::Comma) {
            let mut elements = vec![first];

            span = span.unite(self.next_span);
            while !self.check_consume(Token::RSqBracket)? {
                self.expect_consume(Token::Comma)?;
                elements.push(self.parse_expr(0)?);

                span = span.unite(self.next_span);
            }

            Ok(AstExpression::array_literal(span, elements))
        } else if self.check_consume(Token::RSqBracket)? {
            let elements = vec![first];
            Ok(AstExpression::array_literal(span, elements))
        } else {
            self.error_here(format!("Expected `]` or `,`, found `{}`", self.next_token))
        }
    }

    fn parse_paren_expr(&mut self) -> PResult<AstExpression> {
        let mut span = self.next_span;
        self.expect_consume(Token::LParen)?;

        if self.check_consume(Token::RParen)? {
            return Ok(AstExpression::nothing(span));
        }

        let first = self.parse_expr(0)?;

        span = span.unite(self.next_span);
        if self.check_consume(Token::RParen)? {
            Ok(first)
        } else if self.check_consume(Token::Comma)? {
            let mut elements = vec![first];
            let mut first = true;

            span = span.unite(self.next_span);
            while !self.check_consume(Token::RParen)? {
                if !first {
                    self.expect_consume(Token::Comma)?;
                }

                elements.push(self.parse_expr(0)?);
                first = false;

                span = span.unite(self.next_span);
            }

            Ok(AstExpression::tuple_literal(span, elements))
        } else {
            // Expected ) , or ..
            self.error_here(format!("Expected `)` or `,`, found `{}`", self.next_token))
        }
    }

    fn parse_expr_args(&mut self) -> PResult<(Vec<AstExpression>, Span)> {
        let mut span = self.next_span;

        self.expect_consume(Token::LParen)?;
        let mut args = Vec::new();

        span = span.unite(self.next_span);
        while !self.check_consume(Token::RParen)? {
            if args.len() != 0 {
                self.expect_consume(Token::Comma)?;
            }

            args.push(self.parse_expression()?);

            span = span.unite(self.next_span);
        }

        Ok((args, span))
    }

    fn parse_expr_generics(&mut self) -> PResult<(Vec<AstType>, Span)> {
        let mut span = self.next_span;
        self.expect_consume(Token::Lt)?;

        let mut generics = Vec::new();

        span = span.unite(self.next_span);
        while !self.check_consume(Token::Gt)? || generics.len() == 0 {
            if generics.len() != 0 {
                self.expect_consume(Token::Comma)?;
            }

            let (ty, _) = self.parse_type()?;
            generics.push(ty);
            span = span.unite(self.next_span);
        }

        if generics.len() == 0 {
            self.error_at(span, format!("Expected generics, got `<>`"))
        } else {
            Ok((generics, span))
        }
    }

    fn ensure_lval(&self, expr: &AstExpression) -> PResult<()> {
        match &expr.data {
            &AstExpressionData::Identifier { .. } | &AstExpressionData::ArrayAccess { .. } => Ok(()),
            &AstExpressionData::TupleAccess { ref accessible, .. } => self.ensure_lval(accessible),
            &AstExpressionData::ObjectAccess { ref object, .. } => {
                if mem::discriminant(&object.data) == mem::discriminant(&AstExpressionData::SelfRef)
                {
                    // Because self itself cannot be an lval, but it can be inside an lval...
                    Ok(())
                } else {
                    self.ensure_lval(object)
                }
            }
            _ => self.error_at(expr.span, format!("Expected lval for left of `=`")),
        }
    }

    fn ensure_not_infer(&mut self, ty: &AstType, span: Span) -> PResult<()> {
        match ty {
            &AstType::Array { ref ty } => self.ensure_not_infer(ty, span),
            &AstType::Tuple { ref types } => {
                for ty in types {
                    self.ensure_not_infer(ty, span)?;
                }
                Ok(())
            }
            // TODO: Add spans to types...
            &AstType::Infer(..) => self.error_at(span, format!("Infer `_` type not expected")),
            _ => Ok(()),
        }
    }

    fn parse_object(&mut self) -> PResult<AstObject> {
        self.expect_consume(Token::Object)?;
        let name_span = self.next_span;
        let name = self.expect_consume_typename()?;
        let generics = self.try_parse_decl_generics()?;
        let restrictions = self.try_parse_restrictions()?;
        self.expect_consume(Token::LBrace)?;
        let mut members = Vec::new();

        while !self.check_consume(Token::RBrace)? {
            let mut span = self.next_span;
            let mem_name = self.expect_consume_identifier()?;
            self.expect_consume(Token::Colon)?;
            let (ty, ty_span) = self.parse_type()?;
            span = span.unite(ty_span);
            members.push(AstObjectMember::new(span, mem_name, ty));
            self.expect_consume(Token::Dot)?;
        }

        Ok(AstObject::new(
            name_span,
            generics,
            name,
            members,
            restrictions,
        ))
    }

    fn parse_object_function(&mut self, has_body: bool) -> PResult<AstObjectFunction> {
        self.expect_consume(Token::Fn)?;
        let name_span = self.next_span;
        let name = self.expect_consume_identifier()?;
        let generics = self.try_parse_decl_generics()?;
        self.expect_consume(Token::LParen)?;
        let mut parameters = Vec::new();

        let has_self = if self.check(Token::SelfRef) {
            let self_span = self.next_span;
            self.expect_consume(Token::SelfRef)?;
            parameters.push(AstNamedVariable::new(
                self_span,
                "self".to_string(),
                AstType::SelfType,
            ));
            true
        } else {
            false
        };

        while !self.check_consume(Token::RParen)? {
            if parameters.len() != 0 || has_self {
                // If it's not the first, then we need a comma
                self.expect_consume(Token::Comma)?;
            }

            // name colon type
            let mut param_span = self.next_span;
            let param_name = self.expect_consume_identifier()?;
            self.expect_consume(Token::Colon)?;
            let (param_type, ty_span) = self.parse_type()?;
            param_span = param_span.unite(ty_span);
            // parameter types can't have infer(s) in them
            self.ensure_not_infer(&param_type, ty_span)?;
            parameters.push(AstNamedVariable::new(param_span, param_name, param_type));
        }

        let return_type = self.try_parse_return_type()?;
        let restrictions = self.try_parse_restrictions()?;

        let definition = if has_body {
            Some(self.parse_block()?)
        } else {
            self.expect_consume(Token::Dot)?;
            None
        };

        Ok(AstObjectFunction::new(
            name_span,
            name,
            generics,
            has_self,
            parameters,
            return_type,
            restrictions,
            definition,
        ))
    }

    fn parse_trait(&mut self) -> PResult<AstTrait> {
        self.expect_consume(Token::Trait)?;
        let name_span = self.next_span;
        let name = self.expect_consume_typename()?;
        let generics = self.try_parse_decl_generics()?;

        let mut restrictions = Vec::new();

        if self.check_consume(Token::Colon)? {
            loop {
                let (trt, _) = self.parse_trait_type()?;
                restrictions.push(AstTypeRestriction::new(AstType::SelfType, trt));

                if !self.check_consume(Token::Plus)? {
                    break;
                }
            }
        }

        restrictions.extend(self.try_parse_restrictions()?);
        self.expect_consume(Token::LBrace)?;

        let mut fns = Vec::new();
        let mut tys = HashMap::new();

        while !self.check_consume(Token::RBrace)? {
            if self.check(Token::Fn) {
                let fun = self.parse_object_function(false)?;
                fns.push(fun);
            } else if self.check(Token::Type) {
                let (name, ty) = self.parse_trait_associated_ty()?;
                tys.insert(name, ty);
            } else {
                self.error_here(format!(
                    "Expected `fn` or `type`, found `{}`",
                    self.next_token
                ))?;
            }
        }

        // PARSE_TODO
        Ok(AstTrait::new(
            name_span,
            name,
            generics,
            fns,
            restrictions,
            tys,
        ))
    }

    fn parse_trait_associated_ty(&mut self) -> PResult<(String, AstAssociatedType)> {
        self.expect_consume(Token::Type)?;
        let name = self.expect_consume_typename()?;
        let mut restrictions = Vec::new();

        if self.check_consume(Token::Colon)? {
            loop {
                let (restriction, _) = self.parse_trait_type()?;
                restrictions.push(restriction);

                if self.check(Token::Dot) {
                    break;
                }
            }
        }

        self.expect_consume(Token::Dot)?;

        Ok((name.clone(), AstAssociatedType { name, restrictions }))
    }

    fn parse_impl(&mut self) -> PResult<AstImpl> {
        let name_span = self.next_span;
        self.expect_consume(Token::Impl)?;
        let impl_generics = self.try_parse_decl_generics()?;
        let (trait_ty, _) = self.parse_trait_type()?;
        self.expect_consume(Token::For)?;
        let (impl_ty, _) = self.parse_type()?;
        let restrictions = self.try_parse_restrictions()?;
        self.expect_consume(Token::LBrace)?;

        let mut fns = Vec::new();
        let mut tys = HashMap::new();

        while !self.check_consume(Token::RBrace)? {
            if self.check(Token::Fn) {
                let obj_fn = self.parse_object_function(true)?;
                fns.push(obj_fn);
            } else if self.check(Token::Type) {
                let (name, ty) = self.parse_impl_associated_ty()?;
                tys.insert(name, ty);
            } else {
                self.error_here(format!(
                    "Expected `fn` or `type`, found `{}`",
                    self.next_token
                ))?;
            }
        }

        Ok(AstImpl::new(
            name_span,
            impl_generics,
            trait_ty,
            impl_ty,
            fns,
            restrictions,
            tys,
        ))
    }

    fn parse_impl_associated_ty(&mut self) -> PResult<(String, AstType)> {
        self.expect_consume(Token::Type)?;
        let name = self.expect_consume_typename()?;
        self.expect_consume(Token::Equals)?;
        let (ty, _) = self.parse_type()?;
        self.expect_consume(Token::Dot)?;

        Ok((name, ty))
    }
}

fn get_kind(t: Token) -> BinOpKind {
    match t {
        Token::Star => BinOpKind::Multiply,
        Token::Slash => BinOpKind::Divide,
        Token::Modulo => BinOpKind::Modulo,
        Token::Plus => BinOpKind::Add,
        Token::Minus => BinOpKind::Subtract,
        Token::Lt => BinOpKind::Less,
        Token::Gt => BinOpKind::Greater,
        Token::GreaterEqual => BinOpKind::GreaterEqual,
        Token::LessEqual => BinOpKind::LessEqual,
        Token::NotEquals => BinOpKind::NotEqual,
        Token::EqualsEquals => BinOpKind::EqualsEquals,
        Token::And => BinOpKind::And,
        Token::Pipe => BinOpKind::Or,
        Token::Equals => BinOpKind::Set,
        _ => unreachable!(),
    }
}
