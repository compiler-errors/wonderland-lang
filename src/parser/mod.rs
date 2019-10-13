pub mod ast;
pub mod ast_visitor;

use self::ast::*;
use crate::lexer::*;
use crate::util::*;

use crate::util::{FileId, FileRegistry, Span};
use std::collections::HashMap;
use std::mem;
use std::str::FromStr;

pub struct Parser {
    file: FileId,
    lexer: Lexer,
    next_span: Span,
    next_token: Token,
}

pub fn parse_program(files: Vec<FileId>) -> PResult<AstProgram> {
    let modules = files
        .into_iter()
        .map(parse_module)
        .collect::<PResult<_>>()?;

    Ok(AstProgram { modules })
}

pub fn parse_module(file: FileId) -> PResult<AstModule> {
    let mut parser = Parser {
        file,
        lexer: Lexer::new(file)?,
        next_span: Span::none(),
        next_token: Token::BOF,
    };

    parser.parse_module()
}

impl Parser {
    /// Report an error at the current position
    fn error_at<T>(&self, span: Span, error: String) -> PResult<T> {
        PResult::error_at(span, error)
    }

    /// Report an error at the current position
    fn error_here<T>(&self, s: String) -> PResult<T> {
        self.error_at(self.next_span, s)
    }

    fn id(&self) -> FileId {
        self.lexer.id()
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

    fn check_typename(&self) -> bool {
        if let Token::TypeName(_) = self.next_token {
            true
        } else {
            false
        }
    }

    fn check_identifier(&self) -> bool {
        if let Token::Identifier(_) = self.next_token {
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
        if let Token::Identifier(ident) = tok {
            self.bump()?;
            Ok(ident)
        } else {
            self.error_here(format!("Expected token `Identifier`, found `{}`.", tok))
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
    fn parse_module(mut self) -> PResult<AstModule> {
        self.expect_consume(Token::BOF).unwrap();

        let mut uses = Vec::new();
        let mut pub_uses = Vec::new();
        let mut functions = HashMap::new();
        let mut objects = HashMap::new();
        let mut traits = HashMap::new();
        let mut impls = HashMap::new();

        while self.check_consume(Token::Use)? {
            if self.check_consume(Token::Pub)? {
                pub_uses.push(self.parse_mod_path()?);
            } else {
                uses.push(self.parse_mod_path()?);
            }
        }

        while !self.check_consume(Token::EOF)? {
            if self.check(Token::Fn) {
                let fun = self.parse_function(true)?;
                let name = fun.name.clone();
                let span = fun.name_span;

                functions
                    .insert(fun.name.clone(), fun)
                    .is_not_expected(span, "function", &name)?;
            } else if self.check(Token::Object) {
                let obj = self.parse_object()?;
                let name = obj.name.clone();
                let span = obj.name_span;

                objects
                    .insert(obj.name.clone(), obj)
                    .is_not_expected(span, "object", &name)?;
            } else if self.check(Token::Export) {
                self.bump()?;
                let fun = self.parse_function(false)?;
                let name = fun.name.clone();
                let span = fun.name_span;

                functions
                    .insert(fun.name.clone(), fun)
                    .is_not_expected(span, "function", &name)?;
            } else if self.check(Token::Trait) {
                let trt = self.parse_trait()?;
                let name = trt.name.clone();
                let span = trt.name_span;

                traits
                    .insert(trt.name.clone(), trt)
                    .is_not_expected(span, "trait", &name)?;
            } else if self.check(Token::Impl) {
                let imp = self.parse_impl()?;

                impls.insert(imp.impl_id, imp);
            } else {
                // TODO: wonky
                return self.error_here(format!(
                    "Expected `export`, `fn`, `trait`, `impl` or `object`, found `{}`",
                    self.next_token
                ));
            }
        }

        Ok(AstModule::new(
            self.id(),
            FileRegistry::name(self.id())?,
            pub_uses,
            uses,
            functions,
            objects,
            traits,
            impls,
        ))
    }

    fn parse_mod_path(&mut self) -> PResult<AstUse> {
        let mut path = Vec::new();

        loop {
            if self.check(Token::Mod) {
                if !path.is_empty() {
                    return self.error_here(format!(
                        "`mod` keyword can only appear at the beginning of a module path!"
                    ));
                }

                self.bump()?;
                path.extend(FileRegistry::parent_mod_path(self.file)?);
            } else if self.check(Token::Star) {
                // Can't have a path with just `use *`
                if path.is_empty() {
                    return self.error_here(format!("Cannot have empty use-all"));
                }

                self.bump()?;
                self.expect_consume(Token::Dot)?;
                return Ok(AstUse::UseAll(path));
            } else if self.check_typename() {
                let name = self.expect_consume_typename()?;
                self.expect_consume(Token::Dot)?;
                return Ok(AstUse::Use(path, name));
            } else if self.check_identifier() {
                path.push(self.expect_consume_identifier()?);
            } else {
                return PResult::error(format!(
                    "Expected identifier, typename or `*`, got `{}`.",
                    self.next_token
                ));
            }

            if self.check_consume(Token::ColonColon)? {
                continue;
            } else if self.check_consume(Token::Dot)? {
                let name = path.pop().unwrap();
                return Ok(AstUse::Use(path, name));
            } else {
                return PResult::error(format!("Expected `.` or `::`, got `{}`.", self.next_token));
            }
        }
    }

    /// Parse a single function from the file.
    fn parse_function(&mut self, has_body: bool) -> PResult<AstFunction> {
        self.expect_consume(Token::Fn)?;
        let name_span = self.next_span;
        let fn_name = self.expect_consume_identifier()?;
        let generics = self.try_parse_decl_generics()?;
        let parameter_list = self.parse_fn_parameter_list()?;
        let return_type = self.try_parse_return_type()?;
        let restrictions = self.try_parse_restrictions()?;

        let definition = if has_body {
            let (block, _) = self.parse_block()?;

            Some(block)
        } else {
            self.expect_consume(Token::Dot)?;
            None
        };

        Ok(AstFunction::new(
            self.id(),
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
            Token::Int => {
                self.bump()?;
                (AstType::Int, self.next_span)
            }
            Token::Bool => {
                self.bump()?;
                (AstType::Bool, self.next_span)
            }
            Token::Char => {
                self.bump()?;
                (AstType::Char, self.next_span)
            }
            Token::StringType => {
                self.bump()?;
                (AstType::String, self.next_span)
            }
            Token::Infer => {
                self.bump()?;
                (AstType::infer(), self.next_span)
            }
            Token::SelfType => {
                self.bump()?;
                (AstType::SelfType, self.next_span)
            }
            Token::LSqBracket => self.parse_array_type()?,
            Token::LParen => self.parse_tuple_type()?,
            Token::TypeName(..) | Token::Identifier(..) => self.parse_object_type()?,
            Token::GenericName(..) => self.parse_generic_type()?,
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
        let mut path = Vec::new();

        loop {
            if self.check_typename() {
                span = span.unite(self.next_span);
                path.push(self.expect_consume_typename()?);
                break;
            } else if self.check_identifier() {
                span = span.unite(self.next_span);
                path.push(self.expect_consume_identifier()?);
                self.expect_consume(Token::ColonColon)?;
            } else {
                return self.error_here(format!(
                    "Expected a type name or module name, got `{}`",
                    self.next_token
                ));
            }
        }

        let generics = if self.check(Token::Lt) {
            let (generics, generics_span) = self.parse_expr_generics()?;
            span = span.unite(generics_span);
            generics
        } else {
            Vec::new()
        };

        let obj = ModuleRef::Denormalized(path);
        Ok((AstType::object(obj, generics), span))
    }

    fn parse_trait_type(&mut self) -> PResult<(AstTraitType, Span)> {
        let mut span = self.next_span;
        let mut path = Vec::new();

        loop {
            if self.check_typename() {
                path.push(self.expect_consume_typename()?);
                break;
            } else if self.check_identifier() {
                path.push(self.expect_consume_identifier()?);
                self.expect_consume(Token::ColonColon)?;
            } else {
                unimplemented!("Error! Typename or identifier.");
            }
        }

        let generics = if self.check(Token::Lt) {
            let (generics, generics_span) = self.parse_expr_generics()?;
            span = span.unite(generics_span);
            generics
        } else {
            Vec::new()
        };

        let obj = ModuleRef::Denormalized(path);
        Ok((AstTraitType(obj, generics), span))
    }

    fn parse_generic_type(&mut self) -> PResult<(AstType, Span)> {
        let span = self.next_span;
        let generic = self.expect_consume_generic()?;
        Ok((AstType::generic(generic), span))
    }

    // Parse a block of statements including LBrace and RBrace.
    fn parse_block(&mut self) -> PResult<(AstBlock, Span)> {
        let mut span = self.next_span;

        self.expect_consume(Token::LBrace)?;
        let mut statements = Vec::new();

        let expression = loop {
            while self.check_consume(Token::Dot)? {
                // Consume any dots.
            }

            span = span.unite(self.next_span);
            if self.check_consume(Token::RBrace)? {
                break AstExpression::nothing(span);
            }

            let statement = self.parse_statement()?;

            // Either parse a `.`, or an expr to end the block.
            // If there's no
            if self.check_consume(Token::Dot)? {
                statements.push(statement);
                span = span.unite(self.next_span);
            } else if self.check(Token::RBrace) {
                if let AstStatement::Expression { expression } = statement {
                    self.bump()?; // Consume the rbrace.
                    break expression;
                } else {
                    self.error_here(format!("Expected expression to end block, got statement."))?;
                }
            } else {
                self.ensure_no_dot(span, &statement)?;
                statements.push(statement);
            }
        };

        Ok((AstBlock::new(statements, expression), span))
    }

    /// Parse a statement.
    fn parse_statement(&mut self) -> PResult<AstStatement> {
        match &self.next_token {
            Token::Let => self.parse_let_statement(),
            Token::While => self.parse_while_loop(),
            Token::Break => {
                self.bump()?;
                Ok(AstStatement::break_stmt())
            }
            Token::Continue => {
                self.bump()?;
                Ok(AstStatement::continue_stmt())
            }
            Token::Return => self.parse_return_statement(),
            Token::Assert => self.parse_assert_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_let_statement(&mut self) -> PResult<AstStatement> {
        //let mut span = self.next_span;

        self.expect_consume(Token::Let)?;
        let name_span = self.next_span;
        let var_name = self.expect_consume_identifier()?;

        let (ty, _) = if self.check_consume(Token::Colon)? {
            self.parse_type()?
        } else {
            (AstType::infer(), Span::none())
        };

        self.expect_consume(Token::Equals)?;

        let value = self.parse_expression()?;

        Ok(AstStatement::let_statement(name_span, var_name, ty, value))
    }

    fn parse_while_loop(&mut self) -> PResult<AstStatement> {
        self.expect_consume(Token::While)?;
        let condition = self.parse_expression()?;
        let (block, _) = self.parse_block()?;

        Ok(AstStatement::while_loop(condition, block))
    }

    fn parse_return_statement(&mut self) -> PResult<AstStatement> {
        let span = self.next_span;
        self.expect_consume(Token::Return)?;

        if self.check(Token::Dot) {
            Ok(AstStatement::return_nothing(span))
        } else {
            let value = self.parse_expression()?;
            Ok(AstStatement::return_statement(value))
        }
    }

    fn parse_assert_statement(&mut self) -> PResult<AstStatement> {
        self.expect_consume(Token::Assert)?;
        let condition = self.parse_expression()?;
        Ok(AstStatement::assert_statement(condition))
    }

    fn parse_expression_statement(&mut self) -> PResult<AstStatement> {
        let expression = self.parse_expression()?;

        Ok(AstStatement::expression_statement(expression))
    }

    fn parse_expression(&mut self) -> PResult<AstExpression> {
        self.parse_expr(0)
    }

    fn check_operator(&self) -> bool {
        match &self.next_token {
            Token::Star
            | Token::Slash
            | Token::Modulo
            | Token::Plus
            | Token::Minus
            | Token::Lt
            | Token::Gt
            | Token::LessEqual
            | Token::GreaterEqual
            | Token::EqualsEquals
            | Token::NotEquals
            | Token::And
            | Token::Pipe
            | Token::Equals
            | Token::Colon
            | Token::LSqBracket
            | Token::LParen => true,
            _ => false,
        }
    }

    fn get_precedence(&self, token: &Token, span: Span) -> PResult<usize> {
        match token {
            Token::Star | Token::Slash | Token::Modulo => Ok(7),
            Token::Plus | Token::Minus => Ok(6),
            Token::Lt
            | Token::Gt
            | Token::LessEqual
            | Token::GreaterEqual
            | Token::EqualsEquals
            | Token::NotEquals => Ok(4),
            Token::And => Ok(2),
            Token::Pipe => Ok(1),
            Token::Equals => Ok(0),
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

            if op == Token::Equals {
                self.ensure_lval(&lhs)?;

                let rhs = self.parse_expr(new_prec)?;
                span = span.unite(rhs.span);

                lhs = AstExpression::assign(span, lhs, rhs);
            } else {
                let rhs = self.parse_expr(new_prec + 1)?;
                span = span.unite(rhs.span);

                let op_kind = get_kind(op);
                lhs = AstExpression::binop(span, lhs, rhs, op_kind);
            }
        }

        Ok(lhs)
    }

    fn parse_expr_initial(&mut self) -> PResult<AstExpression> {
        let mut span = self.next_span;

        match &self.next_token {
            Token::LBrace => {
                let (block, span) = self.parse_block()?;
                Ok(AstExpression::block(span, block))
            }
            Token::If => self.parse_if_statement(),
            Token::Commalipses => {
                self.bump()?;
                Ok(AstExpression::unimplemented(span))
            }
            Token::Not => {
                self.bump()?;
                let e = self.parse_expr(9)?;
                span = span.unite(e.span);
                Ok(AstExpression::not(span, e))
            }
            Token::Minus => {
                self.bump()?;
                let e = self.parse_expr(9)?;
                span = span.unite(e.span);
                Ok(AstExpression::neg(span, e))
            }
            Token::Allocate => self.parse_allocate_expr(),
            Token::LParen => self.parse_paren_expr(),
            Token::LSqBracket => self.parse_array_literal(),
            Token::Identifier(_) | Token::TypeName(_) => self.parse_path_expr(),
            Token::GenericName(_) | Token::Infer | Token::SelfType | Token::Lt => {
                let (ty, _) = self.parse_type()?;
                self.parse_static_call(ty)
            }
            Token::True => {
                self.bump()?;
                Ok(AstExpression::true_lit(span))
            }
            Token::False => {
                self.bump()?;
                Ok(AstExpression::false_lit(span))
            }
            Token::Null => {
                self.bump()?;
                Ok(AstExpression::null_lit(span))
            }
            Token::SelfRef => {
                self.bump()?;
                Ok(AstExpression::self_ref(span))
            }
            Token::String(string, len) => {
                let string = string.clone();
                let len = *len;
                self.bump()?;
                Ok(AstExpression::string_literal(span, string, len))
            }
            Token::IntLiteral(num) => {
                let num = num.clone();
                self.bump()?;
                Ok(AstExpression::int_literal(span, num))
            }
            Token::CharLiteral(ch) => {
                let ch = *ch;
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

    fn parse_if_statement(&mut self) -> PResult<AstExpression> {
        let mut span = self.next_span;
        self.expect_consume(Token::If)?;

        let condition = self.parse_expression()?;
        let (block, block_span) = self.parse_block()?;
        span = span.unite(block_span);

        let else_block = if self.check_consume(Token::Else)? {
            if self.check(Token::If) {
                AstBlock::new(vec![], self.parse_if_statement()?)
            } else {
                let (block, block_span) = self.parse_block()?;
                span = span.unite(block_span);
                block
            }
        } else {
            AstBlock::empty(span)
        };

        Ok(AstExpression::if_statement(
            span, condition, block, else_block,
        ))
    }

    fn parse_path_expr(&mut self) -> PResult<AstExpression> {
        let mut span = self.next_span;
        let mut path = Vec::new();

        loop {
            if self.check_typename() {
                span = span.unite(self.next_span);
                path.push(self.expect_consume_typename()?);

                let generics = if self.check(Token::Lt) {
                    let (generics, generics_span) = self.parse_expr_generics()?;
                    span = span.unite(generics_span);
                    generics
                } else {
                    Vec::new()
                };

                let ty = AstType::object(ModuleRef::Denormalized(path), generics);
                return self.parse_static_call(ty);
            } else if self.check_identifier() {
                span = span.unite(self.next_span);
                path.push(self.expect_consume_identifier()?);

                if !self.check_consume(Token::ColonColon)? {
                    return self.parse_identifier_expr(path);
                }
            } else {
                return self.error_here(format!(
                    "Expected a type name or module name, got `{}`",
                    self.next_token
                ));
            }
        }
    }

    fn parse_identifier_expr(&mut self, path: Vec<String>) -> PResult<AstExpression> {
        let mut span = self.next_span;

        if self.check(Token::LParen) {
            span = span.unite(self.next_span);
            let (args, args_span) = self.parse_expr_args()?;
            span = span.unite(args_span);
            Ok(AstExpression::call(
                span,
                ModuleRef::Denormalized(path),
                Vec::new(),
                args,
            ))
        } else if self.check(Token::ColonLt) {
            span = span.unite(self.next_span);
            self.expect_colon_push_lt()?;
            let (generics, generics_span) = self.parse_expr_generics()?;
            let (args, args_span) = self.parse_expr_args()?;
            span = span.unite(generics_span).unite(args_span);
            Ok(AstExpression::call(
                span,
                ModuleRef::Denormalized(path),
                generics,
                args,
            ))
        } else if path.len() == 1 {
            let identifier = path[0].clone();
            Ok(AstExpression::identifier(span, identifier))
        } else {
            unimplemented!("Error: Identifiers must be path-length 1. No global variables yet.");
        }
    }

    fn parse_static_call(&mut self, ty: AstType) -> PResult<AstExpression> {
        let mut span = self.next_span;
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

    fn parse_allocate_expr(&mut self) -> PResult<AstExpression> {
        let mut span = self.next_span;
        self.expect_consume(Token::Allocate)?;

        if self.check_consume(Token::LSqBracket)? {
            let (ty, ty_span) = self.parse_type()?;
            // self.expect_consume(Token::SemiColon)?;
            let expr = self.parse_expression()?;
            span = span.unite(self.next_span);
            self.expect_consume(Token::RSqBracket)?;

            // Ok(AstExpression::allocate_array(span, ty, expr))
            unimplemented!("TODO implement [T; 10]");
        } else {
            let (ty, ty_span) = self.parse_object_type()?;
            span = span.unite(ty_span);

            Ok(AstExpression::allocate_object(span, ty))
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
            AstExpressionData::Identifier { .. }
            | AstExpressionData::ArrayAccess { .. }
            | AstExpressionData::ObjectAccess { .. } => Ok(()),
            AstExpressionData::TupleAccess { accessible, .. } => self.ensure_lval(accessible),
            _ => self.error_at(expr.span, format!("Expected lval for left of `=`")),
        }
    }

    fn ensure_not_infer(&mut self, ty: &AstType, span: Span) -> PResult<()> {
        match ty {
            AstType::Array { ty } => self.ensure_not_infer(ty, span),
            AstType::Tuple { types } => {
                for ty in types {
                    self.ensure_not_infer(ty, span)?;
                }
                Ok(())
            }
            // TODO: Add spans to types...
            AstType::Infer(..) => self.error_at(span, format!("Infer `_` type not expected")),
            _ => Ok(()),
        }
    }

    fn ensure_no_dot(&mut self, span: Span, stmt: &AstStatement) -> PResult<()> {
        match stmt {
            AstStatement::Expression {
                expression:
                    AstExpression {
                        data: AstExpressionData::Block { .. },
                        ..
                    },
            }
            | AstStatement::Expression {
                expression:
                    AstExpression {
                        data: AstExpressionData::If { .. },
                        ..
                    },
            }
            | AstStatement::While { .. } => Ok(()),
            _ => PResult::error_at(span, format!("Statement must be ended with a `.`")),
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
            self.id(),
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
        let mut parameter_list = Vec::new();

        let has_self = if self.check(Token::SelfRef) {
            let self_span = self.next_span;
            self.expect_consume(Token::SelfRef)?;
            parameter_list.push(AstNamedVariable::new(
                self_span,
                "self".into(),
                AstType::SelfType,
            ));
            true
        } else {
            false
        };

        while !self.check_consume(Token::RParen)? {
            if !parameter_list.is_empty() {
                // If there are params before this, then we need a comma
                // This includes if the first param is `self`
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
            parameter_list.push(AstNamedVariable::new(param_span, param_name, param_type));
        }

        let return_type = self.try_parse_return_type()?;
        let restrictions = self.try_parse_restrictions()?;

        let definition = if has_body {
            let (block, _) = self.parse_block()?;

            Some(block)
        } else {
            self.expect_consume(Token::Dot)?;
            None
        };

        Ok(AstObjectFunction::new(
            name_span,
            name,
            generics,
            has_self,
            parameter_list,
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

        let mut fns = HashMap::new();
        let mut tys = HashMap::new();

        while !self.check_consume(Token::RBrace)? {
            if self.check(Token::Fn) {
                let fun = self.parse_object_function(false)?;
                fns.insert(fun.name.clone(), fun);
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

        Ok(AstTrait::new(
            self.id(),
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

        let mut fns = HashMap::new();
        let mut tys = HashMap::new();

        while !self.check_consume(Token::RBrace)? {
            if self.check(Token::Fn) {
                let obj_fn = self.parse_object_function(true)?;
                fns.insert(obj_fn.name.clone(), obj_fn);
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
        _ => unreachable!(),
    }
}
