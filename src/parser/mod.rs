lalrpop_mod!(pub cheshire, "/parser/cheshire.rs");

use crate::{
    ast::{AstExpression, AstImpl, AstModule, AstProgram},
    lexer::{Lexer, Token},
    util::{FileId, FileRegistry, PError, PResult, Span},
};
use lalrpop_util::ParseError;

pub mod display;
pub mod quote;
pub mod util;

pub fn parse_program(files: Vec<FileId>) -> PResult<AstProgram> {
    let mut modules = vec![];

    for f in files {
        let contents = FileRegistry::open(f)?;
        let name = FileRegistry::name(f);
        let lex = Lexer::new(f, &contents, false);

        modules.push(parse(f, &name, lex)?);
    }

    Ok(AstProgram { modules })
}

pub fn parse<T: CheshireParsable>(file: FileId, name: &str, lex: Lexer<'_>) -> PResult<T> {
    T::parse(file, name, lex).map_err(|e: ParseError<_, _, _>| match e {
        ParseError::InvalidToken { location } => PError::new_at(
            Span::new(file, location, location + 1),
            format!("Invalid token"),
        ),
        ParseError::UnrecognizedEOF { location, expected } => PError::new_at(
            Span::new(file, location, location + 1),
            format!(
                "Unexpected token `<EOF>`. Expected `{}`.",
                expected.join("`, `")
            ),
        ),
        ParseError::UnrecognizedToken {
            token: (l, t, h),
            expected,
        } => PError::new_at(
            Span::new(file, l, h),
            format!(
                "Unexpected token `{}`. Expected `{}`.",
                t,
                expected.join("`, `")
            ),
        ),
        ParseError::ExtraToken { token: (l, t, h) } =>
            PError::new_at(Span::new(file, l, h), format!("Extra token `{}`.", t,)),
        ParseError::User { error } => error,
    })
}

pub trait CheshireParsable: Sized {
    fn parse(
        file: FileId,
        name: &str,
        lex: Lexer<'_>,
    ) -> Result<Self, ParseError<usize, Token, PError>>;
}

impl CheshireParsable for AstModule {
    fn parse(
        file: FileId,
        name: &str,
        lex: Lexer<'_>,
    ) -> Result<AstModule, ParseError<usize, Token, PError>> {
        cheshire::ModuleParser::new().parse(file, name, lex)
    }
}

impl CheshireParsable for AstImpl {
    fn parse(
        file: FileId,
        name: &str,
        lex: Lexer<'_>,
    ) -> Result<AstImpl, ParseError<usize, Token, PError>> {
        cheshire::ImplParser::new().parse(file, name, lex)
    }
}

impl CheshireParsable for AstExpression {
    fn parse(
        file: FileId,
        name: &str,
        lex: Lexer<'_>,
    ) -> Result<AstExpression, ParseError<usize, Token, PError>> {
        cheshire::ExpressionParser::new().parse(file, name, lex)
    }
}
