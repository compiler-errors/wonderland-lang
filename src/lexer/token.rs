use std::fmt::{Display, Formatter, Result};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Token {
    // Language symbols
    Dot,            //    .
    Comma,          //    ,
    Colon,          //    :
    ColonColon,     //    ::
    ColonLt,        //    :<
    LBrace,         //    {
    RBrace,         //    }
    LSqBracket,     //    [
    RSqBracket,     //    ]
    Lt,             //    <
    Gt,             //    >
    LParen,         //    (
    RParen,         //    )
    RArrow,         //    ->

    // Mathematical operators (excluding LSquare, RSquare)
    LessEqual,      //    <=
    GreaterEqual,   //    >=
    EqualsEquals,   //    ==
    NotEquals,      //    !=
    And,            //    &
    Pipe,           //    |
    Equals,         //    =
    Not,            //    !
    Plus,           //    +
    Minus,          //    -
    ShiftLeft,      //    <<
    ShiftRight,     //    >>
    Star,           //    *
    Slash,          //    /
    Modulo,         //    %
    Caret,          //    ^

    // Special keywords -- all are lowercase
    Fn,
    Export,
    Let,
    Trait,
    Impl,
    Where,
    For,

    If,
    Else,
    While,
    Break,
    Continue,
    Return,
    Assert,
    True,
    False,
    Null,
    Object,
    Has,
    SelfRef,
    Allocate,

    // Privileged Types
    Int,
    UInt,
    Bool,
    StringType,
    Float,
    Char,
    Infer,
    SelfType,

    // Literals
    String(String, u32),
    IntLiteral(String),
    UIntLiteral(String),
    FloatLiteral(String),
    CharLiteral(char),
    Identifier(String),

    /// End of file
    EOF,
    /// Beginning of file
    BOF,
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            &Token::Dot => write!(f, "."),
            &Token::Comma => write!(f, ","),
            &Token::Colon => write!(f, ":"),
            &Token::ColonColon => write!(f, "::"),
            &Token::ColonLt => write!(f, ":<"),
            &Token::LBrace => write!(f, "{{"),
            &Token::RBrace => write!(f, "}}"),
            &Token::LSqBracket => write!(f, "["),
            &Token::RSqBracket => write!(f, "]"),
            &Token::Lt => write!(f, "<"),
            &Token::Gt => write!(f, ">"),
            &Token::LParen => write!(f, "("),
            &Token::RParen => write!(f, ")"),
            &Token::RArrow => write!(f, "->"),
            &Token::LessEqual => write!(f, "<="),
            &Token::GreaterEqual => write!(f, ">="),
            &Token::EqualsEquals => write!(f, "=="),
            &Token::NotEquals => write!(f, "!="),
            &Token::And => write!(f, "&"),
            &Token::Pipe => write!(f, "|"),
            &Token::Equals => write!(f, "="),
            &Token::Not => write!(f, "!"),
            &Token::Plus => write!(f, "+"),
            &Token::Minus => write!(f, "-"),
            &Token::ShiftLeft => write!(f, "<<"),
            &Token::ShiftRight => write!(f, ">>"),
            &Token::Star => write!(f, "*"),
            &Token::Slash => write!(f, "/"),
            &Token::Modulo => write!(f, "%"),
            &Token::Caret => write!(f, "^"),
            &Token::Fn => write!(f, "fn"),
            &Token::Export => write!(f, "export"),
            &Token::Trait => write!(f, "trait"),
            &Token::Impl => write!(f, "impl"),
            &Token::Where => write!(f, "where"),
            &Token::For => write!(f, "for"),
            &Token::Let => write!(f, "let"),
            &Token::If => write!(f, "if"),
            &Token::Else => write!(f, "else"),
            &Token::While => write!(f, "while"),
            &Token::Break => write!(f, "break"),
            &Token::Continue => write!(f, "continue"),
            &Token::Return => write!(f, "return"),
            &Token::Assert => write!(f, "assert"),
            &Token::True => write!(f, "true"),
            &Token::False => write!(f, "false"),
            &Token::Null => write!(f, "null"),
            &Token::Object => write!(f, "object"),
            &Token::Has => write!(f, "has"),
            &Token::SelfRef => write!(f, "self"),
            &Token::Allocate => write!(f, "allocate"),
            &Token::Int => write!(f, "Type (Int)"),
            &Token::UInt => write!(f, "Type (UInt)"),
            &Token::Bool => write!(f, "Type (Bool)"),
            &Token::Float => write!(f, "Type (Float)"),
            &Token::Char => write!(f, "Type (Char)"),
            &Token::StringType => write!(f, "Type (String)"),
            &Token::Infer => write!(f, "Type (_)"),
            &Token::SelfType => write!(f, "Type (Self)"),
            &Token::String(..) => write!(f, "String"),
            &Token::IntLiteral(_) => write!(f, "Number"),
            &Token::UIntLiteral(_) => write!(f, "Number"),
            &Token::FloatLiteral(_) => write!(f, "Number"),
            &Token::CharLiteral(_) => write!(f, "Char Literal"),
            &Token::Identifier(_) => write!(f, "Identifier"),
            &Token::EOF => write!(f, "EOF"),
            &Token::BOF => write!(f, "BOF"),
        }
    }
}
