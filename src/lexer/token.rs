use std::fmt::{Display, Formatter, Result};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Token {
    // Language symbols
    Dot,         //    .
    Ellipsis,    //    ...
    Comma,       //    ,
    Commalipses, //    ,,,
    Colon,       //    :
    ColonColon,  //    ::
    SemiColon,   //    ;
    ColonLt,     //    :<
    LBrace,      //    {
    RBrace,      //    }
    LSqBracket,  //    [
    RSqBracket,  //    ]
    Lt,          //    <
    Gt,          //    >
    LParen,      //    (
    RParen,      //    )
    RArrow,      //    ->
    RBigArrow,   //    =>
    Underscore,  //    _

    // Mathematical operators (excluding LSquare, RSquare)
    LessEqual,    //    <=
    GreaterEqual, //    >=
    EqualsEquals, //    ==
    NotEquals,    //    !=
    And,          //    &
    Pipe,         //    |
    Equals,       //    =
    Bang,         //    !
    Plus,         //    +
    Minus,        //    -
    Star,         //    *
    Slash,        //    /
    Modulo,       //    %

    // Special keywords -- all are lowercase
    Use,
    Pub,
    Mod,
    Fn,
    Export,
    Let,
    Trait,
    Impl,
    Where,
    For,
    In,
    As,
    If,
    Else,
    While,
    Break,
    Continue,
    At,
    Return,
    Assert,
    True,
    False,
    Object,
    Type,
    SelfRef,
    Allocate,
    Enum,
    Match,
    Instruction,

    // Privileged Types
    Int,
    Bool,
    StringType,
    Char,
    SelfType,
    FnTrait,
    ClosureEnvType,

    // Literals
    String(String),
    IntLiteral(String),
    InstructionLiteral(String),
    CharLiteral(char),

    InterpolateBegin(String),
    InterpolateContinue(String),
    InterpolateEnd(String),

    Identifier(String),
    TypeName(String),
    GenericName(String),

    /// End of file
    EOF,
    /// Beginning of file
    BOF,
}

impl Display for Token {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            Token::Dot => write!(f, "."),
            Token::Ellipsis => write!(f, "..."),
            Token::Comma => write!(f, ","),
            Token::Commalipses => write!(f, ",,,"),
            Token::Colon => write!(f, ":"),
            Token::ColonColon => write!(f, "::"),
            Token::ColonLt => write!(f, ":<"),
            Token::SemiColon => write!(f, ";"),
            Token::LBrace => write!(f, "{{"),
            Token::RBrace => write!(f, "}}"),
            Token::LSqBracket => write!(f, "["),
            Token::RSqBracket => write!(f, "]"),
            Token::Lt => write!(f, "<"),
            Token::Gt => write!(f, ">"),
            Token::LParen => write!(f, "("),
            Token::RParen => write!(f, ")"),
            Token::RArrow => write!(f, "->"),
            Token::RBigArrow => write!(f, "=>"),
            Token::Underscore => write!(f, "_"),
            Token::LessEqual => write!(f, "<="),
            Token::GreaterEqual => write!(f, ">="),
            Token::EqualsEquals => write!(f, "=="),
            Token::NotEquals => write!(f, "!="),
            Token::And => write!(f, "&"),
            Token::Pipe => write!(f, "|"),
            Token::Equals => write!(f, "="),
            Token::Bang => write!(f, "!"),
            Token::Plus => write!(f, "+"),
            Token::Minus => write!(f, "-"),
            Token::Star => write!(f, "*"),
            Token::Slash => write!(f, "/"),
            Token::Modulo => write!(f, "%"),
            Token::Use => write!(f, "use"),
            Token::Pub => write!(f, "pub"),
            Token::Mod => write!(f, "mod"),
            Token::Fn => write!(f, "fn"),
            Token::Export => write!(f, "export"),
            Token::Trait => write!(f, "trait"),
            Token::Impl => write!(f, "impl"),
            Token::Where => write!(f, "where"),
            Token::For => write!(f, "for"),
            Token::In => write!(f, "in"),
            Token::As => write!(f, "as"),
            Token::Let => write!(f, "let"),
            Token::If => write!(f, "if"),
            Token::Else => write!(f, "else"),
            Token::While => write!(f, "while"),
            Token::Break => write!(f, "break"),
            Token::Continue => write!(f, "continue"),
            Token::At => write!(f, "at"),
            Token::Return => write!(f, "return"),
            Token::Assert => write!(f, "assert"),
            Token::True => write!(f, "true"),
            Token::False => write!(f, "false"),
            Token::Object => write!(f, "object"),
            Token::Type => write!(f, "type"),
            Token::SelfRef => write!(f, "self"),
            Token::Allocate => write!(f, "allocate"),
            Token::Enum => write!(f, "enum"),
            Token::Match => write!(f, "match"),
            Token::Instruction => write!(f, "instruction"),
            Token::Int => write!(f, "Type (Int)"),
            Token::Bool => write!(f, "Type (Bool)"),
            Token::Char => write!(f, "Type (Char)"),
            Token::StringType => write!(f, "Type (String)"),
            Token::SelfType => write!(f, "Type (Self)"),
            Token::ClosureEnvType => write!(f, "Type (ClosureEnvType)"),
            Token::FnTrait => write!(f, "Fn"),
            Token::String(_) => write!(f, "String"),
            Token::InterpolateBegin(_) => write!(f, "InterpolateBegin"),
            Token::InterpolateContinue(_) => write!(f, "InterpolateContinue"),
            Token::InterpolateEnd(_) => write!(f, "InterpolateEnd"),
            Token::IntLiteral(_) => write!(f, "Number"),
            Token::CharLiteral(_) => write!(f, "CharLiteral"),
            Token::InstructionLiteral(_) => write!(f, "InstructionLiteral"),
            Token::Identifier(_) => write!(f, "Identifier"),
            Token::TypeName(_) => write!(f, "TypeName"),
            Token::GenericName(_) => write!(f, "GenericName"),
            Token::EOF => write!(f, "EOF"),
            Token::BOF => write!(f, "BOF"),
        }
    }
}
