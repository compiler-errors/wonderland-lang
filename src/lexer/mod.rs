mod token;
pub use crate::lexer::token::*;
use crate::util::{FileId, PResult, Span};
use std::str::Chars;

/// Mnemonic for the EOF end-of-file character.
const EOF: char = '\x00';

pub type SpanToken = (usize, Token, usize);

pub struct Lexer<'input> {
    file: FileId,
    stream: Chars<'input>,
    quotable: bool,

    current_pos: usize,
    current_char: char,
    next_char: char,

    interp_parenthetical: Vec<usize>,
}

pub enum LexStringChar {
    Char(char),
    QuoteEnd,
    InterpolateBegin,
}

impl<'input> Lexer<'input> {
    pub fn new(file: FileId, input: &'input str, quotable: bool) -> Lexer<'input> {
        let mut lex = Lexer {
            file,
            stream: input.chars(),
            current_pos: 0,
            current_char: EOF,
            next_char: EOF,
            interp_parenthetical: vec![],
            quotable,
        };

        lex.bump(2);
        lex.current_pos = 0;
        lex
    }

    fn spanned_lex(&mut self) -> PResult<SpanToken> {
        self.discard_whitespace_or_comments()?;

        let start = self.current_pos;
        let t = self.lex()?;
        let end = self.current_pos;

        Ok((start, t, end))
    }

    fn bump(&mut self, n: usize) {
        for _ in 0..n {
            self.current_char = self.next_char;
            self.next_char = self.stream.next().unwrap_or(EOF);
            self.current_pos += 1;
        }
    }

    fn discard_whitespace_or_comments(&mut self) -> PResult<()> {
        loop {
            // Consume any whitespace or comments before a real token
            match self.current_char {
                '/' => {
                    // May be a comment or division..=
                    if self.next_char == '/' || self.next_char == '*' {
                        self.scan_comment()?;
                    } else {
                        break; // A division sign
                    }
                },
                ' ' | '\t' | '\n' | '\r' => self.bump(1),
                _ => {
                    break;
                },
            }
        }

        Ok(())
    }

    fn scan_comment(&mut self) -> PResult<()> {
        let start_pos = self.current_pos;

        match self.next_char {
            '/' => {
                // Read until end of line.
                self.bump(2); // Read //.
                while self.current_char != '\r'
                    && self.current_char != '\n'
                    && self.current_char != EOF
                {
                    self.bump(1);
                }
                self.bump(1);
            },
            '*' => {
                // Read until */ is encountered
                self.bump(2); // Read /*.
                while self.current_char != '*' || self.next_char != '/' {
                    self.bump(1);
                    if self.current_char == EOF {
                        let end_pos = self.current_pos;

                        return perror_at!(
                            Span::new(self.file, start_pos, end_pos),
                            "EOF encountered in block comment",
                        );
                    }
                }
                self.bump(2);
            },
            _ => unreachable!("ICE: Expected // or /* to begin scanned comment"),
        }

        Ok(())
    }

    /// Returns the next lookahead token.
    fn lex(&mut self) -> PResult<Token> {
        let c = self.current_char;

        if c == EOF {
            Ok(Token::EOF)
        } else if c == '"' {
            self.scan_string()
        } else if c == '\'' {
            self.scan_char_literal()
        } else if c == '$' {
            if self.quotable && self.next_char == '[' {
                self.scan_quotation_start()
            } else {
                self.scan_instruction_literal()
            }
        } else if is_numeric(c) {
            self.scan_numeric_literal()
        } else if is_identifier_start(c) {
            self.scan_identifier_or_keyword()
        } else {
            match c {
                '.' => {
                    self.bump(1);
                    if self.current_char == '.' {
                        self.bump(1);
                        if self.current_char == '.' {
                            self.bump(1);
                            Ok(Token::Ellipsis)
                        } else {
                            Ok(Token::DotDot)
                        }
                    } else {
                        Ok(Token::Dot)
                    }
                },
                ',' => {
                    self.bump(1);

                    if self.current_char == ',' {
                        self.bump(1);

                        if self.current_char != ',' {
                            return perror_at!(
                                Span::new(self.file, self.current_pos, self.current_pos + 1),
                                "Expected a third comma for commalipses,,,"
                            );
                        }

                        self.bump(1);
                        Ok(Token::Commalipses)
                    } else {
                        Ok(Token::Comma)
                    }
                },
                ':' =>
                    if self.next_char == '<' {
                        self.bump(2);
                        Ok(Token::ColonLt)
                    } else if self.next_char == ':' {
                        self.bump(2);
                        Ok(Token::ColonColon)
                    } else {
                        self.bump(1);
                        Ok(Token::Colon)
                    },
                ';' => {
                    self.bump(1);
                    Ok(Token::SemiColon)
                },
                '{' => {
                    self.bump(1);
                    Ok(Token::LBrace)
                },
                '}' => {
                    self.bump(1);
                    Ok(Token::RBrace)
                },
                '[' => {
                    self.bump(1);
                    Ok(Token::LSqBracket)
                },
                ']' => {
                    self.bump(1);
                    Ok(Token::RSqBracket)
                },
                '(' => {
                    self.bump(1);

                    if let Some(nest) = self.interp_parenthetical.last_mut() {
                        *nest += 1;
                    }

                    Ok(Token::LParen)
                },
                ')' =>
                    if self.quotable && self.next_char == '$' {
                        self.bump(2);
                        Ok(Token::QuotationEnd)
                    } else if let Some(0) = self.interp_parenthetical.last() {
                        self.scan_interp_continue()
                    } else {
                        if let Some(n) = self.interp_parenthetical.last_mut() {
                            *n -= 1;
                        }

                        self.bump(1);
                        Ok(Token::RParen)
                    },
                '<' => match self.next_char {
                    '=' => {
                        self.bump(2);
                        Ok(Token::LessEqual)
                    },
                    _ => {
                        self.bump(1);
                        Ok(Token::Lt)
                    },
                },
                '>' => match self.next_char {
                    '=' => {
                        self.bump(2);
                        Ok(Token::GreaterEqual)
                    },
                    _ => {
                        self.bump(1);
                        Ok(Token::Gt)
                    },
                },
                '!' =>
                    if self.next_char == '=' {
                        self.bump(2);
                        Ok(Token::NotEquals)
                    } else {
                        self.bump(1);
                        Ok(Token::Bang)
                    },
                '&' =>
                    if self.next_char == '?' {
                        self.bump(2);
                        Ok(Token::AndShort)
                    } else {
                        self.bump(1);
                        Ok(Token::And)
                    },
                '|' =>
                    if self.next_char == '?' {
                        self.bump(2);
                        Ok(Token::PipeShort)
                    } else {
                        self.bump(1);
                        Ok(Token::Pipe)
                    },
                '=' =>
                    if self.next_char == '=' {
                        self.bump(2);
                        Ok(Token::EqualsEquals)
                    } else if self.next_char == '>' {
                        self.bump(2);
                        Ok(Token::RBigArrow)
                    } else {
                        self.bump(1);
                        Ok(Token::Equals)
                    },
                '+' =>
                    if is_numeric(self.next_char) {
                        self.scan_numeric_literal()
                    } else {
                        self.bump(1);
                        Ok(Token::Plus)
                    },
                '-' =>
                    if is_numeric(self.next_char) {
                        self.scan_numeric_literal()
                    } else if self.next_char == '>' {
                        self.bump(2);
                        Ok(Token::RArrow)
                    } else {
                        self.bump(1);
                        Ok(Token::Minus)
                    },
                '*' => {
                    self.bump(1);
                    Ok(Token::Star)
                },
                '/' => {
                    self.bump(1);
                    Ok(Token::Slash)
                },
                '%' => {
                    self.bump(1);
                    Ok(Token::Modulo)
                },
                '?' => {
                    self.bump(1);
                    Ok(Token::Question)
                },
                c => perror_at!(
                    Span::new(self.file, self.current_pos, self.current_pos + 1),
                    "Unknown symbol '{}'",
                    c
                ),
            }
        }
    }

    /// Scans a new parsed string token.
    fn scan_string(&mut self) -> PResult<Token> {
        self.bump(1); // Blindly consume the quote character
        let mut string = String::new();

        loop {
            match self.scan_string_char()? {
                LexStringChar::Char(c) => {
                    string.push(c);
                },
                LexStringChar::QuoteEnd => {
                    return Ok(Token::String(string));
                },
                LexStringChar::InterpolateBegin => {
                    self.interp_parenthetical.push(0);
                    return Ok(Token::InterpolateBegin(string));
                },
            }
        }
    }

    fn scan_interp_continue(&mut self) -> PResult<Token> {
        self.bump(1); // Blindly consume the rparen character
        let mut string = String::new();

        loop {
            match self.scan_string_char()? {
                LexStringChar::Char(c) => {
                    string.push(c);
                },
                LexStringChar::QuoteEnd => {
                    self.interp_parenthetical.pop();
                    return Ok(Token::InterpolateEnd(string));
                },
                LexStringChar::InterpolateBegin => {
                    return Ok(Token::InterpolateContinue(string));
                },
            }
        }
    }

    fn scan_string_char(&mut self) -> PResult<LexStringChar> {
        let ret;

        match self.current_char {
            '\\' => {
                match self.next_char {
                    'r' => {
                        ret = LexStringChar::Char('\r');
                    },
                    'n' => {
                        ret = LexStringChar::Char('\n');
                    },
                    't' => {
                        ret = LexStringChar::Char('\t');
                    },
                    '"' => {
                        ret = LexStringChar::Char('\"');
                    },
                    '\'' => {
                        ret = LexStringChar::Char('\'');
                    },
                    '\\' => {
                        ret = LexStringChar::Char('\\');
                    },
                    '(' => {
                        ret = LexStringChar::InterpolateBegin;
                    },
                    c => {
                        return perror_at!(
                            Span::new(self.file, self.current_pos, self.current_pos + 1),
                            "Unknown escaped character in string '\\{}'",
                            c
                        );
                    },
                }
                self.bump(2);
            },
            '\"' => {
                ret = LexStringChar::QuoteEnd;
                self.bump(1);
            },
            '\r' | '\n' | EOF => {
                return perror_at!(
                    Span::new(self.file, self.current_pos, self.current_pos + 1),
                    "Reached end of line in string"
                );
            },
            c => {
                ret = LexStringChar::Char(c);
                self.bump(1);
            },
        }

        Ok(ret)
    }

    /// Scans a single character literal token.
    fn scan_char_literal(&mut self) -> PResult<Token> {
        self.bump(1);

        let c = match self.current_char {
            '\\' => {
                let esc = match self.next_char {
                    'r' => '\r',
                    'n' => '\n',
                    't' => '\t',
                    '\'' => '\'',
                    '\\' => '\\',
                    c => {
                        return perror_at!(
                            Span::new(self.file, self.current_pos, self.current_pos + 1),
                            "Unknown escaped character in literal '\\{}'",
                            c
                        );
                    },
                };
                self.bump(2);
                esc
            },
            c => {
                self.bump(1);
                c
            },
        };

        if self.current_char != '\'' {
            return perror_at!(
                Span::new(self.file, self.current_pos, self.current_pos + 1),
                "Unclosed character literal"
            );
        }

        self.bump(1);
        return Ok(Token::CharLiteral(c));
    }

    fn scan_quotation_start(&mut self) -> PResult<Token> {
        self.bump(2);

        if !is_identifier_start(self.current_char) {
            return perror_at!(
                Span::new(self.file, self.current_pos, self.current_pos + 1),
                "Expected start of identifier for quotation"
            );
        }

        let string_start = self.current_pos;
        let mut string = String::new();
        string.push(self.current_char);
        self.bump(1);

        while is_identifier_continuer(self.current_char) {
            string.push(self.current_char);
            self.bump(1);
        }

        let token = match string.as_str() {
            "ModuleRef" => Token::QuotationStart("ModuleRef"),
            "SpannedExpr" => Token::QuotationStart("SpannedExpr"),
            "Identifier" => Token::QuotationStart("Identifier"),
            "For" => Token::QuotationStart("For"),
            "While" => Token::QuotationStart("While"),
            "Break" => Token::QuotationStart("Break"),
            "Block" => Token::QuotationStart("Block"),
            "Continue" => Token::QuotationStart("Continue"),
            "SpannedPattern" => Token::QuotationStart("SpannedPattern"),
            "MatchBranch" => Token::QuotationStart("MatchBranch"),
            "NamedVariable" => Token::QuotationStart("NamedVariable"),
            "Infer" => Token::QuotationStart("Infer"),
            "GenericPlaceholder" => Token::QuotationStart("GenericPlaceholder"),
            "DummyGeneric" => Token::QuotationStart("DummyGeneric"),
            "Dummy" => Token::QuotationStart("Dummy"),
            "StaticCall" => Token::QuotationStart("StaticCall"),
            "GlobalVariable" => Token::QuotationStart("GlobalVariable"),
            "GlobalFn" => Token::QuotationStart("GlobalFn"),
            "Closure" => Token::QuotationStart("Closure"),
            "Async" => Token::QuotationStart("Async"),
            other => {
                return perror_at!(
                    Span::new(self.file, string_start, self.current_pos),
                    "Unknown quotation mode `$[{}]`",
                    other
                );
            },
        };

        if self.current_char != ']' || self.next_char != '(' {
            return perror_at!(
                Span::new(self.file, self.current_pos, self.current_pos + 1),
                "Expected `](` after beginning of quotation"
            );
        }

        self.bump(2);
        Ok(token)
    }

    fn scan_instruction_literal(&mut self) -> PResult<Token> {
        let mut string = String::new();
        self.bump(1);

        while is_identifier_continuer(self.current_char) {
            string.push(self.current_char);
            self.bump(1);
        }

        Ok(Token::InstructionLiteral(string))
    }

    /// Scans a numeric literal, consuming it and converting it to a token in
    /// the process.
    fn scan_numeric_literal(&mut self) -> PResult<Token> {
        let mut string = String::new();

        if self.current_char == '-' || self.current_char == '+' {
            if self.current_char == '-' {
                string.push(self.current_char);
            }

            self.bump(1);

            if !is_numeric(self.current_char) {
                unreachable!("ICE: Tried to parse a numeric literal, but this is a plain number!");
            }
        }

        while is_numeric(self.current_char) {
            string.push(self.current_char);
            self.bump(1);
        }

        if self.current_char == '.' && is_numeric(self.next_char) {
            string += ".";
            self.bump(1);

            while is_numeric(self.current_char) {
                string.push(self.current_char);
                self.bump(1);
            }

            if self.current_char == 'e' || self.current_char == 'E' {
                self.bump(1);
                string.push('e');

                if self.current_char == '+' || self.current_char == '-' {
                    string.push(self.current_char);
                    self.bump(1);
                }

                let mut expect_number = false;

                while is_numeric(self.current_char) {
                    expect_number = true;
                    string.push(self.current_char);
                    self.bump(1);
                }

                if !expect_number {
                    return perror!(
                        "Expected a numerical value following the exponential: {}",
                        string
                    );
                }
            }

            debug!("Scanned Float `{}`", string);
            Ok(Token::FloatLiteral(string))
        }
        /* else if self.current_char == 'u' {
            self.bump(1);
            Ok(Token::UIntLiteral(string))
        } */
        else {
            debug!("Scanned Int `{}`", string);
            Ok(Token::IntLiteral(string))
        }
    }

    // Scans an identifier, unless it matches a keyword.
    fn scan_identifier_or_keyword(&mut self) -> PResult<Token> {
        let mut string = String::new();

        string.push(self.current_char);
        self.bump(1);

        while is_identifier_continuer(self.current_char) {
            string.push(self.current_char);
            self.bump(1);
        }

        let token = match string.as_str() {
            "_" => Token::Underscore,

            "use" => Token::Use,
            "pub" => Token::Pub,
            "mod" => Token::Mod,

            "fn" => Token::Fn,
            "extern" => Token::Extern,
            "trait" => Token::Trait,
            "impl" => Token::Impl,
            "where" => Token::Where,

            "let" => Token::Let,
            "if" => Token::If,
            "else" => Token::Else,
            "while" => Token::While,
            "for" => Token::For,
            "in" => Token::In,
            "as" => Token::As,
            "break" => Token::Break,
            "continue" => Token::Continue,
            "at" => Token::At,
            "return" => Token::Return,
            "assert" => Token::Assert,
            "true" => Token::True,
            "false" => Token::False,

            "object" => Token::Object,
            "type" => Token::Type,
            "self" => Token::SelfRef,
            "allocate" => Token::Allocate,

            "enum" => Token::Enum,
            "match" => Token::Match,

            "instruction" => Token::Instruction,

            "async" => Token::Async,
            "await" => Token::Await,

            "Int" => Token::Int,
            "Float" => Token::Float,
            "Bool" => Token::Bool,
            "String" => Token::StringType,
            "Char" => Token::Char,
            "Self" => Token::SelfType,
            "Fn" => Token::FnTrait,
            "Dyn" => Token::DynTrait,
            "ClosureEnvironment" => Token::ClosureEnvironment,

            _ => match string.chars().nth(0).unwrap() {
                '_' => Token::GenericName(string[1..].into()),
                'A'..='Z' => Token::TypeName(string),
                'a'..='z' => Token::Identifier(string),
                _ => {
                    return perror_at!(
                        Span::new(self.file, self.current_pos, self.current_pos + 1),
                        "TODO: This should never happen, ever. `{}`",
                        string
                    );
                },
            },
        };

        Ok(token)
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = PResult<SpanToken>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.spanned_lex() {
            Ok((_, Token::EOF, _)) => None,
            t => Some(t),
        }
    }
}

/// Returns whether the character is a valid part of a number.
fn is_numeric(c: char) -> bool {
    match c {
        '0'..='9' => true,
        _ => false,
    }
}

/// Returns whether the character is a valid beginning of an identifier.
fn is_identifier_start(c: char) -> bool {
    match c {
        'a'..='z' => true,
        'A'..='Z' => true,
        '_' => true,
        _ => false,
    }
}

/// Returns whether the character is a valid non-initial part of an identifier.
fn is_identifier_continuer(c: char) -> bool {
    match c {
        'a'..='z' => true,
        'A'..='Z' => true,
        '0'..='9' => true,
        '_' => true,
        _ => false,
    }
}
