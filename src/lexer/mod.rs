mod token;
pub use self::token::Token;
use crate::util::{FileId, FileReader, PResult, Span};

pub struct SpanToken(pub Token, pub Span);

/// Mnemonic for the EOF end-of-file character.
const EOF: char = '\x00';

pub enum LexStringChar {
    Char(char),
    QuoteEnd,
    InterpolateBegin,
}

/// A `Lexer` is a stream of relevant tokens that can be used by
/// the Cheshire parser.
pub struct Lexer {
    file: FileReader,
    next_span: Span,
    next_token: Token,

    interp_parenthetical: Vec<usize>,
}

impl Lexer {
    pub fn new(file: FileId) -> PResult<Lexer> {
        let lexer = Lexer {
            next_span: Span::new(file, 0, 0),
            file: FileReader::new(file)?,
            next_token: Token::BOF,
            interp_parenthetical: vec![],
        };

        Ok(lexer)
    }

    /// Retrieves the token waiting to be consumed by the parser.
    ///
    /// Does not actually consume the token, but instead returns a
    /// clone of the current lookahead-cached token.
    pub fn peek_token(&self) -> Token {
        self.next_token.clone()
    }

    /// Retrieves the position of the next token waiting to be
    /// consumed by the parser.
    pub fn peek_token_span(&self) -> Span {
        self.next_span
    }

    /// Retrieves _and consumes_ the next token.
    ///
    /// Caches the next token so it may be retrieved by `peek_token`.
    pub fn bump_token(&mut self) -> PResult<SpanToken> {
        let last_token = self.next_token.clone();
        let last_token_span = self.next_span;

        self.discard_whitespace_or_comments()?;

        let next_span_start = self.file.current_pos();
        self.next_token = self.get_token_internal()?;
        let next_span_end = self.file.current_pos();
        self.next_span = Span::new(self.file.file_id, next_span_start, next_span_end);

        Ok(SpanToken(last_token, last_token_span))
    }

    /// Returns the next lookahead token.
    fn get_token_internal(&mut self) -> PResult<Token> {
        let c = self.current_char();

        if c == EOF {
            Ok(Token::EOF)
        } else if c == '"' {
            self.scan_string()
        } else if c == '\'' {
            self.scan_char_literal()
        } else if c == '$' {
            self.scan_llvm_literal()
        } else if is_numeric(c) {
            self.scan_numeric_literal()
        } else if is_identifier_start(c) {
            self.scan_identifier_or_keyword()
        } else {
            match c {
                '.' => {
                    self.bump(1);

                    if self.current_char() == '.' {
                        self.bump(1);

                        if self.current_char() != '.' {
                            return perror_at!(
                                self.next_span,
                                "Expected a third dot for ellipsis..."
                            );
                        }

                        self.bump(1);
                        Ok(Token::Ellipsis)
                    } else {
                        Ok(Token::Dot)
                    }
                },
                ',' => {
                    self.bump(1);

                    if self.current_char() == ',' {
                        self.bump(1);

                        if self.current_char() != ',' {
                            return perror_at!(
                                self.next_span,
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
                    if self.next_char() == '<' {
                        self.bump(2);
                        Ok(Token::ColonLt)
                    } else if self.next_char() == ':' {
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
                    if let Some(0) = self.interp_parenthetical.last() {
                        self.scan_interp_continue()
                    } else {
                        if let Some(n) = self.interp_parenthetical.last_mut() {
                            *n -= 1;
                        }

                        self.bump(1);
                        Ok(Token::RParen)
                    },
                '<' => match self.next_char() {
                    '=' => {
                        self.bump(2);
                        Ok(Token::LessEqual)
                    },
                    _ => {
                        self.bump(1);
                        Ok(Token::Lt)
                    },
                },
                '>' => match self.next_char() {
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
                    if self.next_char() == '=' {
                        self.bump(2);
                        Ok(Token::NotEquals)
                    } else {
                        self.bump(1);
                        Ok(Token::Bang)
                    },
                '&' => {
                    self.bump(1);
                    Ok(Token::And)
                },
                '|' => {
                    self.bump(1);
                    Ok(Token::Pipe)
                },
                '=' =>
                    if self.next_char() == '=' {
                        self.bump(2);
                        Ok(Token::EqualsEquals)
                    } else if self.next_char() == '>' {
                        self.bump(2);
                        Ok(Token::RBigArrow)
                    } else {
                        self.bump(1);
                        Ok(Token::Equals)
                    },
                '+' => {
                    self.bump(1);
                    Ok(Token::Plus)
                },
                '-' =>
                    if self.next_char() == '>' {
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
                c => perror_at!(self.next_span, "Unknown symbol '{}'", c),
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

        match self.current_char() {
            '\\' => {
                match self.next_char() {
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
                            self.next_span,
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
                return perror_at!(self.next_span, "Reached end of line in string");
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

        let c = match self.current_char() {
            '\\' => {
                let esc = match self.next_char() {
                    'r' => '\r',
                    'n' => '\n',
                    't' => '\t',
                    '\'' => '\'',
                    '\\' => '\\',
                    c => {
                        return perror_at!(
                            self.next_span,
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

        if self.current_char() != '\'' {
            return perror_at!(self.next_span, "Unclosed character literal");
        }

        self.bump(1);
        return Ok(Token::CharLiteral(c));
    }

    fn scan_llvm_literal(&mut self) -> PResult<Token> {
        let mut string = String::new();
        self.bump(1);

        while is_identifier_continuer(self.current_char()) {
            string.push(self.current_char());
            self.bump(1);
        }

        Ok(Token::InstructionLiteral(string))
    }

    /// Scans a numeric literal, consuming it and converting it to a token in
    /// the process.
    fn scan_numeric_literal(&mut self) -> PResult<Token> {
        let mut string = String::new();

        while is_numeric(self.current_char()) {
            string.push(self.current_char());
            self.bump(1);
        }

        if self.current_char() == '.' && is_numeric(self.next_char()) {
            string += ".";
            self.bump(1);

            while let c @ '0'..='9' = self.current_char() {
                self.bump(1);
                string.push(c);
            }

            if self.current_char() == 'e' || self.current_char() == 'E' {
                self.bump(1);
                string.push('e');

                if self.current_char() == '+' || self.current_char() == '-' {
                    let c = self.current_char();
                    self.bump(1);
                    string.push(c);
                }

                let mut expect_number = false;

                while let c @ '0'..='9' = self.current_char() {
                    expect_number = true;
                    self.bump(1);
                    string.push(c);
                }

                if !expect_number {
                    return perror!(
                        "Expected a numerical value following the exponential: {}",
                        string
                    );
                }
            }

            Ok(Token::FloatLiteral(string))
        }
        /* else if self.current_char() == 'u' {
            self.bump(1);
            Ok(Token::UIntLiteral(string))
        } */
        else {
            Ok(Token::IntLiteral(string))
        }
    }

    // Scans an identifier, unless it matches a keyword.
    fn scan_identifier_or_keyword(&mut self) -> PResult<Token> {
        let mut string = String::new();

        string.push(self.current_char());
        self.bump(1);

        while is_identifier_continuer(self.current_char()) {
            string.push(self.current_char());
            self.bump(1);
        }

        let token = match &*string {
            "_" => Token::Underscore,

            "use" => Token::Use,
            "pub" => Token::Pub,
            "mod" => Token::Mod,

            "fn" => Token::Fn,
            "export" => Token::Export,
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

            "Int" => Token::Int,
            "Float" => Token::Float,
            "Bool" => Token::Bool,
            "String" => Token::StringType,
            "Char" => Token::Char,
            "Self" => Token::SelfType,
            "Fn" => Token::FnTrait,
            "ClosureEnvironment" => Token::ClosureEnvType,

            _ => match string.chars().nth(0).unwrap() {
                '_' => Token::GenericName(string[1..].into()),
                'A'..='Z' => Token::TypeName(string),
                'a'..='z' => Token::Identifier(string),
                _ => {
                    return perror_at!(
                        self.next_span,
                        "TODO: This should never happen, ever. `{}`",
                        string
                    );
                },
            },
        };

        Ok(token)
    }

    fn discard_whitespace_or_comments(&mut self) -> PResult<()> {
        loop {
            // Consume any whitespace or comments before a real token
            match self.current_char() {
                '/' => {
                    // May be a comment or division..=
                    if self.next_char() == '/' || self.next_char() == '*' {
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
        let start_pos = self.file.current_pos();

        match self.next_char() {
            '/' => {
                // Read until end of line.
                self.bump(2); // Read //.
                while self.current_char() != '\r'
                    && self.current_char() != '\n'
                    && self.current_char() != EOF
                {
                    self.bump(1);
                }
                self.bump(1);
            },
            '*' => {
                // Read until */ is encountered
                self.bump(2); // Read /*.
                while self.current_char() != '*' || self.next_char() != '/' {
                    self.bump(1);
                    if self.current_char() == EOF {
                        let end_pos = self.file.current_pos();

                        return perror_at!(
                            Span::new(self.file.file_id, start_pos, end_pos),
                            "EOF encountered in block comment",
                        );
                    }
                }
                self.bump(2);
            },
            _ => unreachable!(),
        }

        Ok(())
    }

    // These are just shortcuts so we don't need to type `self.file`.

    pub fn id(&self) -> FileId {
        self.file.file_id
    }

    fn bump(&mut self, n: usize) {
        self.file.bump(n);
    }

    fn current_char(&self) -> char {
        self.file.current_char()
    }

    fn next_char(&self) -> char {
        self.file.next_char()
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
