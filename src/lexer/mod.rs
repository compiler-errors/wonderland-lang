mod token;

use std::process::exit;
use util::FileReader;
pub use self::token::Token;

pub struct TokenPos(pub Token, pub usize);

/// Mnemonic for the EOF end-of-file character.
const EOF: char = '\x00';

/// A `Lexer` is a stream of relevant tokens that can be used by
/// the Cheshire parser.
pub struct Lexer<'a> {
    pub file: FileReader<'a>,
    next_token_pos: usize,
    next_token: Token,
}

impl<'a> Lexer<'a> {
    pub fn new(file: FileReader<'a>) -> Lexer<'a> {
        let lexer = Lexer {
            file: file,
            next_token_pos: 0,
            next_token: Token::BOF,
        };

        lexer
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
    pub fn peek_token_pos(&self) -> usize {
        self.next_token_pos
    }

    /// Retrieves _and consumes_ the next token.
    ///
    /// Caches the next token so it may be retrieved by `peek_token`.
    pub fn bump_token(&mut self) -> TokenPos {
        let last_token = self.next_token.clone();
        let last_token_pos = self.next_token_pos;

        if let Err((pos, string)) = self.discard_whitespace_or_comments() {
            report_lex_err_at(&self.file, pos, string);
        }

        self.next_token_pos = self.file.current_pos();
        self.next_token = self.get_token_internal().unwrap_or_else(|err| {
            report_lex_err_at(&self.file, self.next_token_pos, err);
        });

        TokenPos(last_token, last_token_pos)
    }

    /// Returns the next lookahead token.
    fn get_token_internal(&mut self) -> Result<Token, String> {
        let c = self.current_char();

        if c == EOF {
            return Ok(Token::EOF);
        } else if c == '"' {
            return self.scan_string();
        } else if c == '\'' {
            return self.scan_char_literal();
        } else if is_numeric(c) {
            return self.scan_numeric_literal();
        } else if is_identifier_start(c) {
            return self.scan_identifier_or_keyword();
        } else {
            match c {
                '.' => {
                    self.bump(1);
                    return Ok(Token::Dot);
                }
                ',' => {
                    self.bump(1);
                    return Ok(Token::Comma);
                }
                ':' => {
                    if self.next_char() == ':' {
                        self.bump(2);
                        return Ok(Token::ColonColon);
                    } else if self.next_char() == '<' {
                        self.bump(2);
                        return Ok(Token::ColonLt);
                    } else {
                        self.bump(1);
                        return Ok(Token::Colon);
                    }
                }
                '{' => {
                    self.bump(1);
                    return Ok(Token::LBrace);
                }
                '}' => {
                    self.bump(1);
                    return Ok(Token::RBrace);
                }
                '[' => {
                    self.bump(1);
                    return Ok(Token::LSqBracket);
                }
                ']' => {
                    self.bump(1);
                    return Ok(Token::RSqBracket);
                }
                '(' => {
                    self.bump(1);
                    return Ok(Token::LParen);
                }
                ')' => {
                    self.bump(1);
                    return Ok(Token::RParen);
                }
                '<' => {
                    match self.next_char() {
                        '=' => {
                            self.bump(2);
                            return Ok(Token::LessEqual);
                        }
                        '<' => {
                            self.bump(2);
                            return Ok(Token::ShiftLeft);
                        }
                        _ => {
                            self.bump(1);
                            return Ok(Token::Lt);
                        }
                    }
                }
                '>' => {
                    match self.next_char() {
                        '=' => {
                            self.bump(2);
                            return Ok(Token::GreaterEqual);
                        }
                        '>' => {
                            self.bump(2);
                            return Ok(Token::ShiftRight);
                        }
                        _ => {
                            self.bump(1);
                            return Ok(Token::Gt);
                        }
                    }
                }
                '!' => {
                    if self.next_char() == '=' {
                        self.bump(2);
                        return Ok(Token::NotEquals);
                    } else {
                        self.bump(1);
                        return Ok(Token::Not);
                    }
                }
                '&' => {
                    self.bump(1);
                    return Ok(Token::And);
                }
                '|' => {
                    self.bump(1);
                    return Ok(Token::Pipe);
                }
                '=' => {
                    if self.next_char() == '=' {
                        self.bump(2);
                        return Ok(Token::EqualsEquals);
                    } else {
                        self.bump(1);
                        return Ok(Token::Equals);
                    }
                }
                '+' => {
                    self.bump(1);
                    return Ok(Token::Plus);
                }
                '-' => {
                    if self.next_char() == '>' {
                        self.bump(2);
                        return Ok(Token::RArrow);
                    } else {
                        self.bump(1);
                        return Ok(Token::Minus);
                    }
                }
                '*' => {
                    self.bump(1);
                    return Ok(Token::Star);
                }
                '/' => {
                    self.bump(1);
                    return Ok(Token::Slash);
                }
                '%' => {
                    self.bump(1);
                    return Ok(Token::Modulo);
                }
                '^' => {
                    self.bump(1);
                    return Ok(Token::Caret);
                }
                c => {
                    return Err(format!("Unknown symbol '{}'", c));
                }
            }
        }
    }

    /// Scans a new parsed string token.
    fn scan_string(&mut self) -> Result<Token, String> {
        self.bump(1); // Blindly consume the quote character
        let mut len = 0;
        let mut string = "".to_string();

        loop {
            match self.current_char() {
                '\\' => {
                    match self.next_char() {
                        'r' => string += "\\0D",
                        'n' => string += "\\0A",
                        't' => string += "\\09",
                        '"' => string += "\\22",
                        '\'' => string += "'",
                        '\\' => string += "\\\\",
                        c => {
                            return Err(format!("Unknown escaped character in string '\\{}'", c));
                        }
                    }
                    len += 1;
                    self.bump(2);
                }
                '\"' => {
                    self.bump(1);
                    break;
                }
                '\r' | '\n' | EOF => {
                    return Err("Reached end of line in string".to_string());
                }
                c => {
                    string.push(c);
                    len += 1;
                    self.bump(1);
                }
            }
        }

        return Ok(Token::String(string, len));
    }

    /// Scans a single character literal token.
    fn scan_char_literal(&mut self) -> Result<Token, String> {
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
                        return Err(format!("Unknown escaped character in literal '\\{}'", c));
                    }
                };
                self.bump(2);
                esc
            }
            c => {
                self.bump(1);
                c
            }
        };

        if self.current_char() != '\'' {
            return Err("Unclosed character literal".to_string());
        }

        self.bump(1);
        return Ok(Token::CharLiteral(c));
    }

    /// Scans a numeric literal, consuming it and converting it to a token in the process.
    fn scan_numeric_literal(&mut self) -> Result<Token, String> {
        let mut string = "".to_string();

        while is_numeric(self.current_char()) {
            string.push(self.current_char());
            self.bump(1);
        }

        if self.current_char() == '.' && is_numeric(self.next_char()) {
            string += ".";
            self.bump(1);

            loop {
                // EOF
                match self.current_char() {
                    c @ '0'...'9' => {
                        self.bump(1);
                        string.push(c);
                    }
                    _ => {
                        break;
                    }
                }
            }

            Ok(Token::FloatLiteral(string))
        } else if self.current_char() == 'u' {
            self.bump(1);
            Ok(Token::UIntLiteral(string))
        } else {
            Ok(Token::IntLiteral(string))
        }
    }

    // Scans an identifier, unless it matches a keyword.
    fn scan_identifier_or_keyword(&mut self) -> Result<Token, String> {
        let mut string = "".to_string();

        string.push(self.current_char());
        self.bump(1);

        while is_identifier_continuer(self.current_char()) {
            string.push(self.current_char());
            self.bump(1);
        }

        Ok(match string.as_ref() {
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
            "break" => Token::Break,
            "continue" => Token::Continue,
            "return" => Token::Return,
            "assert" => Token::Assert,
            "true" => Token::True,
            "false" => Token::False,
            "null" => Token::Null,

            "object" => Token::Object,
            "has" => Token::Has,
            "self" => Token::SelfRef,
            "allocate" => Token::Allocate,

            "Int" => Token::Int,
            "UInt" => Token::UInt,
            "Bool" => Token::Bool,
            "String" => Token::StringType,
            "Float" => Token::Float,
            "Char" => Token::Char,
            "_" => Token::Infer,
            "Self" => Token::SelfType,

            _ => Token::Identifier(string),
        })
    }


    fn discard_whitespace_or_comments(&mut self) -> Result<(), (usize, String)> {
        loop {
            // Consume any whitespace or comments before a real token
            match self.current_char() {
                '/' => {
                    // May be a comment or division...
                    if self.next_char() == '/' || self.next_char() == '*' {
                        self.scan_comment()?;
                    } else {
                        break; // A division sign
                    }
                }
                ' ' | '\t' | '\n' | '\r' => self.bump(1),
                _ => {
                    break;
                }
            }
        }

        Ok(())
    }

    fn scan_comment(&mut self) -> Result<(), (usize, String)> {
        let pos = self.file.current_pos();
        match self.next_char() {
            '/' => {
                // Read until end of line.
                self.bump(2); // Read //.
                while self.current_char() != '\r' && self.current_char() != '\n' &&
                      self.current_char() != EOF {
                    self.bump(1);
                }
                self.bump(1);
            }
            '*' => {
                // Read until */ is encountered
                self.bump(2); // Read /*.
                while self.current_char() != '*' || self.next_char() != '/' {
                    self.bump(1);
                    if self.current_char() == EOF {
                        return Err((pos, "EOF encountered in block comment".to_string()));
                    }
                }
                self.bump(2);
            }
            _ => unreachable!(),
        }

        Ok(())
    }

    // These are just shortcuts so we don't need to type `self.file`.

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
        '0'...'9' => true,
        _ => false,
    }
}

/// Returns whether the character is a valid beginning of an identifier.
fn is_identifier_start(c: char) -> bool {
    match c {
        'a'...'z' => true,
        'A'...'Z' => true,
        '_' => true,
        _ => false,
    }
}

/// Returns whether the character is a valid non-initial part of an identifier.
fn is_identifier_continuer(c: char) -> bool {
    match c {
        'a'...'z' => true,
        'A'...'Z' => true,
        '0'...'9' => true,
        '_' => true,
        _ => false,
    }
}

fn report_lex_err_at(fr: &FileReader, pos: usize, err: String) -> ! {
    let (line, col) = fr.get_row_col(pos);
    let line_str = fr.get_line_from_pos(pos);

    println!("");
    // TODO: fix tabs later
    println!("Error \"{}\" encountered on line {}:", err, line + 1); //TODO: in file

    println!("| {}", line_str);
    for _ in 0..(col + 2) {
        print!("-");
    }
    println!("^");
    exit(1);
}
