#![deny(unused_must_use)]

use crate::lexer::Lexer;
use crate::parser::{ParseFile, Parser};
use crate::util::result::{report_err_at, PError, PResult};
use crate::util::FileReader;
use std::io::{stdin, Read};

mod analyze;
mod lexer;
mod parser;
mod util;

fn main() {
    let mut string = String::new();

    if let e @ Err(_) = stdin().lock().read_to_string(&mut string) {
        panic!("File reading issue: {:?}", e);
    }

    let mut file_reader = FileReader::new(string);
    let parse_file = parse_file(&mut file_reader);

    match parse_file {
        Ok(_) => println!("Okay!"),
        Err(e) => report_err_at(&file_reader, e),
    }
}

fn parse_file(file_reader: &mut FileReader) -> PResult<ParseFile> {
    let lexer = Lexer::new(file_reader);
    let parser = Parser::new(lexer);
    parser.parse_file()
}
