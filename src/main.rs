#![deny(unused_must_use)]

#[macro_use]
extern crate lazy_static;

use crate::analyze::analyze;
use crate::lexer::Lexer;
use crate::parser::parse_file;
use crate::util::result::*;
use crate::util::FileReader;
use std::io::{stdin, Read};

mod analyze;
mod lexer;
mod parser;
mod tyck;
mod util;

fn main() {
    let mut string = String::new();

    if let e @ Err(_) = stdin().lock().read_to_string(&mut string) {
        panic!("File reading issue: {:?}", e);
    }

    let mut file_reader = FileReader::new(string);

    match try_main(&mut file_reader) {
        Ok(_) => println!("Okay!"),
        Err(e) => report_err_at(&file_reader, e),
    }
}

fn try_main(file_reader: &mut FileReader) -> PResult<()> {
    let lexer = Lexer::new(file_reader);
    let parsed_file = parse_file(lexer)?;
    let analyzed_file = analyze(parsed_file)?;
    //let tycked_file =

    Ok(())
}
