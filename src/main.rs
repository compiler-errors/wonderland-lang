#![deny(unused_must_use)]

#[macro_use]
extern crate lazy_static;

use crate::analyze::analyze;
use crate::lexer::Lexer;
use crate::parser::parse_file;
use crate::tyck::typecheck;
use crate::util::result::*;
use crate::util::FileReader;
use std::io::Read;
use crate::inst::instantiate;

mod analyze;
mod lexer;
mod parser;
mod tyck;
mod inst;
mod util;

fn main() {
    let mut string = String::new();

    if let e @ Err(_) = std::io::stdin().lock().read_to_string(&mut string) {
        panic!("File reading issue: {:?}", e);
    }

    let mut file_reader = FileReader::new(string);

    match try_main(&mut file_reader) {
        Ok(_) => println!("Okay!"),
        Err(e) => report_err_at(&file_reader, e),
    }
}

fn try_main(file_reader: &mut FileReader) -> PResult<()> {
    let mut lexer = Lexer::new(file_reader);

    let parsed_file = parse_file(&mut lexer)?;

    // (Transformed) parsed file and analyzed file.
    let (parsed_file, analyzed_file) = analyze(parsed_file)?;

    typecheck(&parsed_file, &analyzed_file)?;

    //let instantiated_file = instantiate(parsed_file, analyzed_file)?;

    Ok(())
}
