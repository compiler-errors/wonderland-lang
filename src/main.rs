#![deny(unused_must_use)]
#![feature(result_cloned)]

#[macro_use]
extern crate lazy_static;
extern crate getopts;
extern crate inkwell;

use crate::ana::analyze;
use crate::inst::instantiate;
use crate::lexer::{Lexer, Token};
use crate::parser::parse_program;
use crate::tr::translate;
use crate::tyck::typecheck;
use crate::util::{report_err, FileId, FileRegistry, PResult};
use getopts::{Matches, Options};

use std::path::Path;
use std::process::exit;

mod ana;
mod inst;
mod lexer;
mod parser;
mod tr;
mod tyck;
mod util;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum Mode {
    Help,
    Lex,
    Parse,
    Analyze,
    Typecheck,
    Instantiate,
    Translate,
}

const DEFAULT_MODE: Mode = Mode::Translate;

fn main() {
    let args: Vec<_> = std::env::args().collect();

    let mut opts = Options::new();
    opts.optflag("h", "help", "print this help menu");
    opts.optopt("o", "output", "set output file name", "FILE");

    opts.optflag("l", "lex", "Tokenize file(s)");
    opts.optflag("p", "parse", "Parse file(s)");
    opts.optflag("a", "analyze", "Analyze file(s)");
    opts.optflag("t", "tyck", "Typecheck file(s)");
    opts.optflag("i", "inst", "Instantiate generics and typecheck file(s)");
    opts.optflag("c", "tr", "Compile, a.k.a. translate, file(s)");

    opts.optflag("L", "llvm-ir", "Compile file(s) to LLVM IR");
    opts.optopt("O", "output", "output file", "FILE");

    let matches = opts.parse(&args[1..]).unwrap_or_else(|_| {
        println!("Something went wrong when parsing...");
        help(true);
    });

    let mode = select_mode(&matches).unwrap_or_else(|_| {
        println!("Duplicated options, please choose one compiler mode!");
        help(true);
    });

    let files = get_files(mode, &matches.free).unwrap_or_else(|e| report_err(e));
    let llvm_ir = matches.opt_present("L");
    let output_file = matches.opt_str("O").unwrap_or_else(|| {
        if llvm_ir {
            "cheshire.out.ll".into()
        } else {
            "cheshire.out.o".into()
        }
    });

    match mode {
        Mode::Help => help(false),
        Mode::Lex => try_lex(files),
        Mode::Parse => try_parse(files),
        Mode::Analyze => try_analyze(files),
        Mode::Typecheck => try_typecheck(files),
        Mode::Instantiate => try_instantiate(files),
        Mode::Translate => try_translate(files, llvm_ir, &output_file),
    }
    .unwrap_or_else(|e| report_err(e));

    println!("Okay!");
}

fn get_files(mode: Mode, matches: &[String]) -> PResult<Vec<FileId>> {
    if matches.is_empty() {
        if mode == Mode::Help {
            help(false);
        } else {
            println!("Please provide a filename!");
            help(true);
        }
    } else {
        let (paths, stdin): (Vec<_>, Vec<_>) = matches.iter().partition(|s| *s != "-");
        let mut files = Vec::new();

        for p in paths {
            let p = Path::new(p);
            let f = FileRegistry::seek_path(p)?;
            files.extend(f);
        }

        if !stdin.is_empty() {
            files.push(read_stdin());
        }

        Ok(files)
    }
}

fn select_mode(matches: &Matches) -> Result<Mode, ()> {
    let mut chosen_mode = Option::None;

    let options = [
        ("l", Mode::Lex),
        ("p", Mode::Parse),
        ("a", Mode::Analyze),
        ("t", Mode::Typecheck),
        ("i", Mode::Instantiate),
        ("c", Mode::Translate),
    ];

    for (flag, mode) in &options {
        if matches.opt_present(flag) {
            if chosen_mode.is_some() {
                return Err(());
            }

            chosen_mode = Some(mode.clone());
        }
    }

    Ok(chosen_mode.unwrap_or(DEFAULT_MODE))
}

fn read_stdin() -> FileId {
    unimplemented!()
}

fn help(fail: bool) -> ! {
    println!(
        "
------------------------------------------------------------

CHESHIRE - A smart compiler.

cheshire (-l | --lex) FILE...
  Tokenize files in order to verify that they are all proper
  Cheshire tokens.


cheshire (-p | --parse) FILE...
  Parse files in order to verify that they are all proper
  Cheshire syntax.


cheshire (-a --analyze) FILE...
  Analyze files, doing some basic semantic analysis to
  verify that certain assumptions about the Cheshire program
  are true. This includes checking argument parity, `Self`
  type usage, etc.


cheshire (-t | --tyck) FILE...
  Type-check a program. This will type-check generic
  functions in their generic state.


cheshire (-i | --instantiate) FILE...
  Instantiate a program's generics. This will type-check
  generic functions in their instantiated state, verifying
  that every fully-typed version of the method is valid.


cheshire (-c | --compile) [--llvm-ir] FILE... [-O OUTPUT]
  Compile files to LLVM IR, which is then linked into a
  file and outputted into OUTPUT. If the `-O` argument is
  not provided, then this defaults to 'cheshire.out' in
  the current directory.

  If `--llvm-ir` is provided, then the program will halt
  before the linking phase and produce a plain LLVM IR
  blob. The default output filename will also be changed
  to `cheshire.out.ll`.

------------------------------------------------------------
    "
    );
    exit(if fail { 1 } else { 0 });
}

fn try_lex(files: Vec<FileId>) -> PResult<()> {
    let lexers = files
        .into_iter()
        .map(Lexer::new)
        .collect::<PResult<Vec<Lexer>>>()?;

    for mut lex in lexers {
        // Consume token stream until EOF.
        while lex.bump_token()?.0 != Token::EOF {}
    }

    Ok(())
}

fn try_parse(files: Vec<FileId>) -> PResult<()> {
    parse_program(files).and(Ok(()))
}

fn try_analyze(files: Vec<FileId>) -> PResult<()> {
    let program = parse_program(files)?;
    analyze(program)?;

    Ok(())
}

fn try_typecheck(files: Vec<FileId>) -> PResult<()> {
    let program = parse_program(files)?;
    let (a, p) = analyze(program)?;

    // Wow!
    typecheck(&a, &p)?;

    Ok(())
}

fn try_instantiate(files: Vec<FileId>) -> PResult<()> {
    let program = parse_program(files)?;
    let (a, p) = analyze(program)?;

    typecheck(&a, &p)?;
    instantiate(a, p)?;

    Ok(())
}

fn try_translate(files: Vec<FileId>, llvm_ir: bool, output_file: &str) -> PResult<()> {
    let program = parse_program(files)?;
    let (a, p) = analyze(program)?;

    typecheck(&a, &p)?;
    let i = instantiate(a, p)?;
    translate(i, llvm_ir, output_file)?;

    Ok(())
}
