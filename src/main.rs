#![deny(unused_must_use)]
#![feature(result_cloned)]
#![feature(option_expect_none)]

#[macro_use]
extern crate lazy_static;
extern crate getopts;
#[macro_use]
extern crate log;
#[macro_use]
extern crate adapter;

#[cfg(feature = "ana")]
use crate::ana::analyze;

#[cfg(feature = "inst")]
use crate::inst::instantiate;

#[cfg(feature = "lex")]
use crate::lexer::{Lexer, Token};

#[cfg(feature = "parse")]
use crate::parser::parse_program;

#[cfg(feature = "tr")]
extern crate inkwell;

#[cfg(feature = "tr")]
use crate::tr::translate;

#[cfg(feature = "tyck")]
use crate::tyck::typecheck;

#[cfg(feature = "lg")]
extern crate gc;

#[cfg(feature = "lg")]
extern crate gc_derive;

#[cfg(feature = "lg")]
use crate::lg::evaluate;

use crate::util::{report_err, FileId, FileRegistry, PError, PResult};

use getopts::{Matches, Options};

use crate::lexer::SpanToken;
use log::LevelFilter;
use std::{
    ffi::OsString,
    io::{Read, Write},
    path::Path,
    process::exit,
};
use tempfile::NamedTempFile;

#[macro_use]
mod util;

#[cfg(feature = "ana")]
mod ana;

#[cfg(feature = "inst")]
mod inst;

#[cfg(feature = "lex")]
mod lexer;

#[cfg(feature = "parse")]
mod parser;

#[cfg(feature = "tr")]
mod tr;

#[cfg(feature = "tyck")]
mod tyck;

#[cfg(feature = "lg")]
mod lg;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum Mode {
    Help,
    Lex,
    Parse,
    Analyze,
    Typecheck,
    Instantiate,
    Translate,
    Evaluate,
}

const DEFAULT_MODE: Mode = Mode::Translate;

fn main() {
    env_logger::builder().init();
    let args: Vec<_> = std::env::args().collect();

    let mut opts = Options::new();
    opts.optflag("h", "help", "print this help menu");
    opts.optopt("o", "output", "set output file name", "FILE");

    if cfg!(feature = "lex") {
        opts.optflag("l", "lex", "Tokenize file(s)");
    }
    if cfg!(feature = "parse") {
        opts.optflag("p", "parse", "Parse file(s)");
    }
    if cfg!(feature = "ana") {
        opts.optflag("a", "analyze", "Analyze file(s)");
    }
    if cfg!(feature = "tyck") {
        opts.optflag("t", "tyck", "Typecheck file(s)");
    }
    if cfg!(feature = "inst") {
        opts.optflag("i", "inst", "Instantiate generics and typecheck file(s)");
    }
    if cfg!(feature = "tr") {
        opts.optflag("c", "tr", "Compile, a.k.a. translate, file(s)");
    }
    if cfg!(feature = "lg") {
        opts.optflag("e", "lgvm", "Evaluate with the Looking Glass VM");
    }

    opts.optflag("L", "llvm-ir", "Compile file(s) to LLVM IR");
    opts.optopt("O", "output", "output file", "FILE");
    opts.optopt("S", "tempdir", "temporary file output", "DIR");

    opts.optmulti("I", "include", "Included C files", "CFILE");

    let matches = opts.parse(&args[1..]).unwrap_or_else(|_| {
        error!("Something went wrong when parsing...");
        help(true);
    });

    if matches.opt_present("h") {
        help(true);
    }

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
            "cheshire.out".into()
        }
    });
    let included_files = matches
        .opt_strs("I")
        .into_iter()
        .map(|s| OsString::from(s))
        .collect();
    let permanent_temp_dir = matches.opt_str("S");

    println!("Mode: {:#?}", mode);
    match_mode(
        mode,
        files,
        llvm_ir,
        &output_file,
        included_files,
        permanent_temp_dir.as_deref(),
    )
    .unwrap_or_else(|e| report_err(e));
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
            files.push(read_stdin()?);
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
        ("e", Mode::Evaluate),
    ];

    for (flag, mode) in &options {
        if matches.opt_defined(flag) && matches.opt_present(flag) {
            if chosen_mode.is_some() {
                return Err(());
            }

            chosen_mode = Some(mode.clone());
        }
    }

    Ok(chosen_mode.unwrap_or(DEFAULT_MODE))
}

fn match_mode(
    mode: Mode,
    files: Vec<FileId>,
    _llvm_ir: bool,
    _output_file: &str,
    _included_files: Vec<OsString>,
    _permanent_temp_dir: Option<&str>,
) -> PResult<()> {
    match mode {
        #[cfg(feature = "lex")]
        Mode::Lex => try_lex(files),
        #[cfg(feature = "parse")]
        Mode::Parse => try_parse(files),
        #[cfg(feature = "ana")]
        Mode::Analyze => try_analyze(files),
        #[cfg(feature = "tyck")]
        Mode::Typecheck => try_typecheck(files),
        #[cfg(feature = "inst")]
        Mode::Instantiate => try_instantiate(files),
        #[cfg(feature = "tr")]
        Mode::Translate => try_translate(
            files,
            _llvm_ir,
            &_output_file,
            _included_files,
            _permanent_temp_dir,
        ),
        #[cfg(feature = "lg")]
        Mode::Evaluate => try_evaluate(files),
        m => help(m != Mode::Help),
    }
}

fn read_stdin() -> PResult<FileId> {
    let mut file = NamedTempFile::new()
        .map_err(|e| PError::new(format!("Error creating temporary file: {}", e)))?;
    let mut buf = vec![];

    std::io::stdin()
        .read_to_end(&mut buf)
        .map_err(|e| PError::new(format!("Error reading from stdin: {}", e)))?;
    file.write_all(&buf)
        .map_err(|e| PError::new(format!("Error writing to temporary file: {}", e)))?;

    Ok(FileRegistry::register_temporary(file))
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


cheshire (-c | --compile) [--llvm-ir | -L] FILE... [-S | --tempdir DIR] [-O OUTPUT]
  Compile files to LLVM IR, which is then linked into a
  file and outputted into OUTPUT. If the `-O` argument is
  not provided, then this defaults to 'cheshire.out' in
  the current directory.

  If `--llvm-ir` is provided, then the program will halt
  before the linking phase and produce a plain LLVM IR
  blob. The default output filename will also be changed
  to `cheshire.out.ll`.

  if `-tempdir` is provided, then the program will emit the temporary
  files that it uses during compilation and linking to the given
  directory. It will ensure that this directory exists.

  The `-` filename can be used for either input or output.
  If provided multiple times for input, it will only be
  used once. If used for input, it corresponds to the module
  `stdin`.

------------------------------------------------------------
    "
    );
    exit(if fail { 1 } else { 0 });
}

#[cfg(feature = "lex")]
fn try_lex(files: Vec<FileId>) -> PResult<()> {
    let lexers = files
        .into_iter()
        .map(Lexer::new)
        .collect::<PResult<Vec<Lexer>>>()?;

    for mut lex in lexers {
        loop {
            let SpanToken(t, _) = lex.bump_token()?;
            print!("{} ", t);

            if t == Token::EOF {
                println!();
                break;
            }
        }
    }

    Ok(())
}

#[cfg(feature = "parse")]
fn try_parse(files: Vec<FileId>) -> PResult<()> {
    parse_program(files).and(Ok(()))
}

#[cfg(feature = "ana")]
fn try_analyze(files: Vec<FileId>) -> PResult<()> {
    let program = parse_program(files)?;
    analyze(program)?;

    Ok(())
}

#[cfg(any(feature = "tyck"))]
fn try_typecheck(files: Vec<FileId>) -> PResult<()> {
    let program = parse_program(files)?;
    let (a, p) = analyze(program)?;

    // Wow!
    typecheck(&a, &p)?;

    Ok(())
}

#[cfg(feature = "inst")]
fn try_instantiate(files: Vec<FileId>) -> PResult<()> {
    let program = parse_program(files)?;
    let (a, p) = analyze(program)?;

    typecheck(&a, &p)?;
    instantiate(a, p)?;

    Ok(())
}

#[cfg(feature = "tr")]
fn try_translate(
    files: Vec<FileId>,
    llvm_ir: bool,
    output_file: &str,
    included_files: Vec<OsString>,
    permanent_temp_dir: Option<&str>,
) -> PResult<()> {
    let program = parse_program(files)?;
    let (a, p) = analyze(program)?;

    typecheck(&a, &p)?;
    let i = instantiate(a, p)?;
    translate(i, llvm_ir, output_file, included_files, permanent_temp_dir)?;

    Ok(())
}

#[cfg(feature = "lg")]
fn try_evaluate(files: Vec<FileId>) -> PResult<()> {
    let program = parse_program(files)?;
    let (a, p) = analyze(program)?;

    typecheck(&a, &p)?;
    let i = instantiate(a, p)?;
    evaluate(i)?;

    Ok(())
}
