use crate::util::{FileReader, Span};
use std::process::exit;

pub type PResult<T> = Result<T, PError>;

#[derive(Debug)]
/// An error with a location, error string, and possible comments
pub struct PError {
    pub span: Span,
    pub error_string: String,
    pub comments: Vec<String>,
}

impl PError {
    pub fn new<T>(span: Span, error: String) -> PResult<T> {
        Err(PError {
            span,
            error_string: error,
            comments: Vec::new(),
        })
    }

    pub fn comment(&mut self, comment: String) {
        self.comments.push(comment);
    }
}

pub trait Expect<T> {
    fn expected(self, span: Span, type_of: &str, name: &str) -> PResult<T>;
    fn not_expected(self, span: Span, type_of: &str, name: &str) -> PResult<()>;
}

impl<T> Expect<T> for Option<T> {
    fn expected(self, span: Span, type_of: &str, name: &str) -> PResult<T> {
        if let Some(t) = self {
            Ok(t)
        } else {
            PError::new(
                span,
                format!("Couldn't find {} with name `{}`", type_of, name),
            )
        }
    }

    fn not_expected(self, span: Span, type_of: &str, name: &str) -> PResult<()> {
        if self.is_none() {
            Ok(())
        } else {
            PError::new(span, format!("Duplicate {} with name `{}`", type_of, name))
        }
    }
}

pub fn report_err_at(fr: &FileReader, err: PError) -> ! {
    let PError {
        span: Span { start, end },
        error_string,
        comments,
    } = err;

    let (start_row, start_col) = fr.get_row_col(start);
    let (end_row, end_col) = fr.get_row_col(end);

    println!();
    println!(
        "Error \"{}\" encountered on line {}:",
        error_string,
        start_row + 1
    );

    for comment in comments {
        println!("   * {}", comment);
    }

    if start_row == end_row {
        let line_str = fr.get_line_from_pos(start);

        println!("| {}", line_str);
        for _ in 0..(start_col + 2) {
            print!(" ");
        }

        for _ in start_col..end_col {
            print!("~")
        }
    } else {
        let start_line_str = fr.get_line_from_pos(start);
        let end_line_str = fr.get_line_from_pos(end);

        println!("starting: {}", start_line_str);
        for _ in 0..(start_col + 10) {
            print!(" ");
        }
        println!("^--");

        println!("  ending: {}", end_line_str);
        for _ in 0..(end_col + 10 - 2) {
            print!(" ");
        }
        println!("--^");
    }


    exit(1);
}
