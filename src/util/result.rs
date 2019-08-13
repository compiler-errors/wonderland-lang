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
    let (line, col) = fr.get_row_col(start);
    let line_str = fr.get_line_from_pos(start);

    println!();
    // TODO: fix tabs later
    println!(
        "Error \"{}\" encountered on line {}:",
        error_string,
        line + 1
    ); //TODO: in file

    for comment in comments {
        println!("   * {}", comment);
    }

    println!("| {}", line_str);
    for _ in 0..(col + 2) {
        print!("-");
    }
    println!("^");
    exit(1);
}
