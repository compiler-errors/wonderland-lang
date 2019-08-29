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
        println!("; > {}", error);
        Err(PError {
            span,
            error_string: error,
            comments: Vec::new(),
        })
    }
}

impl Comment for PError {
    fn add_comment(&mut self, comment: String) {
        self.comments.push(comment);
    }

    fn with_comment(mut self, comment: String) -> PError {
        self.add_comment(comment);

        self
    }
}

pub trait Comment {
    fn add_comment(&mut self, comment: String);
    fn with_comment(self, comment: String) -> Self;
}

impl<T, S: Comment> Comment for Result<T, S> {
    fn add_comment(&mut self, comment: String) {
        if let Some(e) = self.as_mut().err() {
            e.add_comment(comment);
        }
    }

    fn with_comment(self, comment: String) -> Result<T, S> {
        self.map_err(|e| e.with_comment(comment))
    }
}

pub trait Expect<T> {
    fn is_expected(self, span: Span, type_of: &str, name: &str) -> PResult<T>;
    fn is_not_expected(self, span: Span, type_of: &str, name: &str) -> PResult<()>;
}

impl<T> Expect<T> for Option<T> {
    fn is_expected(self, span: Span, type_of: &str, name: &str) -> PResult<T> {
        if let Some(t) = self {
            Ok(t)
        } else {
            panic!();
            PError::new(
                span,
                format!("Couldn't find {} with name `{}`", type_of, name),
            )
        }
    }

    fn is_not_expected(self, span: Span, type_of: &str, name: &str) -> PResult<()> {
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

    if start_row == end_row {
        println!(
            "Error \"{}\" encountered on line {}:",
            error_string,
            start_row + 1,
        );
    } else {
        println!(
            "Error \"{}\" encountered on lines {}-{}:",
            error_string,
            start_row + 1,
            end_row + 1,
        );
    }

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

        println!();
    } else {
        let start_line_str = fr.get_line_from_pos(start);
        let end_line_str = fr.get_line_from_pos(end);

        println!("| {}", start_line_str);
        for _ in 0..(start_col + 2) {
            print!(" ");
        }
        println!("^-");

        if start_row + 1 != end_row {
            println!("| [...lines omitted...]");
        }

        println!("| {}", end_line_str);
        for _ in 0..end_col {
            print!(" ");
        }
        println!("-^");
    }

    exit(1);
}
