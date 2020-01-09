use crate::util::{FileRegistry, Span};
use std::process::exit;

pub type PResult<T> = Result<T, PError>;

pub trait IntoError {
    fn error(error: String) -> Self;
    fn error_at(span: Span, error: String) -> Self;
}

impl<T> IntoError for PResult<T> {
    fn error(error: String) -> PResult<T> {
        Err(PError::new(error))
    }

    fn error_at(span: Span, error: String) -> PResult<T> {
        Err(PError::new_at(span, error))
    }
}

#[derive(Debug)]
/// An error with a location, error string, and possible comments
pub struct PError {
    span: Option<Span>,
    error_string: String,
    comments: Vec<String>,
}

impl PError {
    pub fn new(error: String) -> PError {
        PError {
            span: None,
            error_string: error,
            comments: Vec::new(),
        }
    }

    pub fn new_at(span: Span, error: String) -> PError {
        PError {
            span: Some(span),
            error_string: error,
            comments: Vec::new(),
        }
    }

    pub fn why(&self) -> String {
        self.error_string.clone()
    }
}

pub trait Comment<S> {
    fn add_comment(&mut self, comment: S);
    fn with_comment(self, comment: S) -> Self;
}

impl Comment<String> for PError {
    fn add_comment(&mut self, comment: String) {
        self.comments.push(comment);
    }

    fn with_comment(mut self, comment: String) -> PError {
        self.add_comment(comment);

        self
    }
}

impl<T, S, F> Comment<F> for Result<T, S>
where
    S: Comment<String>,
    F: (FnOnce() -> String),
{
    fn add_comment(&mut self, comment: F) {
        if let Some(e) = self.as_mut().err() {
            e.add_comment(comment());
        }
    }

    fn with_comment(self, comment: F) -> Result<T, S> {
        self.map_err(|e| e.with_comment(comment()))
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
            PResult::error_at(
                span,
                format!("Couldn't find {} with name `{}`", type_of, name),
            )
        }
    }

    fn is_not_expected(self, span: Span, type_of: &str, name: &str) -> PResult<()> {
        if self.is_none() {
            Ok(())
        } else {
            PResult::error_at(span, format!("Duplicate {} with name `{}`", type_of, name))
        }
    }
}

pub fn get_row_col(file_contents: &str, pos: usize) -> (usize, usize) {
    let mut row = 0;
    let mut col = 0;

    for c in file_contents[0..pos].chars() {
        if c == '\n' {
            row += 1;
            col = 0;
        } else {
            col += 1;
        }
    }

    (row, col)
}

pub fn get_line_from_pos(file_contents: &str, pos: usize) -> &str {
    if pos > file_contents.len() {
        return "<EOF>";
    }

    let (line, _) = get_row_col(file_contents, pos);
    file_contents.lines().nth(line).unwrap_or_else(|| "<EOF>")
}

pub fn report_err(err: PError) -> ! {
    let PError {
        span,
        error_string,
        comments,
    } = err;

    if let Some(Span { file, start, end }) = span {
        let file_contents_borrow = FileRegistry::open(file).unwrap();
        let file_contents = file_contents_borrow.as_ref();
        let (start_row, start_col) = get_row_col(file_contents, start);
        let (end_row, end_col) = get_row_col(file_contents, end);

        println!();

        if start_row == end_row {
            println!("Error: {} ... on line {}:", error_string, start_row + 1,);
        } else {
            println!(
                "Error {} ... on lines {}-{}:",
                error_string,
                start_row + 1,
                end_row + 1,
            );
        }

        if start_row == end_row {
            let line_str = get_line_from_pos(file_contents, start);

            println!("| {}", line_str);
            for _ in 0..(start_col + 2) {
                print!(" ");
            }

            for _ in start_col..end_col {
                print!("~")
            }

            println!();
        } else {
            let start_line_str = get_line_from_pos(file_contents, start);
            let end_line_str = get_line_from_pos(file_contents, end);

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
    } else {
        println!("Error: {}", error_string);
    }

    if !comments.is_empty() {
        println!();
        println!("Additional comments: ");

        for comment in comments {
            println!("   * {}", comment);
        }
    }

    exit(1);
}
