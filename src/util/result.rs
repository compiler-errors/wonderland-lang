use crate::util::Span;
use std::process::exit;

macro_rules! perror {
    ($($arg:tt)*) => (crate::util::PError::new_error(format!($($arg)*)))
}

macro_rules! perror_at {
    ($span:expr, $($arg:tt)*) => (crate::util::PError::new_error_at($span, format!($($arg)*)))
}

#[derive(Debug)]
struct SpanString {
    span: Option<Span>,
    string: String,
}

#[derive(Debug)]
/// An error with a location, error string, and possible comments
pub struct PError {
    main_message: SpanString,
    comments: Vec<String>,

    related_messages: Vec<SpanString>,
}

impl PError {
    pub fn new(error: String) -> PError {
        PError {
            main_message: SpanString {
                span: None,
                string: error,
            },
            comments: Vec::new(),
            related_messages: Vec::new(),
        }
    }

    pub fn new_at(span: Span, error: String) -> PError {
        PError {
            main_message: SpanString {
                span: Some(span),
                string: error,
            },
            comments: Vec::new(),
            related_messages: Vec::new(),
        }
    }

    pub fn new_error<T>(error: String) -> PResult<T> {
        Err(PError::new(error))
    }

    pub fn new_error_at<T>(span: Span, error: String) -> PResult<T> {
        Err(PError::new_at(span, error))
    }

    pub fn why(&self) -> &str {
        &self.main_message.string
    }
}

pub type PResult<T> = Result<T, PError>;

pub trait Context {
    fn add_context(&mut self, span: Span);

    fn with_context(self, span: Span) -> Self;

    fn add_comment<F>(&mut self, comment: F)
    where
        F: Fn() -> String;

    fn with_comment<F>(self, comment: F) -> Self
    where
        F: Fn() -> String;

    fn add_context_comment<F>(&mut self, span: Span, comment: F)
    where
        F: Fn() -> String;

    fn with_context_comment<F>(self, span: Span, comment: F) -> Self
    where
        F: Fn() -> String;
}

impl<T> Context for PResult<T> {
    fn add_context(&mut self, span: Span) {
        if let Err(e) = self {
            if e.main_message.span.is_none() {
                e.main_message.span = Some(span);
            }
        }
    }

    fn with_context(mut self, span: Span) -> Self {
        self.add_context(span);

        self
    }

    fn add_comment<F>(&mut self, comment: F)
    where
        F: Fn() -> String,
    {
        if let Err(e) = self {
            e.comments.push(comment());
        }
    }

    fn with_comment<F>(mut self, comment: F) -> Self
    where
        F: Fn() -> String,
    {
        self.add_comment(comment);

        self
    }

    fn add_context_comment<F>(&mut self, span: Span, comment: F)
    where
        F: Fn() -> String,
    {
        self.add_context(span);
        self.add_comment(comment);
    }

    fn with_context_comment<F>(mut self, span: Span, comment: F) -> PResult<T>
    where
        F: Fn() -> String,
    {
        self.add_context_comment(span, comment);

        self
    }
}

pub trait Expect<T> {
    fn as_expected(self, span: Span, type_of: &str, name: &str) -> PResult<T>;
    fn as_not_expected(self, span: Span, type_of: &str, name: &str) -> PResult<()>;
}

impl<T> Expect<T> for Option<T> {
    fn as_expected(self, span: Span, type_of: &str, name: &str) -> PResult<T> {
        if let Some(t) = self {
            Ok(t)
        } else {
            perror_at!(span, "Couldn't find {} with name `{}`", type_of, name)
        }
    }

    fn as_not_expected(self, span: Span, type_of: &str, name: &str) -> PResult<()> {
        if self.is_none() {
            Ok(())
        } else {
            perror_at!(span, "Duplicate {} with name `{}`", type_of, name)
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
    /*

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

    */

    exit(1);
}
