use crate::util::FileId;
use std::cmp::{max, min};

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct Span {
    pub file: FileId,
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(file: FileId, start: usize, end: usize) -> Span {
        Span { file, start, end }
    }

    pub fn unite(self, next: Span) -> Span {
        assert_eq!(self.file, next.file);

        Span {
            file: self.file,
            start: min(self.start, next.start),
            end: max(self.end, next.end),
        }
    }
}
