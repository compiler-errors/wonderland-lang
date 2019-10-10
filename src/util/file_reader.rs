use crate::util::{FileId, FileRegistry, PResult};
use std::sync::Arc;

const EOF: char = '\x00';

#[derive(Debug)]
pub struct FileReader {
    pub file_id: FileId,
    file_contents: Arc<str>,

    pos: usize,

    current: char,
    next: char,

    reached_end: bool,
}

impl FileReader {
    pub fn new(file_id: FileId) -> PResult<FileReader> {
        let file_contents = FileRegistry::open(file_id)?;

        let mut fr = FileReader {
            file_id,
            file_contents,
            pos: 0,
            current: EOF,
            next: EOF,
            reached_end: false,
        };

        // TODO: this is ugly...
        fr.next = fr.char_at(0);
        fr.bump(1);

        Ok(fr)
    }

    pub fn current_char(&self) -> char {
        self.current
    }

    pub fn next_char(&self) -> char {
        self.next
    }

    pub fn bump(&mut self, n: usize) {
        for _ in 0..n {
            self.bump_once();
        }
    }

    pub fn current_pos(&self) -> usize {
        // This is necessary because self.pos is actually
        // the position of self.next... so we can just subtract
        // the preceding character, which is self.current.
        self.pos - self.current.len_utf8()
    }

    fn bump_once(&mut self) {
        self.current = self.next;
        self.next = if !self.reached_end {
            self.pos += self.next.len_utf8();
            if self.pos < self.file_contents.len() {
                self.char_at(self.pos)
            } else {
                self.reached_end = true;
                EOF
            }
        } else {
            EOF
        };
    }

    fn char_at(&self, n: usize) -> char {
        self.file_contents[n..].chars().next().unwrap_or(EOF)
    }
}
