const EOF: char = '\x00';

#[derive(Debug)]
pub struct FileReader<'a> {
    file: &'a str,
    pos: usize,
    current: char,
    next: char,
    reached_end: bool,
}

impl<'a> FileReader<'a> {
    pub fn new(file_contents: &str) -> FileReader {
        let mut fr = FileReader {
            file: file_contents,
            pos: 0,
            current: EOF,
            next: EOF,
            reached_end: false,
        };

        // TODO: this is ugly...
        fr.next = fr.char_at(0);
        fr.bump(1);

        fr
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

    pub fn get_line_from_pos(&self, pos: usize) -> &str {
        if pos > self.file.len() {
            return "<EOF>";
        }

        let (line, _) = self.get_row_col(pos);
        self.file.lines().nth(line).unwrap()
    }

    fn bump_once(&mut self) {
        self.current = self.next;
        self.next = if !self.reached_end {
            self.pos += self.next.len_utf8();
            if self.pos < self.file.len() {
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
        self.file[n..].chars().next().unwrap_or(EOF)
    }

    pub fn get_row_col(&self, pos: usize) -> (usize, usize) {
        let mut row = 0;
        let mut col = 0;

        for c in self.file[0..pos].chars() {
            if c == '\n' {
                row += 1;
                col = 0;
            } else {
                col += 1;
            }
        }

        (row, col)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    const EOF: char = '\x00';

    #[test]
    fn test_file_reader() {
        let mut fr = FileReader::new("ABCDEFG");

        assert_eq!(fr.current_char(), 'A');
        assert_eq!(fr.next_char(), 'B');

        fr.bump(1);
        assert_eq!(fr.current_char(), 'B');
        assert_eq!(fr.next_char(), 'C');

        fr.bump(5);
        assert_eq!(fr.current_char(), 'G');
        assert_eq!(fr.next_char(), EOF);

        fr.bump(1);
        assert_eq!(fr.current_char(), EOF);
    }

    #[test]
    fn test_unicode_file_reader() {
        let mut fr = FileReader::new("ÃBあ EFË");

        assert_eq!(fr.current_char(), 'Ã');
        assert_eq!(fr.next_char(), 'B');

        fr.bump(1);
        assert_eq!(fr.current_char(), 'B');
        assert_eq!(fr.next_char(), 'あ');

        fr.bump(1);
        assert_eq!(fr.current_char(), 'あ');
        assert_eq!(fr.next_char(), ' ');

        fr.bump(4);
        assert_eq!(fr.current_char(), 'Ë');
        assert_eq!(fr.next_char(), EOF);

        fr.bump(1);
        assert_eq!(fr.current_char(), EOF);
        assert_eq!(fr.next_char(), EOF);
    }

    #[test]
    fn test_short() {
        let mut fr2 = FileReader::new("あI");

        assert_eq!(fr2.current_char(), 'あ');
        assert_eq!(fr2.next_char(), 'I');

        fr2.bump(1);
        assert_eq!(fr2.current_char(), 'I');
        assert_eq!(fr2.next_char(), EOF);

        fr2.bump(1);
        assert_eq!(fr2.current_char(), EOF);
        assert_eq!(fr2.next_char(), EOF);

        let mut fr1 = FileReader::new("あ");

        assert_eq!(fr1.current_char(), 'あ');
        assert_eq!(fr1.next_char(), EOF);

        fr1.bump(1);
        assert_eq!(fr1.current_char(), EOF);
        assert_eq!(fr1.next_char(), EOF);
    }

    #[test]
    fn test_pos() {
        let mut fr = FileReader::new("ABC\nDEFG\nHI\n\nJK\n");

        assert_eq!("DEFG", fr.get_line_from_pos(6));
        assert_eq!(fr.get_row_col(7), (1, 3));
        assert_eq!(fr.get_row_col(12), (3, 0));
        assert_eq!("JK", fr.get_line_from_pos(13));
        assert_eq!("JK", fr.get_line_from_pos(14));

        fr = FileReader::new("ABCあ");

        assert_eq!(fr.current_pos(), 0);
        fr.bump(2);
        assert_eq!(fr.current_pos(), 2);
        fr.bump(2);
        assert_eq!(fr.current_pos(), 5); // Because Japanese A is 2-bytes long.
        assert_eq!(fr.current_char(), EOF);
    }
}
