pub struct Counter {
    i: usize,
}

impl Counter {
    pub fn new(start: usize) -> Counter {
        Counter { i: start }
    }

    pub fn next(&mut self) -> usize {
        self.i += 1;

        self.i - 1
    }

    pub fn current(&self) -> usize {
        self.i
    }
}
