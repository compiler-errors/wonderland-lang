pub struct Counter {
    i: u32,
}

impl Counter {
    pub fn new(start: u32) -> Counter {
        Counter { i: start }
    }

    pub fn next(&mut self) -> u32 {
        self.i += 1;

        self.i - 1
    }

    pub fn current(&self) -> u32 {
        self.i
    }
}
