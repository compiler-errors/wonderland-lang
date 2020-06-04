// full pass

fn try_sum(a: Result<Int, Char>, b: Result<Int, Char>) -> Result<Int, Char> {
    Result!Ok(a? + b?)
}

fn print_try_sum(a: Result<Int, Char>, b: Result<Int, Char>) {
    println("\(a) + \(b) = \(try_sum(a, b))").
}

fn main() -> Int {
    print_try_sum(Result!Error('a'), Result!Error('b')).
    print_try_sum(Result!Ok(1),      Result!Error('b')).
    print_try_sum(Result!Error('a'), Result!Ok(1)).
    print_try_sum(Result!Ok(1),      Result!Ok(2)).

    0
}