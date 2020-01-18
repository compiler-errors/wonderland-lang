// inst pass

fn main() -> Int {
    let x: || -> Int = || 5.
    x()
}