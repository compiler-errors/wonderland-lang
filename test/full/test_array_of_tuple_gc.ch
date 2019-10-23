// full pass

fn main() -> Int {
    let x = "hello, ".
    let y = [(x, "world\n")].
    gc().
    print(y[0]:0 + y[0]:1).
    gc().
    0
}