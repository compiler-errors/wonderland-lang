fn side_effecc(expected: Bool) -> Bool {
    if expected {
        println("All right...").
    } else {
        panic:<()>("Oh no!").
    }

    expected
}

fn main() -> Int {
    true  |? side_effecc(false).
    false |? side_effecc(true).
    true  &? side_effecc(true).
    false &? side_effecc(false).
    
    0
}