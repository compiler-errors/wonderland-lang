// full pass stdout="1/2/20/10/5/1/200\n"
fn main() -> Int {
  let x = 1. // 1
  print(x:into() + "/").
  x = x + 1. // 2
  print(x:into() + "/").
  x = x * 10. // 20
  print(x:into() + "/").
  x = x / 2. // 10
  print(x:into() + "/").
  x = x - 5. // 5
  print(x:into() + "/").
  x = x % 2. // 1
  print(x:into() + "/").
  x = x * 200. // 200
  print(x:into() + "\n").
  0
}
