fn main() -> Int {
  test_add_string().
  test_int_into_string().
  test_char_into_string().
  0
}

fn test_add_string() -> Int {
  let whomst = "hello".
  let whatmst = whomst + ", ".
  let wheremst = whatmst + "world! ".

  print(wheremst).
  0
}

fn test_int_into_string() -> Int {
  // print(" 1:").
  // print(1:into()).
  // print(" ").

  // print(" 123:").
  // print(123:into()).
  // print(" ").

  // print(" 9223372036854775807:").
  // print(9223372036854775807:into()).
  // print(" ").

  // print(" 0:").
  // print(0:into()).
  // print(" ").

  // print(" -1:").
  // print((-1):into()).
  // print(" ").

  // print(" -123:").
  // print((-123):into()).
  // print(" ").

  print(" -9223372036854775808:").
  print((-9223372036854775808):into()).
  print(" ").

  0
}

fn test_char_into_string() -> Int {
  print(" a:").
  print('a':into()).
  print(" ").
  0
}
