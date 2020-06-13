fn commalipses_impl<_T>(where_at: String) -> _T = {
  panic("TODO: implement commalipses at \(where_at)")
}.

fn assert_impl(b: Bool, where_at: String) = {
  if !b {
    panic:<()>("Assert failed at \(where_at)").
  }
}.
