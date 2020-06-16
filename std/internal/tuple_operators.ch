
impl<_Ret> Call<()> for || -> _Ret {
  type Return = _Ret.

  fn call(self, args: ()) -> _Ret = {
    instruction "ch_call" (self) -> _Ret
  }.
}

impl<_Ret> Call<()> for fn() -> _Ret {
  type Return = _Ret.

  fn call(self, args: ()) -> _Ret = {
    instruction "ch_call" (self) -> _Ret
  }.
}

impl Into<String> for () {
  fn into(self) -> String = {
    "()"
  }.
}

impl Hash for () {
  fn hash(self) -> Int = {
    let h = 7.

    h
  }.
}

enum Join0 {
  Variant,
}

impl Join for () {
  type Result = ().
  type Joined = Join0.

  fn join(self) -> Join0 =
    Join0!Variant.
}

impl Poll for Join0 {
  type Result = ().

  fn poll(self) -> (PollState<()>, Self) = match self {
    Join0!Variant => {
      let success = true.

      let new_self = Join0!Variant.

      if success {
        (PollState!Complete(()), new_self)
      } else {
        (PollState!Incomplete, new_self)
      }
    },
  }.
}


impl<_Ret, _A> Call<(_A,)> for |_A| -> _Ret {
  type Return = _Ret.

  fn call(self, args: (_A,)) -> _Ret = {
    instruction "ch_call" (self, args:0) -> _Ret
  }.
}

impl<_Ret, _A> Call<(_A,)> for fn(_A) -> _Ret {
  type Return = _Ret.

  fn call(self, args: (_A,)) -> _Ret = {
    instruction "ch_call" (self, args:0) -> _Ret
  }.
}

impl<_A> Into<String> for (_A,) where _A: Into<String> {
  fn into(self) -> String = {
    "(\(self:0),)"
  }.
}

impl<_A> Hash for (_A,) where _A: Hash {
  fn hash(self) -> Int = {
    let h = 7.
        h = 31 * h + self:0:hash().
    h
  }.
}

enum Join1<_A, _PA> {
  Variant(Either<_PA, _A>),
}

impl<_A, _PA> Join for (_PA,) where _PA: Poll<::Result=_A> {
  type Result = (_A,).
  type Joined = Join1<_A, _PA>.

  fn join(self) -> Join1<_A, _PA> =
    Join1!Variant(Either!Left(self:0)).
}

impl<_A, _PA> Poll for Join1<_A, _PA> where _PA: Poll<::Result=_A> {
  type Result = (_A,).

  fn poll(self) -> (PollState<(_A,)>, Self) = match self {
    Join1!Variant(join0) => {
      let success = true.

      let join0 = match join0 {
        Either!Left(left) => match left:poll() {
          (PollState!Complete(right), _) => Either!Right(right),
          (PollState!Incomplete, left) => {
            success = false.
            Either!Left(left)
          },
        },
        Either!Right(right) => Either!Right(right),
      }.

      let new_self = Join1!Variant(join0).

      if success {
        (PollState!Complete((join0:unwrap_right(),)), new_self)
      } else {
        (PollState!Incomplete, new_self)
      }
    },
  }.
}


impl<_Ret, _A, _B> Call<(_A, _B)> for |_A, _B| -> _Ret {
  type Return = _Ret.

  fn call(self, args: (_A, _B)) -> _Ret = {
    instruction "ch_call" (self, args:0, args:1) -> _Ret
  }.
}

impl<_Ret, _A, _B> Call<(_A, _B)> for fn(_A, _B) -> _Ret {
  type Return = _Ret.

  fn call(self, args: (_A, _B)) -> _Ret = {
    instruction "ch_call" (self, args:0, args:1) -> _Ret
  }.
}

impl<_A, _B> Into<String> for (_A, _B) where _A: Into<String>, _B: Into<String> {
  fn into(self) -> String = {
    "(\(self:0), \(self:1))"
  }.
}

impl<_A, _B> Hash for (_A, _B) where _A: Hash, _B: Hash {
  fn hash(self) -> Int = {
    let h = 7.
        h = 31 * h + self:0:hash().
        h = 31 * h + self:1:hash().
    h
  }.
}

enum Join2<_A, _B, _PA, _PB> {
  Variant(Either<_PA, _A>, Either<_PB, _B>),
}

impl<_A, _B, _PA, _PB> Join for (_PA, _PB) where _PA: Poll<::Result=_A>, _PB: Poll<::Result=_B> {
  type Result = (_A, _B).
  type Joined = Join2<_A, _B, _PA, _PB>.

  fn join(self) -> Join2<_A, _B, _PA, _PB> =
    Join2!Variant(Either!Left(self:0), Either!Left(self:1)).
}

impl<_A, _B, _PA, _PB> Poll for Join2<_A, _B, _PA, _PB> where _PA: Poll<::Result=_A>, _PB: Poll<::Result=_B> {
  type Result = (_A, _B).

  fn poll(self) -> (PollState<(_A, _B)>, Self) = match self {
    Join2!Variant(join0, join1) => {
      let success = true.

      let join0 = match join0 {
        Either!Left(left) => match left:poll() {
          (PollState!Complete(right), _) => Either!Right(right),
          (PollState!Incomplete, left) => {
            success = false.
            Either!Left(left)
          },
        },
        Either!Right(right) => Either!Right(right),
      }.


      let join1 = match join1 {
        Either!Left(left) => match left:poll() {
          (PollState!Complete(right), _) => Either!Right(right),
          (PollState!Incomplete, left) => {
            success = false.
            Either!Left(left)
          },
        },
        Either!Right(right) => Either!Right(right),
      }.

      let new_self = Join2!Variant(join0, join1).

      if success {
        (PollState!Complete((join0:unwrap_right(), join1:unwrap_right())), new_self)
      } else {
        (PollState!Incomplete, new_self)
      }
    },
  }.
}


impl<_Ret, _A, _B, _C> Call<(_A, _B, _C)> for |_A, _B, _C| -> _Ret {
  type Return = _Ret.

  fn call(self, args: (_A, _B, _C)) -> _Ret = {
    instruction "ch_call" (self, args:0, args:1, args:2) -> _Ret
  }.
}

impl<_Ret, _A, _B, _C> Call<(_A, _B, _C)> for fn(_A, _B, _C) -> _Ret {
  type Return = _Ret.

  fn call(self, args: (_A, _B, _C)) -> _Ret = {
    instruction "ch_call" (self, args:0, args:1, args:2) -> _Ret
  }.
}

impl<_A, _B, _C> Into<String> for (_A, _B, _C) where _A: Into<String>, _B: Into<String>, _C: Into<String> {
  fn into(self) -> String = {
    "(\(self:0), \(self:1), \(self:2))"
  }.
}

impl<_A, _B, _C> Hash for (_A, _B, _C) where _A: Hash, _B: Hash, _C: Hash {
  fn hash(self) -> Int = {
    let h = 7.
        h = 31 * h + self:0:hash().
        h = 31 * h + self:1:hash().
        h = 31 * h + self:2:hash().
    h
  }.
}

enum Join3<_A, _B, _C, _PA, _PB, _PC> {
  Variant(Either<_PA, _A>, Either<_PB, _B>, Either<_PC, _C>),
}

impl<_A, _B, _C, _PA, _PB, _PC> Join for (_PA, _PB, _PC) where _PA: Poll<::Result=_A>, _PB: Poll<::Result=_B>, _PC: Poll<::Result=_C> {
  type Result = (_A, _B, _C).
  type Joined = Join3<_A, _B, _C, _PA, _PB, _PC>.

  fn join(self) -> Join3<_A, _B, _C, _PA, _PB, _PC> =
    Join3!Variant(Either!Left(self:0), Either!Left(self:1), Either!Left(self:2)).
}

impl<_A, _B, _C, _PA, _PB, _PC> Poll for Join3<_A, _B, _C, _PA, _PB, _PC> where _PA: Poll<::Result=_A>, _PB: Poll<::Result=_B>, _PC: Poll<::Result=_C> {
  type Result = (_A, _B, _C).

  fn poll(self) -> (PollState<(_A, _B, _C)>, Self) = match self {
    Join3!Variant(join0, join1, join2) => {
      let success = true.

      let join0 = match join0 {
        Either!Left(left) => match left:poll() {
          (PollState!Complete(right), _) => Either!Right(right),
          (PollState!Incomplete, left) => {
            success = false.
            Either!Left(left)
          },
        },
        Either!Right(right) => Either!Right(right),
      }.


      let join1 = match join1 {
        Either!Left(left) => match left:poll() {
          (PollState!Complete(right), _) => Either!Right(right),
          (PollState!Incomplete, left) => {
            success = false.
            Either!Left(left)
          },
        },
        Either!Right(right) => Either!Right(right),
      }.


      let join2 = match join2 {
        Either!Left(left) => match left:poll() {
          (PollState!Complete(right), _) => Either!Right(right),
          (PollState!Incomplete, left) => {
            success = false.
            Either!Left(left)
          },
        },
        Either!Right(right) => Either!Right(right),
      }.

      let new_self = Join3!Variant(join0, join1, join2).

      if success {
        (PollState!Complete((join0:unwrap_right(), join1:unwrap_right(), join2:unwrap_right())), new_self)
      } else {
        (PollState!Incomplete, new_self)
      }
    },
  }.
}


impl<_Ret, _A, _B, _C, _D> Call<(_A, _B, _C, _D)> for |_A, _B, _C, _D| -> _Ret {
  type Return = _Ret.

  fn call(self, args: (_A, _B, _C, _D)) -> _Ret = {
    instruction "ch_call" (self, args:0, args:1, args:2, args:3) -> _Ret
  }.
}

impl<_Ret, _A, _B, _C, _D> Call<(_A, _B, _C, _D)> for fn(_A, _B, _C, _D) -> _Ret {
  type Return = _Ret.

  fn call(self, args: (_A, _B, _C, _D)) -> _Ret = {
    instruction "ch_call" (self, args:0, args:1, args:2, args:3) -> _Ret
  }.
}

impl<_A, _B, _C, _D> Into<String> for (_A, _B, _C, _D) where _A: Into<String>, _B: Into<String>, _C: Into<String>, _D: Into<String> {
  fn into(self) -> String = {
    "(\(self:0), \(self:1), \(self:2), \(self:3))"
  }.
}

impl<_A, _B, _C, _D> Hash for (_A, _B, _C, _D) where _A: Hash, _B: Hash, _C: Hash, _D: Hash {
  fn hash(self) -> Int = {
    let h = 7.
        h = 31 * h + self:0:hash().
        h = 31 * h + self:1:hash().
        h = 31 * h + self:2:hash().
        h = 31 * h + self:3:hash().
    h
  }.
}

enum Join4<_A, _B, _C, _D, _PA, _PB, _PC, _PD> {
  Variant(Either<_PA, _A>, Either<_PB, _B>, Either<_PC, _C>, Either<_PD, _D>),
}

impl<_A, _B, _C, _D, _PA, _PB, _PC, _PD> Join for (_PA, _PB, _PC, _PD) where _PA: Poll<::Result=_A>, _PB: Poll<::Result=_B>, _PC: Poll<::Result=_C>, _PD: Poll<::Result=_D> {
  type Result = (_A, _B, _C, _D).
  type Joined = Join4<_A, _B, _C, _D, _PA, _PB, _PC, _PD>.

  fn join(self) -> Join4<_A, _B, _C, _D, _PA, _PB, _PC, _PD> =
    Join4!Variant(Either!Left(self:0), Either!Left(self:1), Either!Left(self:2), Either!Left(self:3)).
}

impl<_A, _B, _C, _D, _PA, _PB, _PC, _PD> Poll for Join4<_A, _B, _C, _D, _PA, _PB, _PC, _PD> where _PA: Poll<::Result=_A>, _PB: Poll<::Result=_B>, _PC: Poll<::Result=_C>, _PD: Poll<::Result=_D> {
  type Result = (_A, _B, _C, _D).

  fn poll(self) -> (PollState<(_A, _B, _C, _D)>, Self) = match self {
    Join4!Variant(join0, join1, join2, join3) => {
      let success = true.

      let join0 = match join0 {
        Either!Left(left) => match left:poll() {
          (PollState!Complete(right), _) => Either!Right(right),
          (PollState!Incomplete, left) => {
            success = false.
            Either!Left(left)
          },
        },
        Either!Right(right) => Either!Right(right),
      }.


      let join1 = match join1 {
        Either!Left(left) => match left:poll() {
          (PollState!Complete(right), _) => Either!Right(right),
          (PollState!Incomplete, left) => {
            success = false.
            Either!Left(left)
          },
        },
        Either!Right(right) => Either!Right(right),
      }.


      let join2 = match join2 {
        Either!Left(left) => match left:poll() {
          (PollState!Complete(right), _) => Either!Right(right),
          (PollState!Incomplete, left) => {
            success = false.
            Either!Left(left)
          },
        },
        Either!Right(right) => Either!Right(right),
      }.


      let join3 = match join3 {
        Either!Left(left) => match left:poll() {
          (PollState!Complete(right), _) => Either!Right(right),
          (PollState!Incomplete, left) => {
            success = false.
            Either!Left(left)
          },
        },
        Either!Right(right) => Either!Right(right),
      }.

      let new_self = Join4!Variant(join0, join1, join2, join3).

      if success {
        (PollState!Complete((join0:unwrap_right(), join1:unwrap_right(), join2:unwrap_right(), join3:unwrap_right())), new_self)
      } else {
        (PollState!Incomplete, new_self)
      }
    },
  }.
}


impl<_Ret, _A, _B, _C, _D, _E> Call<(_A, _B, _C, _D, _E)> for |_A, _B, _C, _D, _E| -> _Ret {
  type Return = _Ret.

  fn call(self, args: (_A, _B, _C, _D, _E)) -> _Ret = {
    instruction "ch_call" (self, args:0, args:1, args:2, args:3, args:4) -> _Ret
  }.
}

impl<_Ret, _A, _B, _C, _D, _E> Call<(_A, _B, _C, _D, _E)> for fn(_A, _B, _C, _D, _E) -> _Ret {
  type Return = _Ret.

  fn call(self, args: (_A, _B, _C, _D, _E)) -> _Ret = {
    instruction "ch_call" (self, args:0, args:1, args:2, args:3, args:4) -> _Ret
  }.
}

impl<_A, _B, _C, _D, _E> Into<String> for (_A, _B, _C, _D, _E) where _A: Into<String>, _B: Into<String>, _C: Into<String>, _D: Into<String>, _E: Into<String> {
  fn into(self) -> String = {
    "(\(self:0), \(self:1), \(self:2), \(self:3), \(self:4))"
  }.
}

impl<_A, _B, _C, _D, _E> Hash for (_A, _B, _C, _D, _E) where _A: Hash, _B: Hash, _C: Hash, _D: Hash, _E: Hash {
  fn hash(self) -> Int = {
    let h = 7.
        h = 31 * h + self:0:hash().
        h = 31 * h + self:1:hash().
        h = 31 * h + self:2:hash().
        h = 31 * h + self:3:hash().
        h = 31 * h + self:4:hash().
    h
  }.
}

enum Join5<_A, _B, _C, _D, _E, _PA, _PB, _PC, _PD, _PE> {
  Variant(Either<_PA, _A>, Either<_PB, _B>, Either<_PC, _C>, Either<_PD, _D>, Either<_PE, _E>),
}

impl<_A, _B, _C, _D, _E, _PA, _PB, _PC, _PD, _PE> Join for (_PA, _PB, _PC, _PD, _PE) where _PA: Poll<::Result=_A>, _PB: Poll<::Result=_B>, _PC: Poll<::Result=_C>, _PD: Poll<::Result=_D>, _PE: Poll<::Result=_E> {
  type Result = (_A, _B, _C, _D, _E).
  type Joined = Join5<_A, _B, _C, _D, _E, _PA, _PB, _PC, _PD, _PE>.

  fn join(self) -> Join5<_A, _B, _C, _D, _E, _PA, _PB, _PC, _PD, _PE> =
    Join5!Variant(Either!Left(self:0), Either!Left(self:1), Either!Left(self:2), Either!Left(self:3), Either!Left(self:4)).
}

impl<_A, _B, _C, _D, _E, _PA, _PB, _PC, _PD, _PE> Poll for Join5<_A, _B, _C, _D, _E, _PA, _PB, _PC, _PD, _PE> where _PA: Poll<::Result=_A>, _PB: Poll<::Result=_B>, _PC: Poll<::Result=_C>, _PD: Poll<::Result=_D>, _PE: Poll<::Result=_E> {
  type Result = (_A, _B, _C, _D, _E).

  fn poll(self) -> (PollState<(_A, _B, _C, _D, _E)>, Self) = match self {
    Join5!Variant(join0, join1, join2, join3, join4) => {
      let success = true.

      let join0 = match join0 {
        Either!Left(left) => match left:poll() {
          (PollState!Complete(right), _) => Either!Right(right),
          (PollState!Incomplete, left) => {
            success = false.
            Either!Left(left)
          },
        },
        Either!Right(right) => Either!Right(right),
      }.


      let join1 = match join1 {
        Either!Left(left) => match left:poll() {
          (PollState!Complete(right), _) => Either!Right(right),
          (PollState!Incomplete, left) => {
            success = false.
            Either!Left(left)
          },
        },
        Either!Right(right) => Either!Right(right),
      }.


      let join2 = match join2 {
        Either!Left(left) => match left:poll() {
          (PollState!Complete(right), _) => Either!Right(right),
          (PollState!Incomplete, left) => {
            success = false.
            Either!Left(left)
          },
        },
        Either!Right(right) => Either!Right(right),
      }.


      let join3 = match join3 {
        Either!Left(left) => match left:poll() {
          (PollState!Complete(right), _) => Either!Right(right),
          (PollState!Incomplete, left) => {
            success = false.
            Either!Left(left)
          },
        },
        Either!Right(right) => Either!Right(right),
      }.


      let join4 = match join4 {
        Either!Left(left) => match left:poll() {
          (PollState!Complete(right), _) => Either!Right(right),
          (PollState!Incomplete, left) => {
            success = false.
            Either!Left(left)
          },
        },
        Either!Right(right) => Either!Right(right),
      }.

      let new_self = Join5!Variant(join0, join1, join2, join3, join4).

      if success {
        (PollState!Complete((join0:unwrap_right(), join1:unwrap_right(), join2:unwrap_right(), join3:unwrap_right(), join4:unwrap_right())), new_self)
      } else {
        (PollState!Incomplete, new_self)
      }
    },
  }.
}


impl<_Ret, _A, _B, _C, _D, _E, _F> Call<(_A, _B, _C, _D, _E, _F)> for |_A, _B, _C, _D, _E, _F| -> _Ret {
  type Return = _Ret.

  fn call(self, args: (_A, _B, _C, _D, _E, _F)) -> _Ret = {
    instruction "ch_call" (self, args:0, args:1, args:2, args:3, args:4, args:5) -> _Ret
  }.
}

impl<_Ret, _A, _B, _C, _D, _E, _F> Call<(_A, _B, _C, _D, _E, _F)> for fn(_A, _B, _C, _D, _E, _F) -> _Ret {
  type Return = _Ret.

  fn call(self, args: (_A, _B, _C, _D, _E, _F)) -> _Ret = {
    instruction "ch_call" (self, args:0, args:1, args:2, args:3, args:4, args:5) -> _Ret
  }.
}

impl<_A, _B, _C, _D, _E, _F> Into<String> for (_A, _B, _C, _D, _E, _F) where _A: Into<String>, _B: Into<String>, _C: Into<String>, _D: Into<String>, _E: Into<String>, _F: Into<String> {
  fn into(self) -> String = {
    "(\(self:0), \(self:1), \(self:2), \(self:3), \(self:4), \(self:5))"
  }.
}

impl<_A, _B, _C, _D, _E, _F> Hash for (_A, _B, _C, _D, _E, _F) where _A: Hash, _B: Hash, _C: Hash, _D: Hash, _E: Hash, _F: Hash {
  fn hash(self) -> Int = {
    let h = 7.
        h = 31 * h + self:0:hash().
        h = 31 * h + self:1:hash().
        h = 31 * h + self:2:hash().
        h = 31 * h + self:3:hash().
        h = 31 * h + self:4:hash().
        h = 31 * h + self:5:hash().
    h
  }.
}

enum Join6<_A, _B, _C, _D, _E, _F, _PA, _PB, _PC, _PD, _PE, _PF> {
  Variant(Either<_PA, _A>, Either<_PB, _B>, Either<_PC, _C>, Either<_PD, _D>, Either<_PE, _E>, Either<_PF, _F>),
}

impl<_A, _B, _C, _D, _E, _F, _PA, _PB, _PC, _PD, _PE, _PF> Join for (_PA, _PB, _PC, _PD, _PE, _PF) where _PA: Poll<::Result=_A>, _PB: Poll<::Result=_B>, _PC: Poll<::Result=_C>, _PD: Poll<::Result=_D>, _PE: Poll<::Result=_E>, _PF: Poll<::Result=_F> {
  type Result = (_A, _B, _C, _D, _E, _F).
  type Joined = Join6<_A, _B, _C, _D, _E, _F, _PA, _PB, _PC, _PD, _PE, _PF>.

  fn join(self) -> Join6<_A, _B, _C, _D, _E, _F, _PA, _PB, _PC, _PD, _PE, _PF> =
    Join6!Variant(Either!Left(self:0), Either!Left(self:1), Either!Left(self:2), Either!Left(self:3), Either!Left(self:4), Either!Left(self:5)).
}

impl<_A, _B, _C, _D, _E, _F, _PA, _PB, _PC, _PD, _PE, _PF> Poll for Join6<_A, _B, _C, _D, _E, _F, _PA, _PB, _PC, _PD, _PE, _PF> where _PA: Poll<::Result=_A>, _PB: Poll<::Result=_B>, _PC: Poll<::Result=_C>, _PD: Poll<::Result=_D>, _PE: Poll<::Result=_E>, _PF: Poll<::Result=_F> {
  type Result = (_A, _B, _C, _D, _E, _F).

  fn poll(self) -> (PollState<(_A, _B, _C, _D, _E, _F)>, Self) = match self {
    Join6!Variant(join0, join1, join2, join3, join4, join5) => {
      let success = true.

      let join0 = match join0 {
        Either!Left(left) => match left:poll() {
          (PollState!Complete(right), _) => Either!Right(right),
          (PollState!Incomplete, left) => {
            success = false.
            Either!Left(left)
          },
        },
        Either!Right(right) => Either!Right(right),
      }.


      let join1 = match join1 {
        Either!Left(left) => match left:poll() {
          (PollState!Complete(right), _) => Either!Right(right),
          (PollState!Incomplete, left) => {
            success = false.
            Either!Left(left)
          },
        },
        Either!Right(right) => Either!Right(right),
      }.


      let join2 = match join2 {
        Either!Left(left) => match left:poll() {
          (PollState!Complete(right), _) => Either!Right(right),
          (PollState!Incomplete, left) => {
            success = false.
            Either!Left(left)
          },
        },
        Either!Right(right) => Either!Right(right),
      }.


      let join3 = match join3 {
        Either!Left(left) => match left:poll() {
          (PollState!Complete(right), _) => Either!Right(right),
          (PollState!Incomplete, left) => {
            success = false.
            Either!Left(left)
          },
        },
        Either!Right(right) => Either!Right(right),
      }.


      let join4 = match join4 {
        Either!Left(left) => match left:poll() {
          (PollState!Complete(right), _) => Either!Right(right),
          (PollState!Incomplete, left) => {
            success = false.
            Either!Left(left)
          },
        },
        Either!Right(right) => Either!Right(right),
      }.


      let join5 = match join5 {
        Either!Left(left) => match left:poll() {
          (PollState!Complete(right), _) => Either!Right(right),
          (PollState!Incomplete, left) => {
            success = false.
            Either!Left(left)
          },
        },
        Either!Right(right) => Either!Right(right),
      }.

      let new_self = Join6!Variant(join0, join1, join2, join3, join4, join5).

      if success {
        (PollState!Complete((join0:unwrap_right(), join1:unwrap_right(), join2:unwrap_right(), join3:unwrap_right(), join4:unwrap_right(), join5:unwrap_right())), new_self)
      } else {
        (PollState!Incomplete, new_self)
      }
    },
  }.
}


impl<_Ret, _A, _B, _C, _D, _E, _F, _G> Call<(_A, _B, _C, _D, _E, _F, _G)> for |_A, _B, _C, _D, _E, _F, _G| -> _Ret {
  type Return = _Ret.

  fn call(self, args: (_A, _B, _C, _D, _E, _F, _G)) -> _Ret = {
    instruction "ch_call" (self, args:0, args:1, args:2, args:3, args:4, args:5, args:6) -> _Ret
  }.
}

impl<_Ret, _A, _B, _C, _D, _E, _F, _G> Call<(_A, _B, _C, _D, _E, _F, _G)> for fn(_A, _B, _C, _D, _E, _F, _G) -> _Ret {
  type Return = _Ret.

  fn call(self, args: (_A, _B, _C, _D, _E, _F, _G)) -> _Ret = {
    instruction "ch_call" (self, args:0, args:1, args:2, args:3, args:4, args:5, args:6) -> _Ret
  }.
}

impl<_A, _B, _C, _D, _E, _F, _G> Into<String> for (_A, _B, _C, _D, _E, _F, _G) where _A: Into<String>, _B: Into<String>, _C: Into<String>, _D: Into<String>, _E: Into<String>, _F: Into<String>, _G: Into<String> {
  fn into(self) -> String = {
    "(\(self:0), \(self:1), \(self:2), \(self:3), \(self:4), \(self:5), \(self:6))"
  }.
}

impl<_A, _B, _C, _D, _E, _F, _G> Hash for (_A, _B, _C, _D, _E, _F, _G) where _A: Hash, _B: Hash, _C: Hash, _D: Hash, _E: Hash, _F: Hash, _G: Hash {
  fn hash(self) -> Int = {
    let h = 7.
        h = 31 * h + self:0:hash().
        h = 31 * h + self:1:hash().
        h = 31 * h + self:2:hash().
        h = 31 * h + self:3:hash().
        h = 31 * h + self:4:hash().
        h = 31 * h + self:5:hash().
        h = 31 * h + self:6:hash().
    h
  }.
}

enum Join7<_A, _B, _C, _D, _E, _F, _G, _PA, _PB, _PC, _PD, _PE, _PF, _PG> {
  Variant(Either<_PA, _A>, Either<_PB, _B>, Either<_PC, _C>, Either<_PD, _D>, Either<_PE, _E>, Either<_PF, _F>, Either<_PG, _G>),
}

impl<_A, _B, _C, _D, _E, _F, _G, _PA, _PB, _PC, _PD, _PE, _PF, _PG> Join for (_PA, _PB, _PC, _PD, _PE, _PF, _PG) where _PA: Poll<::Result=_A>, _PB: Poll<::Result=_B>, _PC: Poll<::Result=_C>, _PD: Poll<::Result=_D>, _PE: Poll<::Result=_E>, _PF: Poll<::Result=_F>, _PG: Poll<::Result=_G> {
  type Result = (_A, _B, _C, _D, _E, _F, _G).
  type Joined = Join7<_A, _B, _C, _D, _E, _F, _G, _PA, _PB, _PC, _PD, _PE, _PF, _PG>.

  fn join(self) -> Join7<_A, _B, _C, _D, _E, _F, _G, _PA, _PB, _PC, _PD, _PE, _PF, _PG> =
    Join7!Variant(Either!Left(self:0), Either!Left(self:1), Either!Left(self:2), Either!Left(self:3), Either!Left(self:4), Either!Left(self:5), Either!Left(self:6)).
}

impl<_A, _B, _C, _D, _E, _F, _G, _PA, _PB, _PC, _PD, _PE, _PF, _PG> Poll for Join7<_A, _B, _C, _D, _E, _F, _G, _PA, _PB, _PC, _PD, _PE, _PF, _PG> where _PA: Poll<::Result=_A>, _PB: Poll<::Result=_B>, _PC: Poll<::Result=_C>, _PD: Poll<::Result=_D>, _PE: Poll<::Result=_E>, _PF: Poll<::Result=_F>, _PG: Poll<::Result=_G> {
  type Result = (_A, _B, _C, _D, _E, _F, _G).

  fn poll(self) -> (PollState<(_A, _B, _C, _D, _E, _F, _G)>, Self) = match self {
    Join7!Variant(join0, join1, join2, join3, join4, join5, join6) => {
      let success = true.

      let join0 = match join0 {
        Either!Left(left) => match left:poll() {
          (PollState!Complete(right), _) => Either!Right(right),
          (PollState!Incomplete, left) => {
            success = false.
            Either!Left(left)
          },
        },
        Either!Right(right) => Either!Right(right),
      }.


      let join1 = match join1 {
        Either!Left(left) => match left:poll() {
          (PollState!Complete(right), _) => Either!Right(right),
          (PollState!Incomplete, left) => {
            success = false.
            Either!Left(left)
          },
        },
        Either!Right(right) => Either!Right(right),
      }.


      let join2 = match join2 {
        Either!Left(left) => match left:poll() {
          (PollState!Complete(right), _) => Either!Right(right),
          (PollState!Incomplete, left) => {
            success = false.
            Either!Left(left)
          },
        },
        Either!Right(right) => Either!Right(right),
      }.


      let join3 = match join3 {
        Either!Left(left) => match left:poll() {
          (PollState!Complete(right), _) => Either!Right(right),
          (PollState!Incomplete, left) => {
            success = false.
            Either!Left(left)
          },
        },
        Either!Right(right) => Either!Right(right),
      }.


      let join4 = match join4 {
        Either!Left(left) => match left:poll() {
          (PollState!Complete(right), _) => Either!Right(right),
          (PollState!Incomplete, left) => {
            success = false.
            Either!Left(left)
          },
        },
        Either!Right(right) => Either!Right(right),
      }.


      let join5 = match join5 {
        Either!Left(left) => match left:poll() {
          (PollState!Complete(right), _) => Either!Right(right),
          (PollState!Incomplete, left) => {
            success = false.
            Either!Left(left)
          },
        },
        Either!Right(right) => Either!Right(right),
      }.


      let join6 = match join6 {
        Either!Left(left) => match left:poll() {
          (PollState!Complete(right), _) => Either!Right(right),
          (PollState!Incomplete, left) => {
            success = false.
            Either!Left(left)
          },
        },
        Either!Right(right) => Either!Right(right),
      }.

      let new_self = Join7!Variant(join0, join1, join2, join3, join4, join5, join6).

      if success {
        (PollState!Complete((join0:unwrap_right(), join1:unwrap_right(), join2:unwrap_right(), join3:unwrap_right(), join4:unwrap_right(), join5:unwrap_right(), join6:unwrap_right())), new_self)
      } else {
        (PollState!Incomplete, new_self)
      }
    },
  }.
}

