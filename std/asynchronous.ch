object Awaitable<_T> {}

trait Poll {
    type Result.

    fn poll(self) -> (PollState<<Self as Poll>::Result>, Self).
}

enum PollState<_T> {
    Complete(_T),
    Incomplete,
}

impl<_T> Poll for Awaitable<_T> {
    type Result = _T.

     fn poll(self) -> (PollState<_T>, Self) = {
        let (poll_state, new_self) = instruction "ch_awaitable_poll" (self) -> (PollState<_T>, Awaitable<_T>).

        // NOTE: self is a heap object (heap awaitable), so
        // we can just return it by reference.
        (poll_state, new_self)
     }.
}

trait Wait where Self: Poll {
    fn wait(self) -> <Self as Poll>::Result.
}

impl<_T> Wait for _T where _T: Poll {
    fn wait(self) -> <Self as Poll>::Result = {
        let pollable = self.

        while true {
            let (state, new_pollable) = pollable:poll().

            match state {
                PollState!Complete(value) => return value,
                PollState!Incomplete => {},
            }

            pollable = new_pollable.
        }

        unreachable()
    }.
}

trait Join {
    type Result.
    type Joined: Poll<::Result = <Self as Join>::Result>.

    fn join(self) -> <Self as Join>::Joined.
}

enum Join2<_PT, _T, _PS, _S> {
    State0(_PT, _PS),
    State1(_T, _PS),
    State2(_PT, _S),
    State3(_T, _S),
}

impl<_PT, _T, _PS, _S> Join for (_PT, _PS) where _PT: Poll<::Result = _T>, _PS: Poll<::Result = _S> {
    type Result = (_T, _S).
    type Joined = Join2<_PT, _T, _PS, _S>.

    fn join(self) -> Join2<_PT, _T, _PS, _S> =
        Join2!State0(self:0, self:1).
}

impl<_PT, _T, _PS, _S> Poll for Join2<_PT, _T, _PS, _S> where _PT: Poll<::Result = _T>, _PS: Poll<::Result = _S> {
    type Result = (_T, _S).

    fn poll(self) -> (PollState<(_T, _S)>, Self) = match self {
        Join2!State0(pt, ps) =>
            match pt:poll() {
                (PollState!Complete(t), _) =>
                    match ps:poll() {
                        (PollState!Complete(s), _) =>
                            (PollState!Complete((t, s)), Join2!State3(t, s)),
                        (PollState!Incomplete, ps) =>
                            (PollState!Incomplete, Join2!State1(t, ps)),
                    },
                (PollState!Incomplete, pt) =>
                    match ps:poll() {
                        (PollState!Complete(s), _) =>
                            (PollState!Incomplete, Join2!State2(pt, s)),
                        (PollState!Incomplete, ps) =>
                            (PollState!Incomplete, Join2!State0(pt, ps)),
                    },
            },
        Join2!State1(t, ps) =>
            match ps:poll() {
                (PollState!Complete(s), _) =>
                    (PollState!Complete((t, s)), Join2!State3(t, s)),
                (PollState!Incomplete, ps) =>
                    (PollState!Incomplete, Join2!State1(t, ps)),
            },
        Join2!State2(pt, s) =>
            match pt:poll() {
                (PollState!Complete(t), _) =>
                    (PollState!Complete((t, s)), Join2!State3(t, s)),
                (PollState!Incomplete, pt) =>
                    (PollState!Incomplete, Join2!State2(pt, s)),
            },
        Join2!State3(t, s) =>
            (PollState!Complete((t, s)), Join2!State3(t, s)),
    }.
}