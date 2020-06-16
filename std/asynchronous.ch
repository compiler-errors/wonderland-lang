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
