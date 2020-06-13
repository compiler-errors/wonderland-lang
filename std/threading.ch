object Thread {
  id: Int,
  completed: Option<Dyn>,
}

fn call_trampoline<_F, _T>(f: _F) -> _T where _F: Fn() -> _T = {
  f()
}.

impl for Thread {
  fn spawn<_F, _T>(f: _F) -> Thread where _F: Fn() -> _T = {
    let c = call_trampoline:<_F, _T>.
    instruction "ch_thread_spawn" (c, f, _: _T) -> Thread
  }.

  // Returns the currently running thread.
  fn current() -> Thread = {
    instruction "ch_thread_current" () -> Thread
  }.

  // Wait for a thread to complete, return the value yielded by the thread.
  fn join(self) -> Dyn = {
    if self:id == Thread:current():id {
      panic:<()>("Cannot Thread:join() on the running thread!").
    }

    while true {
      if let Option!Some(value) = self:completed {
        break value.
      } else {
        // TODO: I: I should actually block on this thread.
        instruction "ch_thread_block" (self:id) -> ().
      }
    } else {
      unreachable()
    }
  }.

  // Convenience function. Same as Thread:join, but explicitly downcast.
  // Panics if the joined value isn't the specified concrete type.
  fn join_as<_T>(self) -> _T = {
    self:join():downcast()
  }.

  // Waits until all threads are finished. This is only allowed to be executed
  // on the main thread, will panic otherwise.
  fn coalesce() = {
    if Thread:current():id != 0 {
      panic:<()>("Can only call Thread:coalesce on the main thread").
    }

    while (instruction "ch_thread_count" () -> Int) > 1 {
      instruction "ch_thread_coalesce" () -> ().
    }
  }.

  fn yield() = instruction "ch_thread_yield" () -> ().
}