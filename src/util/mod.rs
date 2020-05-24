#[macro_use]
mod result;
pub use self::result::{report_err, Context, Expect, PError, PResult};

mod file_registry;
pub use self::file_registry::{FileId, FileRegistry};

mod stack_map;
pub use self::stack_map::StackMap;

mod span;
pub use self::span::Span;

mod zip;
pub use self::zip::{ZipExact, ZipKeys};

mod visitor;
pub use self::visitor::Visit;

// Ugh, I wish this were exported in the library. This is copypasta from
// the `gc::trace` module.
#[cfg(feature = "lg")]
macro_rules! simple_empty_finalize_trace {
    ($($T:ty),*) => {
        $(
            impl ::gc::Finalize for $T {}
            unsafe impl gc::Trace for $T { gc::unsafe_empty_trace!(); }
        )*
    }
}
