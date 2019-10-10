use crate::util::len::Len;
use crate::util::result::{IntoError, PError, PResult};
use crate::util::Span;
use std::iter::Zip;

pub trait ZipExact<S>: IntoIterator
where
    S: IntoIterator,
{
    fn zip_exact(self, other: S, what: &str) -> PResult<Zip<Self::IntoIter, S::IntoIter>>;
}

impl<S: IntoIterator + Len, T: IntoIterator + Len> ZipExact<S> for T {
    fn zip_exact(self, other: S, what: &str) -> PResult<Zip<Self::IntoIter, S::IntoIter>> {
        if self.len() != other.len() {
            PResult::error_at(
                Span::none(),
                format!(
                    "Mismatched {}! LHS has {}, RHS has {}.",
                    what,
                    self.len(),
                    other.len()
                ),
            )
        } else {
            Ok(Iterator::zip(self.into_iter(), other.into_iter()))
        }
    }
}
