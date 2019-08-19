use crate::util::result::{PError, PResult};
use crate::util::Span;
use std::iter::Zip;
use std::path::Iter;

pub trait ZipExact<S>: IntoIterator
where
    S: IntoIterator,
{
    fn zip_exact(self, other: S, what: &str) -> PResult<Zip<Self::IntoIter, S::IntoIter>>;
}

impl<'a, T, S> ZipExact<&'a Vec<S>> for &Vec<T> {
    fn zip_exact(
        self,
        other: &'a Vec<S>,
        what: &str,
    ) -> PResult<Zip<Self::IntoIter, <&'a Vec<S> as IntoIterator>::IntoIter>> {
        if self.len() != other.len() {
            PError::new(
                Span::new(0, 0),
                format!(
                    "Mismatched {}! LHS has {}, RHS has {}.",
                    what,
                    self.len(),
                    other.len()
                ),
            )
        } else {
            Ok(Iterator::zip(self.iter(), other.iter()))
        }
    }
}
