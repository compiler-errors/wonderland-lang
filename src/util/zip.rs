use crate::util::result::{IntoError, PResult};

use std::iter::Zip;

pub trait ZipExact<S>: IntoIterator
where
    S: IntoIterator,
{
    fn zip_exact(self, other: S, what: &str) -> PResult<Zip<Self::IntoIter, S::IntoIter>>;
}

impl<
        S: IntoIterator<IntoIter = SI>,
        SI: ExactSizeIterator<Item = S::Item>,
        T: IntoIterator<IntoIter = TI>,
        TI: ExactSizeIterator<Item = T::Item>,
    > ZipExact<T> for S
{
    fn zip_exact(self, other: T, what: &str) -> PResult<Zip<S::IntoIter, T::IntoIter>> {
        let s = self.into_iter();
        let t = other.into_iter();

        if s.len() != t.len() {
            PResult::error(format!(
                "Mismatched {}! LHS has {}, RHS has {}.",
                what,
                s.len(),
                t.len()
            ))
        } else {
            Ok(Iterator::zip(s, t))
        }
    }
}
