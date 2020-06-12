use crate::util::result::PResult;

use std::{borrow::Borrow, collections::HashMap, hash::Hash, iter::Zip};

pub trait ZipExact<S>: IntoIterator
where
    S: IntoIterator,
{
    fn zip_exact(self, other: S, what: &str) -> PResult<Zip<Self::IntoIter, S::IntoIter>>;
}

impl<S, SI, T, TI> ZipExact<T> for S
where
    S: IntoIterator<IntoIter = SI>,
    SI: ExactSizeIterator<Item = S::Item>,
    T: IntoIterator<IntoIter = TI>,
    TI: ExactSizeIterator<Item = T::Item>,
{
    fn zip_exact(self, other: T, what: &str) -> PResult<Zip<S::IntoIter, T::IntoIter>> {
        let s = self.into_iter();
        let t = other.into_iter();

        if s.len() != t.len() {
            perror!(
                "Mismatched {}! LHS has {}, RHS has {}.",
                what,
                s.len(),
                t.len()
            )
        } else {
            Ok(Iterator::zip(s, t))
        }
    }
}

pub trait ZipKeys<S, K: Eq + Hash, V1, V2> {
    fn zip_keys(self, other: S) -> HashMap<K, (V1, V2)>;
}

impl<T, S, K1, K2, V1, V2> ZipKeys<S, K1, V1, V2> for T
where
    T: IntoIterator<Item = (K1, V1)>,
    S: IntoIterator<Item = (K2, V2)>,
    K1: Borrow<K2> + Eq + Hash,
    K2: Eq + Hash,
{
    fn zip_keys(self, other: S) -> HashMap<K1, (V1, V2)> {
        let mut first = hashmap! {};

        for (k, v) in other.into_iter() {
            first.insert(k, v);
        }

        let mut second = hashmap! {};

        for (k, v1) in self.into_iter() {
            if let Some(v2) = first.remove(k.borrow()) {
                second.insert(k, (v1, v2));
            }
        }

        second
    }
}
