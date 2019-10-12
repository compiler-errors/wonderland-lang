use crate::util::PResult;
use std::collections::HashMap;
use std::hash::Hash;

pub trait Visit<A>: Sized {
    fn visit(self, adapter: &mut A) -> PResult<Self>;
}

impl<T, S: Visit<T>> Visit<T> for Box<S> {
    fn visit(self, adapter: &mut T) -> PResult<Self> {
        (*self).visit(adapter).map(Box::new)
    }
}

impl<T, S: Visit<T>> Visit<T> for Option<S> {
    fn visit(self, adapter: &mut T) -> PResult<Self> {
        Ok(if let Some(s) = self {
            Some(s.visit(adapter)?)
        } else {
            None
        })
    }
}

impl<T, S: Visit<T>> Visit<T> for Vec<S> {
    fn visit(self, adapter: &mut T) -> PResult<Vec<S>> {
        self.into_iter().map(|s| s.visit(adapter)).collect()
    }
}

impl<T, K: Eq + Hash, V: Visit<T>> Visit<T> for HashMap<K, V> {
    fn visit(self, adapter: &mut T) -> PResult<HashMap<K, V>> {
        let mut out = HashMap::new();

        for (k, v) in self.into_iter() {
            out.insert(k, v.visit(adapter)?);
        }

        Ok(out)
    }
}