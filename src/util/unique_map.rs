use crate::util::Counter;
use std::collections::HashMap;
use std::hash::Hash;

type I = usize;

pub struct UniqueMap<T: Sized + Clone + Eq + Hash> {
    ids: Counter,
    map_to: HashMap<T, I>,
    map_from: HashMap<I, T>,
}

impl<T: Sized + Clone + Eq + Hash> UniqueMap<T> {
    fn id_or_add(&mut self, obj: &T) -> I {
        if self.map_to.contains_key(obj) {
            self.map_to[obj]
        } else {
            let new_key = self.ids.next();
            self.map_to.insert(obj.clone(), new_key);
            self.map_from.insert(new_key, obj.clone());
            new_key
        }
    }

    fn id(&mut self, obj: &T) -> Option<I> {
        self.map_to.get(obj).map(Clone::clone)
    }

    fn get(&mut self, id: I) -> Option<T> {
        self.map_from.get(&id).map(Clone::clone)
    }
}
