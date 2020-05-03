use std::{
    borrow::Borrow,
    collections::{HashMap, HashSet},
    fmt::Debug,
    hash::Hash,
};

#[derive(Debug)]
pub struct StackMap<K: Eq + Hash, V: Debug> {
    stack: Vec<HashMap<K, V>>,
}

impl<K: Eq + Hash + Debug + Clone, V: Clone + Debug> StackMap<K, V> {
    pub fn new() -> StackMap<K, V> {
        StackMap { stack: Vec::new() }
    }

    pub fn push(&mut self) {
        self.stack.push(HashMap::new())
    }

    pub fn pop(&mut self) -> HashMap<K, V> {
        self.stack.pop().unwrap()
    }

    pub fn get<Q: ?Sized>(&self, key: &Q) -> Option<V>
    where
        K: Borrow<Q>,
        Q: Hash + Eq,
    {
        for map in self.stack.iter().rev() {
            if map.contains_key(key) {
                return map.get(key).cloned();
            }
        }

        None
    }

    pub fn add(&mut self, key: K, value: V) {
        let map = self.stack.last_mut().unwrap();
        map.insert(key, value);
    }

    pub fn keys(&mut self) -> HashSet<K> {
        let mut keys = HashSet::new();

        for map in self.stack.iter() {
            keys.extend(map.keys().cloned());
        }

        keys
    }
}
