use std::borrow::Borrow;
use std::collections::HashMap;
use std::fmt::Debug;
use std::hash::Hash;

#[derive(Debug)]
pub struct StackMap<K: Eq + Hash + Debug, V: Debug> {
    stack: Vec<HashMap<K, V>>,
}

impl<K: Eq + Hash + Debug, V: Clone + Debug> StackMap<K, V> {
    pub fn new() -> StackMap<K, V> {
        StackMap { stack: Vec::new() }
    }

    pub fn reset(&mut self) {
        self.stack = Vec::new();
        self.push();
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

    pub fn get_top<Q: ?Sized + Hash>(&self, key: &Q) -> Option<V>
    where
        K: Borrow<Q>,
        Q: Hash + Eq,
    {
        self.stack.last().unwrap().get(key).cloned()
    }

    pub fn add(&mut self, key: K, value: V) {
        let map = self.stack.last_mut().unwrap();
        map.insert(key, value);
    }
}
