use std::collections::HashMap;
use std::hash::Hash;

pub struct StackMap<K: Eq + Hash, V> {
    stack: Vec<HashMap<K, V>>,
}

impl<K: Eq + Hash, V: Clone> StackMap<K, V> {
    pub fn new() -> StackMap<K, V> {
        StackMap { stack: Vec::new() }
    }

    pub fn push(&mut self) {
        self.stack.push(HashMap::new())
    }

    pub fn pop(&mut self) {
        self.stack.pop().unwrap();
    }

    pub fn get(&self, key: &K) -> Option<V> {
        for map in self.stack.iter().rev() {
            if map.contains_key(key) {
                return map.get(key).map(Clone::clone);
            }
        }

        None
    }

    pub fn get_top(&self, key: &K) -> Option<V> {
        self.stack.last().unwrap().get(key).map(Clone::clone)
    }

    pub fn add(&mut self, key: K, value: V) {
        let mut map = self.stack.last_mut().unwrap();
        map.insert(key, value);
    }
}
