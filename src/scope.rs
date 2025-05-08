//! Scope
//!
//! A data structure used to handle multi-layer hash maps

use std::{collections::HashMap, hash::Hash};

pub struct Scope<K, V> {
    layers: Vec<HashMap<K, V>>,
}

impl<K: Hash + Eq, V> Scope<K, V> {
    pub fn with_layers_capacity(capacity: usize) -> Scope<K, V> {
        Scope {
            layers: Vec::with_capacity(capacity),
        }
    }

    pub fn start(&mut self) {
        self.layers.push(HashMap::new());
    }

    pub fn end(&mut self) -> Option<HashMap<K, V>> {
        self.layers.pop()
    }

    pub fn insert(&mut self, k: K, v: V) -> Option<V> {
        self.layers.last_mut().unwrap().insert(k, v)
    }

    pub fn contains_key(&self, k: &K) -> bool {
        for layer in self.layers.iter().rev() {
            if layer.contains_key(k) {
                return true;
            }
        }

        false
    }

    pub fn get(&self, k: &K) -> Option<&V> {
        for layer in self.layers.iter().rev() {
            if let Some(v) = layer.get(k) {
                return Some(v);
            }
        }

        None
    }

    pub fn get_mut(&mut self, k: &K) -> Option<&mut V> {
        for layer in self.layers.iter_mut().rev() {
            if let Some(v) = layer.get_mut(k) {
                return Some(v);
            }
        }

        None
    }

    pub fn values(&self) -> impl Iterator<Item = &V> {
        self.layers.iter().rev().flat_map(|layer| layer.values())
    }

    pub fn remove(&mut self, k: &K) -> Option<V> {
        for layer in self.layers.iter_mut().rev() {
            if let Some(v) = layer.remove(k) {
                return Some(v);
            }
        }

        None
    }
}
