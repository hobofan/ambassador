extern crate ambassador;

use ambassador::*;
use std::collections::{BTreeMap, HashMap};
use std::ops::Index;

#[delegatable_trait_remote]
pub trait Index<Idx: ?Sized> {
    type Output: ?Sized;
    fn index(&self, index: Idx) -> &Self::Output;
}

#[derive(Delegate)]
#[delegate(Index<X>, generics = "X", where = "X: Copy")]
pub enum SomeMap<K, V> {
    Hash(HashMap<K, V>),
    BTree(BTreeMap<K, V>),
}

fn main() {
    let m1 = SomeMap::Hash(HashMap::from([("dog".to_string(), "wuff")]));
    let m2 = SomeMap::BTree(BTreeMap::from([("cat".to_string(), "meow")]));
    assert_eq!(m1["dog"], "wuff");
    assert_eq!(m2["cat"], "meow");
}
