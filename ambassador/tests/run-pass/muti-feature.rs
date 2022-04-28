extern crate ambassador;

use ambassador::{delegatable_trait, delegate_remote, Delegate};
use std::borrow::Borrow;
use std::cmp::{Eq, Ord};
use std::collections::{BTreeMap, HashMap};
use std::hash::{BuildHasher, Hash};

#[delegatable_trait]
pub trait Map {
    type K;
    type V;
}

#[delegatable_trait]
pub trait Get<Q: ?Sized>: Map {
    fn get(&self, k: &Q) -> Option<&Self::V>;
}

impl<K, V, S> Map for HashMap<K, V, S> {
    type K = K;
    type V = V;
}

impl<K, V> Map for BTreeMap<K, V> {
    type K = K;
    type V = V;
}

// No automatic where clause provided for target = "self"
#[delegate_remote]
#[delegate(Get<X>, generics = "X", target = "self", where = "K: Hash + Eq + Borrow<X>, S: BuildHasher, X: Hash + Eq + ?Sized")]
struct HashMap<K, V, S>();

#[delegate_remote]
#[delegate(Get<X>, generics = "X", target = "self", where = "K: Ord + Borrow<X>, X: Ord + ?Sized")]
struct BTreeMap<K, V>();

#[derive(Delegate)]
#[delegate(Map)]
#[delegate(Get<X>, generics = "X", where = "X: ?Sized, A: Map, B: Map<K=A::K, V=A::V>")] //auto where clause misses required on super trait
pub enum Either<A, B> {
    Left(A),
    Right(B),
}

pub fn main() {
    let my_map: Either<HashMap<&'static str, u32>, BTreeMap<&'static str, u32>> =
        Either::Left([("a", 1)].into());
    println!("{:?}", my_map.get("a"));
}
