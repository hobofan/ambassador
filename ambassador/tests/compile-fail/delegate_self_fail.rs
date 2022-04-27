extern crate ambassador;

use ambassador::{delegatable_trait, delegate_remote, Delegate};
use std::borrow::Borrow;
use std::cmp::{Eq, Ord};
use std::collections::{BTreeMap, HashMap};
use std::hash::Hash;

#[delegatable_trait]
pub trait Map {
    type K;
    type V;
}

#[delegatable_trait] //~ ERROR function cannot return without recursing [unconditional_recursion]
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

#[delegate_remote]
#[delegate(Get<X>, target = "self", generics = "X", where = "K: Hash + Eq + Borrow<X>, X: Hash + Eq + ?Sized")] //Forgot S: BuildHasher
struct HashMap<K, V, S>();

#[delegate_remote]
#[delegate(Get<X>, target = "self", generics = "X", where = "K: Ord + Borrow<X>, X: Ord + ?Sized")]
struct BTreeMap<K, V>();

#[derive(Delegate)]
#[delegate(Map)]
#[delegate(Get<X>, generics = "X", where = "X: ?Sized, A: Map, B: Map<K=A::K, V=A::V>")]
pub enum Either<A, B> {
    Left(A),
    Right(B),
}

pub fn main() {
    let my_map: Either<HashMap<&'static str, u32>, BTreeMap<&'static str, u32>> =
        Either::Left([("a", 1)].into());
    println!("{:?}", my_map.get("a"));
}
