extern crate ambassador;

use ambassador::{delegatable_trait_remote, Delegate};
use std::iter::IntoIterator;

#[delegatable_trait_remote]
pub trait IntoIterator {
    type Item;
    type IntoIter: Iterator<Item = Self::Item>;
    fn into_iter(self) -> Self::IntoIter;
}

#[derive(Delegate)]
#[delegate(IntoIterator)]
pub struct WrappedVec<E>(Vec<E>);

pub fn main() {
    let wrapped = WrappedVec(vec![1,2,3]);
    println!("{:?}", wrapped.into_iter().collect::<Vec<_>>());
}
