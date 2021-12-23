extern crate ambassador;
use ambassador::*;
use std::ops::Index;

#[delegatable_trait_remote]
pub trait Index<Idx: ?Sized> {
    type Output: ?Sized;
    fn index(&self, index: Idx) -> &Self::Output;
}


#[derive(Delegate)]
#[delegate(Index<usize>)]
pub enum TinyVec<E> {
    One([E; 1]),
    Many(Vec<E>),
}

fn main() {
    let tv1 = TinyVec::One([5]);
    let tv2 = TinyVec::Many(vec![4,7,8]);
    assert_eq!(tv1[0], 5);
    assert_eq!(tv2[1], 7);
}