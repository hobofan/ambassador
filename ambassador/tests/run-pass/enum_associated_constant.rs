extern crate ambassador;

use std::collections::{BTreeMap, HashMap};
use ambassador::*;

#[delegatable_trait]
pub trait IntoMany<N> {
    const N: usize;
}


impl IntoMany<u8> for u32 {
    const N: usize = 4;
}

impl IntoMany<u16> for u32 {
    const N: usize = 2;
}

impl IntoMany<u8> for u16 {
    const N: usize = 2;
}

impl IntoMany<u8> for char {
    const N: usize = 4;
}


#[derive(Delegate)]
#[delegate(IntoMany<X>)]
pub enum CharOrU32 {
    Char(char),
    U32(u32),
}


fn main() {
    assert_eq!( <CharOrU32 as IntoMany<u8>>::N, 4);
}