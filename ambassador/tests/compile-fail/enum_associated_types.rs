extern crate ambassador;

use ambassador::{delegatable_trait, Delegate};
use std::any::type_name;

#[delegatable_trait]
pub trait Animal {
    type Baby;
}

pub struct Kitten;
pub struct Cat;
pub struct Puppy;
pub struct Dog;

impl Animal for Puppy {
    type Baby = Puppy;
}

impl Animal for Dog {
    type Baby = Puppy;
}

impl Animal for Kitten {
    type Baby = Kitten;
}

impl Animal for Cat {
    type Baby = Kitten;
}


#[derive(Delegate)]
#[delegate(Animal)]
pub enum Either<A, B> {
    A(A),
    B(B),
}

#[test]
pub fn main() {
    println!("{:?}", type_name::<<Either<Cat, Dog> as Animal>::Baby>());
}
