#![feature(generic_associated_types)]
extern crate ambassador;

use ambassador::{delegatable_trait, Delegate};
use std::any::type_name;

#[delegatable_trait]
pub trait Animal {
    type Baby<T>;
}

pub struct Kitten;
pub struct Cat;
pub struct Puppy;
pub struct Dog;

impl Animal for Puppy {
    type Baby<T> = Puppy;
}

impl Animal for Dog {
    type Baby<T> = Puppy;
}

impl Animal for Kitten {
    type Baby<T> = Kitten;
}

impl Animal for Cat {
    type Baby<T> = Kitten;
}


#[derive(Delegate)]
#[delegate(Animal)]
pub enum Either<A, B> {
    A(A),
    B(B),
}

pub fn main() {
    println!("{:?}", type_name::<<Either<Cat, Kitten> as Animal>::Baby<()>>());
}
