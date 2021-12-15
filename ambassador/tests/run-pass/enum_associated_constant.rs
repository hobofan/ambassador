extern crate ambassador;

use std::fmt::Display;
use ambassador::*;

#[delegatable_trait]
pub trait Legs {
    const NUM_LEGS: usize;
}

pub struct Cat;

impl Legs for Cat {
    const NUM_LEGS: usize = 4;
}

pub struct Dog;

impl Legs for Dog {
    const NUM_LEGS: usize = 4;
}

#[derive(Delegate)]
#[delegate(Legs)]
pub enum Pet {
    Cat(Cat),
    Dog(Dog),
}


fn main() {
    println!("{}", Pet::NUM_LEGS)
}