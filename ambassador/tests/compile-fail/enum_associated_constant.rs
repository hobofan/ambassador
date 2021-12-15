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

pub struct Bird;

impl Legs for Bird {
    const NUM_LEGS: usize = 2;
}

#[derive(Delegate)]
#[delegate(Legs)] //~ ERROR *
pub enum Pet {
    Cat(Cat),
    Bird(Bird),
}



fn main() {
    println!("{}", Pet::NUM_LEGS)
}