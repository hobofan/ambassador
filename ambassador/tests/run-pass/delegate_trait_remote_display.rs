extern crate ambassador;

use ambassador::{delegatable_trait_remote, Delegate};
use std::fmt::Display;

pub struct Cat;

impl std::fmt::Display for Cat {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        write!(f, "Foo")
    }
}

#[delegatable_trait_remote]
trait Display {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> Result<(), ::std::fmt::Error>;
}

#[derive(Delegate)]
#[delegate(Display)]
pub struct WrappedCat(Cat);

pub fn main() {
    let foo_animal = WrappedCat(Cat);
    println!("{}", foo_animal);
}
