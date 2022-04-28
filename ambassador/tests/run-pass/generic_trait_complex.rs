extern crate ambassador;

use ambassador::{delegatable_trait, Delegate};
use std::fmt::Display;

#[delegatable_trait]
pub trait Shout<T> {
    fn shout(&self, input: T) -> String;
}

pub struct Cat;

impl<A: Display, B: Display> Shout<(A, B)> for Cat {
    fn shout(&self, (a, b): (A, B)) -> String {
        format!("{} - meow, {} - purr...", a, b)
    }
}

#[derive(Delegate)]
#[delegate(Shout<(Y, &'a str)>, generics = "'a, Y", target="0")]
pub struct WrappedCat(Cat, ());

fn main() {
    let c = WrappedCat(Cat, ());
    println!("{}", c.shout((5, "bar")))
}
