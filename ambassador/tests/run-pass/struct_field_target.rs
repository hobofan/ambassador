extern crate ambassador;

use ambassador::{delegatable_trait, Delegate};

#[delegatable_trait]
pub trait Shout {
    fn shout(&self, input: &str) -> String;
}

pub struct Cat;

impl Shout for Cat {
    fn shout(&self, input: &str) -> String {
        format!("{} - meow!", input)
    }
}

pub struct Dog;

#[derive(Delegate)]
#[delegate(Shout, target = "inner")]
pub struct WrappedCat {
    inner: Cat,
    other: Dog,
}

pub fn main() {
    let foo_animal = WrappedCat {
        inner: Cat,
        other: Dog,
    };
    println!("{}", foo_animal.shout("BAR"));
}
