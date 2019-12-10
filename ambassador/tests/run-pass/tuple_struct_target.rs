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

impl Shout for Dog {
    fn shout(&self, input: &str) -> String {
        format!("{} - wuff!", input)
    }
}

#[derive(Delegate)]
#[delegate(Shout, target = "1")]
pub struct WrappedAnimals(Cat, Dog);

pub fn main() {
    let foo_animal = WrappedAnimals(Cat, Dog);
    println!("{}", foo_animal.shout("BAR"));
}
