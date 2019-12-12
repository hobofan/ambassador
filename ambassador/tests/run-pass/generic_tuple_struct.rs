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
#[delegate(Shout)]
pub struct WrappedAnimal<A: Shout>(A);

pub fn main() {
    let foo_animal = WrappedAnimal(Cat);
    println!("{}", foo_animal.shout("BAR"));
    let bar_animal = WrappedAnimal(Dog);
    println!("{}", bar_animal.shout("BAR"));
}
