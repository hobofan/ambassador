extern crate ambassador;

use ambassador::{delegatable_trait, Delegate};

#[delegatable_trait]
pub trait Shout {
    fn shout(self) -> String;
}

pub struct Cat;

impl Shout for Cat {
    fn shout(self) -> String {
        "meow!".to_owned()
    }
}

pub struct Dog;

impl Shout for Dog {
    fn shout(self) -> String {
        "meow!".to_owned()
    }
}

#[derive(Delegate)]
#[delegate(Shout)]
pub enum Animals {
    Cat(Cat),
    Dog(Dog),
}

pub fn main() {
    let foo_animal = Animals::Cat(Cat);
    println!("{}", foo_animal.shout());
}
