extern crate ambassador;

use ambassador::{delegatable_trait, Delegate};

#[delegatable_trait]
pub trait Shout {
    fn shout(&mut self) -> String;
}

pub struct Cat;

impl Shout for Cat {
    fn shout(&mut self) -> String {
        "meow!".to_owned()
    }
}

#[derive(Delegate)]
#[delegate(Shout)]
pub struct WrappedCat(Cat);

#[derive(Delegate)]
#[delegate(Shout)]
pub enum Animals {
    Cat(Cat),
    Wrapped(WrappedCat),
}

pub fn main() {
    let mut foo_animal = Animals::Cat(Cat);
    println!("{}", foo_animal.shout());
}
