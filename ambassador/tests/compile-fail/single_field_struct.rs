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

#[derive(Delegate)]
#[delegate(Shout, target = "foo")] //~ Error Unknown field specified as "target" value in #[delegate] attribute
pub struct WrappedAnimals(Cat);

pub fn main() {}
