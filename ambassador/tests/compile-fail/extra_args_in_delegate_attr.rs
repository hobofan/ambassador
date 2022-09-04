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

#[derive(Delegate)]
#[delegate(Shout, whoops = )] //~ ERROR unexpected end of input, expected string literal
pub struct Boxed(Cat);

pub fn main() {}
