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

#[derive(Delegate)] //~ ERROR proc-macro derive panicked
#[delegate(Shout, whoops = )]
pub struct Boxed(Cat);

pub fn main() { }
