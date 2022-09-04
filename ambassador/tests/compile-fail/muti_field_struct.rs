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
//~^ Error "target" value on #[delegate] attribute has to be specified for structs with multiple fields
#[delegate(Shout target = "1")] //~ Error expected `,`
#[delegate(Shout, target "1")] //~ Error expected `=`
#[delegate(Shout, not_target = "1")] //~ Error invalid key for a delegate attribute
#[delegate(Shout, target = "0+1")] //~ Error unexpected token
#[delegate(Shout, target = "2")] //~ Error Unknown field specified as "target" value in #[delegate] attribute
#[delegate(Shout, target = "foo")] //~ Error Unknown field specified as "target" value in #[delegate] attribute
pub struct WrappedAnimals(Cat, Dog);

pub fn main() {}
