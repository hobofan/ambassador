extern crate ambassador;

use ambassador::{delegatable_trait, Delegate};

#[delegatable_trait]
pub trait Cry {
    fn shout() -> String; //~ method must have a receiver

    fn shout2(self: Box<Self>); //~ method's receiver type is not supported
}

pub fn main() {}
