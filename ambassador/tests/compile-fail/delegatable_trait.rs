extern crate ambassador;

use ambassador::{delegatable_trait, Delegate};

#[delegatable_trait]
pub trait Cry {
    fn shout() -> String; //~ Method in delegatable trait does not have a receiver

    fn shout2(self: Box<Self>); //~ Method's receiver type is not supported
}

pub fn main() {}
