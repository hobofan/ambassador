extern crate ambassador;

use ambassador::{delegatable_trait, Delegate};

#[delegatable_trait] //~ ERROR custom attribute panicked
pub trait Cry {
    fn shout() -> String;
}

pub fn main() {}
