#![deny(late_bound_lifetime_arguments)]
extern crate ambassador;

use ambassador::{delegatable_trait, Delegate};

#[delegatable_trait]
pub trait Shout {
    fn shout<'a>(&'a self) {
        println!("SHOUT!")
    }
}

pub struct A;
impl Shout for A {}

#[derive(Delegate)]
#[delegate(Shout, target = "0")]
pub struct B(A);

fn main() {
    B(A).shout()
}
