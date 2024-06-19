extern crate ambassador;

use ambassador::{delegatable_trait, Delegate};

#[delegatable_trait]
pub trait Trait {
    #[cfg(all())]
    fn method(&self);

    #[cfg(any())]
    fn extra_method(&self);
}

pub struct Cat;

impl Trait for Cat {
    fn method(&self) {
        println!("meow");
    }
}

#[derive(Delegate)]
#[delegate(Trait)]
pub struct WrappedCat {
    inner: Cat,
}

pub fn main() {
    let foo_animal = WrappedCat { inner: Cat };
    foo_animal.method()
}
