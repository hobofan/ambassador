extern crate ambassador;

use ambassador::{delegatable_trait, delegate_remote};

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

mod wrapped {
    use super::{Cat};
    pub struct WrappedAnimals<A> {
        pub foo: Cat,
        pub bar: A,
        baz: u32, // private field
    }

    impl<A> WrappedAnimals<A> {
        pub fn new(foo: Cat, bar: A) -> Self {
            WrappedAnimals{foo, bar, baz: 13}
        }
    }
}

use wrapped::*;

#[delegate_remote]
#[delegate(Shout, target = "bar")]
struct WrappedAnimals<A: Shout> {
    foo: Cat,
    bar: A,
}


pub fn main() {
    let foo_animal = WrappedAnimals::new(Cat, Cat);
    println!("{}", foo_animal.shout("BAR"));
    let bar_animal = WrappedAnimals::new(Cat, Dog);
    println!("{}", bar_animal.shout("BAR"));
}
