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
#[delegate(Shout,inhibit_automatic_where_clause="true")]
pub struct WrappedAnimals(pub Box<dyn Shout>);


fn use_it<T: Shout> (shouter: T) {
    println!("{}", shouter.shout("BAR"));
}

pub fn main() {
    let foo_animal = WrappedAnimals(Box::new(Cat));
    use_it(foo_animal);
}
