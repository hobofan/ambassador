extern crate ambassador;

use ambassador::{delegatable_trait};

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

pub enum Either<A: Shout, B: Shout> {
    Left(A),
    Right(B),
}

impl < A : Shout, B : Shout > Shout for Either < A, B >
{ ambassador_impl_Shout_body_enum! { Either :: Left, Either :: Right } }


pub struct WrappedAnimals<A: Shout> {
    foo: Cat,
    bar: A,
}

impl < A : Shout > Shout for WrappedAnimals < A >
{ ambassador_impl_Shout_body_single_struct! { bar } }

pub fn main() {
    let foo_animal = Either::Left::<Cat, Dog>(Cat);
    println!("{}", foo_animal.shout("BAR"));
    let bar_animal = Either::Right::<Cat, Dog>(Dog);
    println!("{}", bar_animal.shout("BAR"));
}