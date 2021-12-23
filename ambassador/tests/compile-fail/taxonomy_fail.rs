extern crate ambassador;

use ambassador::{delegatable_trait, Delegate};
use std::any::{type_name};

#[delegatable_trait]
pub trait Taxonomy<E> {
    type Res;
}

pub struct Cat;
pub struct Dog;
pub struct Alligator;

pub struct Class;
pub struct Mammal;
pub struct Reptile;

pub struct Kingdom;
pub struct Animal;

pub trait Base {}

impl Base for Cat {}
impl Taxonomy<Class> for Cat {
    type Res = Mammal;
}

impl Base for Dog {}
impl Taxonomy<Class> for Dog {
    type Res = Mammal;
}

impl Base for Alligator {}
impl Taxonomy<Class> for Alligator {
    type Res = Reptile;
}

impl Taxonomy<Kingdom> for Mammal {
    type Res = Animal;
}

impl Taxonomy<Kingdom> for Reptile {
    type Res = Animal;
}

impl<E: Base + Taxonomy<Class>> Taxonomy<Kingdom> for E
    where <E as Taxonomy<Class>>::Res : Taxonomy<Kingdom> {
    type Res = <<E as Taxonomy<Class>>::Res as Taxonomy<Kingdom>>::Res;
}


#[derive(Delegate)]
#[delegate(Taxonomy<X>)]
pub enum Either<A, B> {
    A(A),
    B(B),
}

pub fn main() {
    assert_eq!(type_name::<<Either<Cat, Alligator> as Taxonomy<Class>>::Res>(), type_name::<Mammal>());
    //~^ ERROR type mismatch resolving `<Cat as Taxonomy<Class>>::Res == Reptile`
}
