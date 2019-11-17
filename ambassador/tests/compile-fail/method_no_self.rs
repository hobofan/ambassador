extern crate ambassador;

use ambassador::{delegatable_trait, Delegate};

#[delegatable_trait] //~ ERROR expected value, found module `self`
pub trait Cry {
    fn shout() -> String;
    //~^ ERROR no method named `shout` found for type `Cat` in the current scope
    //~^^ ERROR no method named `shout` found for type `Dog` in the current scope
}

pub struct Cat;

impl Cry for Cat {
    fn shout() -> String {
        "meow!".to_owned()
    }
}

pub struct Dog;

impl Cry for Dog {
    fn shout() -> String {
        "wuff!".to_owned()
    }
}

#[derive(Delegate)]
#[delegate(Cry)]
pub enum Animals {
    Cat(Cat),
    Dog(Dog),
}

pub fn main() {
    let cat = Cat;
    println!("{}", <Cat as Cry>::shout());

    let foo_animal = Animals::Cat(Cat);
    println!("{}", <Animals as Cry>::shout());
}
