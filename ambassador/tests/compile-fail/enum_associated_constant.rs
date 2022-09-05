extern crate ambassador;

use ambassador::*;

#[delegatable_trait] //~ ERROR evaluation of constant value failed [E0080]
pub trait Legs {
    const NUM_LEGS: usize;
}

pub struct Cat;

impl Legs for Cat {
    const NUM_LEGS: usize = 4;
}

pub struct Bird;

impl Legs for Bird {
    const NUM_LEGS: usize = 2;
}

#[derive(Delegate)]
#[delegate(Legs)]
pub enum Pet {
    Cat(Cat),
    Bird(Bird),
}

fn main() {
    println!("{}", Pet::NUM_LEGS) //~ ERROR erroneous constant used [const_err]
                                  //~^ WARNING this was previously accepted by the compiler but is being phased out
                                  //~^^ Error evaluation of constant value failed [E0080]
}
