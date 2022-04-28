extern crate ambassador;

use ambassador::Delegate;

use ambassador::delegatable_trait;
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
#[delegate(Shout)]
pub enum Either<A, B> {
    Left(A),
    Right(B),
}


pub fn main() {
    let foo_animal = Either::Left::<Cat, Dog>(Cat);
    println!("{}", foo_animal.shout("BAR"));
    let bar_animal = Either::Right::<Cat, Dog>(Dog);
    println!("{}", bar_animal.shout("BAR"));
}
