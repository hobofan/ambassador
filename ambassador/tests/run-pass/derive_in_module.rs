extern crate ambassador;

use ambassador::delegatable_trait;

#[delegatable_trait]
pub trait Shout {
    fn shout(&self, input: &str) -> String;
}

mod bar {
    use super::{ambassador_impl_Shout, Shout};
    use ambassador::Delegate;

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
    pub enum Animals {
        Cat(Cat),
        Dog(Dog),
    }
}

use bar::{Animals, Cat};

pub fn main() {
    let foo_animal = Animals::Cat(Cat);
    println!("{}", foo_animal.shout("BAR"));
}
