extern crate ambassador;

use ambassador::{delegatable_trait, delegate_to_methods};
use std::ops::DerefMut;

#[delegatable_trait]
trait MyTrait {
    fn get(&self) -> u32;
    fn into_u32(self) -> u32;
}

impl MyTrait for u32 {
    fn get(&self) -> u32 {
        *self
    }

    fn into_u32(self) -> u32 {
        self
    }
}

struct Wrap<X>(X);

#[delegate_to_methods]
#[delegate(MyTrait, target_owned = "inner", target_ref = "inner_ref")]
#[delegate(MyTrait, target_owned = "inner_ref", target_ref = "inner_ref")]
//~^ Error method needs to have a receiver of type "self"
#[delegate(MyTrait, target_owned = "inner", target_ref = "fail1")]
//~^ impl block doesn't have any valid methods with this name
#[delegate(MyTrait)] //~ Error no targets were specified
impl<X> Wrap<X>
where
    X: DerefMut + Into<u32>,
    X::Target: Sized,
{
    fn inner(self) -> u32 {
        //~^ Error Note: other return type defined here
        self.0.into()
    }

    fn inner_ref(&self) -> &X::Target {
        //~^ Error target methods have different return types
        self.0.deref()
    }

    fn inner_mut(&mut self) -> &mut X::Target {
        self.0.deref_mut()
    }

    fn fail1() -> u32 {
        5
    }
}

fn main() {}
