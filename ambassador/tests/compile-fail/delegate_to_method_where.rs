extern crate ambassador;

use ambassador::{delegatable_trait, delegate_to_methods};
use std::ops::DerefMut;

#[delegatable_trait]
trait MyTrait {
    fn get(&self) -> u32;
    fn change(&mut self);
    fn into_u32(self) -> u32;
}

impl MyTrait for u32 {
    fn get(&self) -> u32 {
        *self
    }

    fn change(&mut self) {
        *self += 1
    }

    fn into_u32(self) -> u32 {
        self
    }
}

struct Wrap<X>(X);

#[delegate_to_methods]
#[delegate(
    MyTrait,
    target_owned = "inner",
    target_ref = "inner_ref",
    target_mut = "inner_mut"
)]
impl<X> Wrap<X>
where
    X: DerefMut + Into<X::Target>,
    X::Target: Sized,
{
    fn inner(self) -> X::Target {
        self.0.into()
    }

    fn inner_ref(&self) -> &X::Target {
        self.0.deref()
    }

    fn inner_mut(&mut self) -> &mut X::Target {
        self.0.deref_mut()
    }
}

fn main() {
    let x = Wrap(Box::new(42u32));
    assert_eq!(x.into_u32(), 42) //~ ERROR the method `into_u32` exists for struct `Wrap<Box<u32>>`, but its trait bounds were not satisfied
}
