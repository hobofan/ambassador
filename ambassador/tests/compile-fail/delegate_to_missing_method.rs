extern crate ambassador;

use ambassador::{delegatable_trait, delegate_to_methods};
use std::ops::DerefMut;

#[delegatable_trait] //~ ERROR target_owned was not specified but was needed
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
#[delegate(MyTrait, target_ref = "inner_ref", target_mut = "inner_mut")]
impl<X> Wrap<X>
where
    X: DerefMut,
{
    fn inner_ref(&self) -> &X::Target {
        self.0.deref()
    }

    fn inner_mut(&mut self) -> &mut X::Target {
        self.0.deref_mut()
    }
}

fn main() {
    let mut x = Wrap(Box::new(42u32));
    assert_eq!(x.get(), 42);
    x.change();
    assert_eq!(x.into_u32(), 43);
}
