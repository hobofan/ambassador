extern crate ambassador;

use ambassador::{delegatable_trait, delegate_to_remote_methods};
use std::ops::{Deref, DerefMut};

#[delegatable_trait]
trait MyTrait {
    fn get(&self) -> u32;
    fn change(&mut self);
}

impl MyTrait for u32 {
    fn get(&self) -> u32 {
        *self
    }

    fn change(&mut self) {
        *self += 1
    }
}

struct Wrap<X>(X);

trait HasAU32 {
    fn get_ref(&self) -> &u32;
    fn get_mut_ref(&mut self) -> &mut u32;
}

impl<X: DerefMut<Target = u32>> HasAU32 for Wrap<X> {
    fn get_ref(&self) -> &u32 { self.0.deref() }
    fn get_mut_ref(&mut self) -> &mut u32 { self.0.deref_mut() }
}

#[delegate_to_remote_methods]
#[delegate(MyTrait, target_ref = "get_ref", target_mut = "get_mut_ref")]
impl<X: DerefMut<Target = u32>> Wrap<X> {
    fn get_ref(&self) -> &u32;
    fn get_mut_ref(&mut self) -> &mut u32;
}

fn main() {
    let mut x = Wrap(Box::new(42u32));
    assert_eq!(x.get(), 42);
    x.change();
    assert_eq!(x.get(), 43);
}
