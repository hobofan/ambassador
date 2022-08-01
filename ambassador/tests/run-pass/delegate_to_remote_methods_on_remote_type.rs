extern crate ambassador;

use ambassador::{delegatable_trait, delegate_to_remote_methods};
use std::ops::{Deref, DerefMut};

#[delegatable_trait]
trait MyTrait {
    fn get(&self) -> u32;
    fn change(&mut self);
}

impl MyTrait for u32 {
    fn get(&self) -> u32 { *self }
    fn change(&mut self) { *self += 1 }
}

#[delegate_to_remote_methods]
#[delegate(MyTrait, target_ref = "deref", target_mut = "deref_mut")]
impl<X: ?Sized + MyTrait> Box<X> {
    fn deref(&self) -> &X;
    fn deref_mut(&mut self) -> &mut X;
}

fn main() {
    let mut x = Box::new(42u32);
    assert_eq!(x.get(), 42);
    x.change();
    assert_eq!(x.get(), 43);

    let mut y: Box<dyn MyTrait> = x;
    assert_eq!(y.get(), 43);
    y.change();
    assert_eq!(y.get(), 44);
}
