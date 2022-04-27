extern crate ambassador;

use ambassador::{delegatable_trait, Delegate};

#[delegatable_trait]
trait Ref<'a> {
    type Target;
    fn deref<'b>(&'b self) -> &'a Self::Target;
}

impl<'a, E> Ref<'a> for &'a E {
    type Target = E;

    fn deref<'b>(&'b self) -> &'a Self::Target {
        *self
    }
}

#[derive(Delegate)]
#[delegate(Ref<'x>, generics = "'x")]
struct WrapRef<'a, T>(&'a T);

fn main() {
    let x = 5;
    let y = WrapRef(&x);
    assert_eq!(*y.deref(), 5)
}
