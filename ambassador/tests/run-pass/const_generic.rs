extern crate ambassador;

use ambassador::{delegatable_trait, Delegate};

#[delegatable_trait]
trait S<const N: usize> {
    fn handle(&self, x: [u8; N]);
}

struct Base;

impl<const N: usize> S<N> for Base {
    fn handle(&self, x: [u8; N]) {
        println!("{x:?}");
    }
}

const C: usize = 2;

#[derive(Delegate)]
#[delegate(S<1>)]
#[delegate(S<C>)]
#[delegate(S<{C+1}>)]
struct Wrap(Base);

fn main() {
    let w = Wrap(Base);
    w.handle([1]);
    w.handle([1, 2]);
    w.handle([1, 2, 3]);
}
