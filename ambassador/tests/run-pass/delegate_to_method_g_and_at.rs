extern crate ambassador;

use ambassador::{delegatable_trait, delegate_to_methods};

#[delegatable_trait]
pub trait FnLike<In> {
    type Out;
    fn apply(&self, input: In) -> Self::Out;
}

impl<In, F: Fn(In) -> R, R> FnLike<In> for F {
    type Out = R;

    fn apply(&self, input: In) -> Self::Out {
        self(input)
    }
}

pub struct Cat;

#[delegate_to_methods]
#[delegate(FnLike<X>, target_ref = "to_fn", generics = "X")]
impl Cat {
    fn to_fn(&self) -> &fn(String) -> String {
        fn ret(x: String) -> String {
            x + "meow"
        }
        &(ret as fn(String) -> String)
    }
}

fn main() {
    assert_eq!(Cat.apply(String::new()), "meow")
}
