extern crate ambassador;

use ambassador::{delegatable_trait, Delegate};

trait StaticFn {
    fn call() -> bool;
}

struct StaticTrue;
struct StaticFalse;

impl StaticFn for StaticTrue {
    fn call() -> bool {
        true
    }
}

impl StaticFn for StaticFalse {
    fn call() -> bool {
        false
    }
}

#[delegatable_trait]
pub trait StaticCall {
    fn call<C: StaticFn>(&self) -> bool;
}

pub struct BaseCaller;

impl StaticCall for BaseCaller {
    fn call<C: StaticFn>(&self) -> bool {
        C::call()
    }
}

#[derive(Delegate)]
#[delegate(StaticCall)]
pub struct WrappedCaller(BaseCaller);

fn main() {
    let c = WrappedCaller(BaseCaller);
    // Verify that the generic type is correctly passed through without relying on type inference.
    assert!(c.call::<StaticTrue>());
    assert!(!c.call::<StaticFalse>());
}
