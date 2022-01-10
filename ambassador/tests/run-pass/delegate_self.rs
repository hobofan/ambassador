extern crate ambassador;

use ambassador::{delegatable_trait, Delegate};

#[delegatable_trait]
pub trait Shout {
    fn shout(&self, input: &str) -> String;
}

#[derive(Delegate)]
#[delegate(Shout, target="self")]
pub struct Cat;

impl Cat {
    fn shout(&self, input: &str) -> String {
        format!("{} - meow!", input)
    }
}

fn use_shout(x: impl Shout) {
    x.shout("BAR");
}

pub fn main() {
    use_shout(Cat)
}
