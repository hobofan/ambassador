extern crate ambassador;

use ambassador::*;

#[delegatable_trait]
pub trait Target<T, U> {}

pub struct SubTarget;

impl<T, U> Target<T, U> for SubTarget {}

#[derive(Delegate)]
#[delegate(Target<T, U>, generics = "T, U")]
pub struct SuperTarget(SubTarget);

pub fn main() {}
