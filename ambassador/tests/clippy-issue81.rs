//! Ensures that the `clippy::unused_unit` lint doesn't fire when .
//!
//! Only catches the error when run with clippy.
#![deny(clippy::unused_unit)]

#[ambassador::delegatable_trait]
pub trait Shout {
    fn shout(&self, input: &str) -> String;
}

#[ambassador::delegate_to_remote_methods] // lint triggered here
#[delegate(Shout, target_ref = "deref")]
impl<'a, T: Shout> Shout for &'a T {}
