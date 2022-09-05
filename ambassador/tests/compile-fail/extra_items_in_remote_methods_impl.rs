extern crate ambassador;

use ambassador::{delegatable_trait, delegate_to_remote_methods};
use std::ops::DerefMut;

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

#[delegate_to_remote_methods]
#[delegate(MyTrait, target_ref = "deref", target_mut = "deref_mut")]
impl<X: ?Sized + MyTrait> Box<X> {
    fn deref(&self) -> &X;
    fn deref_mut(&mut self) -> &mut X;

    fn unused_signature(&self) -> &X; //~ ERROR This method is not used by any `delegate` attributes; please remove it
    fn bad_signature(&self); //~ ERROR delegated to methods must return

    fn method_with_body(&mut self) -> &mut X {
        //~^ ERROR Only method signatures are allowed here (no blocks!)
        //~^^ ERROR This method is not used by any `delegate` attributes; please remove it
        todo!()
    }

    const ASSOCIATED_CONST: () = ();
    //~^ ERROR Only method signatures are allowed here, everything else is discarded

    type SomeType = u8;
    //~^ ERROR Only method signatures are allowed here, everything else is discarded
}

fn main() {}
