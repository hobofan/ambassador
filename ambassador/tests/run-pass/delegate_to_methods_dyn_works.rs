// Ensure that deletage_to_methods can be used as alternative to inhibit_automatic_where_clause for auto-deriving traits for objects.

extern crate ambassador;

use ambassador::{delegatable_trait, delegate_to_methods};

#[delegatable_trait]
pub trait Shout {
    fn shout(&self, input: &str) -> String;
}

pub struct Cat;

impl Shout for Cat {
    fn shout(&self, input: &str) -> String {
        format!("{} - meow!", input)
    }
}

pub struct Dog;

impl Shout for Dog {
    fn shout(&self, input: &str) -> String {
        format!("{} - wuff!", input)
    }
}

pub struct WrappedAnimals(pub Box<dyn Shout>);


#[delegate_to_methods]
#[delegate(Shout, target_ref="deref")]
impl WrappedAnimals {
    // Note that ` &dyn Shout` itself does not implement `Shout`,
    // but for this particular trait all `Shout`'s methods can still be called.
    // If Ambassador were to insert `where &dyn Shout : Shout` clause based on method return value,
    // that whould stop working, breaking backwards compatiblity.
    //
    // Such where clause can be generated, but then `inhibit_automatic_where_clause` needs to
    // be implemented for `delegate_to_methods` as well.  
    fn deref(&self) -> &dyn Shout {
        &*self.0
    }
}

fn use_it<T: Shout> (shouter: T) {
    println!("{}", shouter.shout("BAR"));
}

pub fn main() {
    let foo_animal = WrappedAnimals(Box::new(Cat));
    use_it(foo_animal);
}
