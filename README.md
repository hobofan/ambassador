# Ambassador - Delegate trait implementations via procedural macros

<!-- Crates version -->
<a href="https://crates.io/crates/ambassador">
  <img src="https://img.shields.io/crates/v/ambassador.svg?style=flat-square"
  alt="Crates.io version" />
</a>
<!-- docs.rs docs -->
<a href="https://docs.rs/ambassador">
  <img src="https://img.shields.io/badge/docs-latest-blue.svg?style=flat-square"
    alt="docs.rs docs" />
</a>

<br/>


Delegating the implementation of traits to enum variants or fields of a struct normally requires a lot of boilerplate code. Ambassador is an attempt to eliminate that boilerplate by deriving the delegating trait implementation via procedural macros.

**The minimum supported Rust version is 1.53.0.**

## Installation

```
cargo add ambassador
```

## General Usage

### `#[delegatable_trait]`

First we need to make our trait available for delegation by adding a `#[delegatable_trait]` attribute to it (this also makes your trait delegatable in other crates):

```rust
use ambassador::delegatable_trait;

#[delegatable_trait] // <-------
pub trait Shout {
    fn shout(&self, input: &str) -> String;
}
```

### `#[derive(Delegate)]` & `#[delegate(Trait)]`

Now we can delegate the implementation of our trait to a struct field/enum variants by adding `#[derive(Delegate)]` and its associated attribute `#[delegate(Trait)]` to it:

```rust
use ambassador::Delegate;

pub struct Cat;

impl Shout for Cat {
    fn shout(&self, input: &str) -> String {
        format!("{} - meow!", input)
    }
}

#[derive(Delegate)] // <-------
#[delegate(Shout)] // <-------- Delegate implementation of Shout to struct field
pub struct WrappedCat(Cat);
```

#### `#[delegate(..., target = "foo")]` - `target` key

For structs with multiple fields, the field that should act as delegation target can be specified via the `target` key:

```rust
#[derive(Delegate)]
#[delegate(Shout, target = "foo")] // <-------- Delegate implementation of Shout to struct field .foo
pub struct WrappedCats {
  foo: Cat,
  bar: Cat,
}
```

This also works for tuple structs with multiple fields, by using their index as a target key:

```rust
#[derive(Delegate)]
#[delegate(Shout, target = "1")] // <-------- Delegate implementation of Shout to second field of type Dog
pub struct WrappedAnimals(Cat, Dog);
```

#### `#[delegate(..., target = "self")]` - `target="self"`
Types that implement all the methods of a trait without implementing the trait itself,
can be made to implement that trait by setting `target="self"`.
This doesn't work for traits with associated types and constants, and requires the where clause to be added explicitly (see `where` key).
If the type doesn't actually implement the methods (possibly due to an incomplete `where` clause) this can cause a `[unconditional_recursion]` error.

A possible use case of this is when refactoring some methods of a public type into a trait,
the type still needs to implement the methods outside the trait for semver reasons,
and using this feature reduces the boilderplate of implementing the trait with the same methods.


```rust
#[derive(Delegate)]
#[delegate(Shout, target="self")]
pub struct Cat;

impl Cat {
    fn shout(&self, input: &str) -> String {
        format!("{} - meow!", input)
    }
}
```

#### `#[delegate(..., where = "A: Shout")]` - `where` key

To make a delegation apply only for certain generic bounds, similar to a [native where clause](https://doc.rust-lang.org/stable/rust-by-example/generics/where.html), you can specify a `where` attribute:

A where clause is automatically applied that makes sure the target field implements the trait being delegated
```rust
#[derive(Delegate)]
#[delegate(Shout, where = "A: Debug")] // <---- Delegate implementation of Shout to .foo field if foo field implements Debug
pub struct WrappedFoo<A> {
  foo: A,
}
```


#### `#[delegate(Shout<X>)]` - trait generics

We can also delegate traits with generics.
When doing this all instances of `X` and `'x` followed by arbitrary digits eg. `X0` `X12` `'x3` are treated as maximally generic.
The automatically added where clause ensures they are valid for the inner type being delegated to.
Explict where clauses to further refine these types can be added as normal.
Specific types can be used instead of `X` to only derive for those.

```rust
use ambassador::{delegatable_trait, Delegate};
use std::fmt::Display;

#[delegatable_trait] // <-------
pub trait Shout<T> {
    fn shout(&self, input: T) -> String;
}

pub struct Cat;

impl<T: Display> Shout<T> for Cat {
    fn shout(&self, input: T) -> String {
        format!("{} - meow!", input)
    }
}

#[derive(Delegate)]
#[delegate(Shout<X>, generics = "X")] // <-------- X is fully generic
// The automatic where clause ensures X: Display
// We could also use #[delegate(Shout<& 'a str>, generics = "'a")] to only delegate for &str
pub struct WrappedCat(Cat);
```


### For remote traits: `#[delegatable_trait_remote]`

If you want to make an existing trait that lives outside you crate available for delegation, you can do so by copy-pasting it's signature into your code and using the `#[delegatable_trait_remote]` attribute (see [full code sample](./ambassador/tests/run-pass/delegate_trait_remote_display.rs)):

```rust
use ambassador::delegatable_trait_remote;
use std::fmt::Display;

#[delegatable_trait_remote]
trait Display {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> Result<(), ::std::fmt::Error>;
}

#[derive(Delegate)]
#[delegate(Display)] // <-------- Delegate implementation of Display to struct field
pub struct WrappedCat(Cat);
```

### For remote types `#[delegate_remote]`

If you want to make an existing type that lives outside you crate delegate, you can do so by copy-pasting it's definition into your code and using the `#[delegate_remote]` attribute (see [full code sample](./ambassador/tests/run-pass/delegate_remote.rs)):

If the type is a struct, not all the fields have to be public, only the ones being delegated to.

```rust
mod wrapped {
    use super::*;
    pub struct WrappedAnimals<A> {
        pub foo: Cat,
        pub bar: A,
        baz: u32, // private field
    }
}

use wrapped::*;

#[delegate_remote]
#[delegate(Shout, target = "bar")]
struct WrappedAnimals<A: Shout> {
    foo: Cat,
    bar: A,
    // We don't even have to include baz since we don't delegate to it
}
```

Note: Because of the orphan rule `#[delegatable_trait_remote]` and `#[delegate_remote]` can't be combined

## Usage Examples

In this example we have a trait `Shout` that is implemented for both `Cat` and `Dog`.

### Delegate to enum variants

We now add an `Animal` enum and add a delegated trait implementation for `Shout`,
that calls the respective implementations of the enum variants:

```rust
use ambassador::{delegatable_trait, Delegate};

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

#[derive(Delegate)]
#[delegate(Shout)]
pub enum Animal {
    Cat(Cat),
    Dog(Dog),
}

pub fn main() {
    let foo_animal = Animal::Cat(Cat);
    println!("{}", foo_animal.shout("BAR"));
}
```

### Delegate to tuple struct field

Delegating a trait implementation for a tuple struct **(only single-field tuples supported for now)**, for e.g. a newtype pattern.

```rust
use ambassador::{delegatable_trait, Delegate};

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

#[derive(Delegate)]
#[delegate(Shout)]
pub struct WrappedCat(Cat);

pub fn main() {
    let foo_animal = WrappedCat(Cat);
    println!("{}", foo_animal.shout("BAR"));
}
```

### Delegate to struct field

Delegating a trait implementation for a normal struct

```rust
use ambassador::{delegatable_trait, Delegate};

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

#[derive(Delegate)]
#[delegate(Shout)]
pub struct WrappedCat {
    inner_cat: Cat,
}

pub fn main() {
    let foo_animal = WrappedCat { inner_cat: Cat };
    println!("{}", foo_animal.shout("BAR"));
}
```

#### License

<sup>
Licensed under either of <a href="LICENSE-APACHE">Apache License, Version
2.0</a> or <a href="LICENSE-MIT">MIT license</a> at your option.
</sup>

<br>

<sub>
Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in this crate by you, as defined in the Apache-2.0 license, shall
be dual licensed as above, without any additional terms or conditions.
</sub>

