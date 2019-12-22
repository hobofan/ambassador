# Ambassador - Delegate trait implementations via procedural macros

Delegating the implementation of traits to enum variants or fields of a struct normally requires a lot of boilerplate code. Ambassador is an attempt to eliminate that boilerplate by deriving the delegating trait implementation via procedural macros.

**The minimum supported Rust version is 1.40.0.**

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

