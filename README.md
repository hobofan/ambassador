# Ambassador - Delegate trait implementations via procedural macros

⚠️ This crate currently requires the **beta** Rust toolchain. The corresponding stable version 1.40 is planned to be released on 2019-12-19.

Delegating the implementation of traits to enum variants or fields of a struct normally requires a lot of boilerplate code. Ambassador is an attempt to eliminate that boilerplate by deriving the delegating trait implementation via procedural macros.

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

Delegating a trait implementation for a normal struct **(only single-field tuples supported for now)**

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


## TODO:

- [ ] attribute for `Delegate` to select delegation target for multi-field structs
- [x] macro to allow for implementation of traits from external crates
- [ ] Support for mole complex traits (generics, associated consts, etc.)
