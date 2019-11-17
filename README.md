# Ambassador - Delegate trait implementations via procedural macros

⚠️ This crate currently the **beta** Rust toolchain. The corresponding stable version 1.40 is planned to be released on 2019-12-19.

## Installation

```
cargo add ambassador
```

## Usage

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
- [ ] macro to allow for implementation of traits from external crates
- [ ] Support for mole complex traits (generics, associated consts, etc.)
