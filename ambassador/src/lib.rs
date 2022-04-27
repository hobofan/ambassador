//!
//! # Ambassador - Delegate trait implementations via procedural macros
//!
//! <!-- Crates version -->
//! <a href="https://crates.io/crates/ambassador">
//!   <img src="https://img.shields.io/crates/v/ambassador.svg?style=flat-square"
//!   alt="Crates.io version" />
//! </a>
//! <!-- docs.rs docs -->
//! <a href="https://docs.rs/ambassador">
//!   <img src="https://img.shields.io/badge/docs-latest-blue.svg?style=flat-square"
//!     alt="docs.rs docs" />
//! </a>
//!
//! <br/>
//!
//!
//! Delegating the implementation of traits to enum variants or fields of a struct normally requires a lot of boilerplate code. Ambassador is an attempt to eliminate that boilerplate by deriving the delegating trait implementation via procedural macros.
//!
//! **The minimum supported Rust version is 1.40.0.**
//!
//! See individual macro documentation for detailed instructions.
//!
//! This is one example combining a large number of features:
//!
//! ```
//! extern crate ambassador;
//!
//! use std::collections::{HashMap, BTreeMap};
//! use std::hash::{Hash, BuildHasher};
//! use std::cmp::{Eq, Ord};
//! use std::borrow::Borrow;
//! use ambassador::{delegatable_trait, delegate_remote, Delegate};
//!
//! #[delegatable_trait]
//! pub trait Map {
//!     type K;
//!     type V;
//! }
//!
//! #[delegatable_trait]
//! pub trait Get<Q: ?Sized>: Map {
//!     fn get(&self, k: &Q) -> Option<&Self::V>;
//! }
//!
//! impl<K, V, S> Map for HashMap<K, V, S>  {
//!     type K = K;
//!     type V = V;
//! }
//!
//! impl<K, V> Map for BTreeMap<K, V>  {
//!     type K = K;
//!     type V = V;
//! }
//!
//!
//! // No automatic where clause provided for target = "self"
//! #[delegate_remote]
//! #[delegate(Get<X>, target = "self", generics = "X", where = "K: Hash + Eq + Borrow<X>, S: BuildHasher, X: Hash + Eq + ?Sized")]
//! struct HashMap<K, V, S>();
//!
//! #[delegate_remote]
//! #[delegate(Get<X>, target = "self", generics = "X", where = "K: Ord + Borrow<X>, X: Ord + ?Sized")]
//! struct BTreeMap<K, V>();
//!
//! #[derive(Delegate)]
//! #[delegate(Map)]
//! #[delegate(Get<X>, generics = "X", where = "X: ?Sized, B: Map<K=A::K, V=A::V>")]  //auto where clause misses required on super trait
//! pub enum Either<A, B> {
//!     Left(A),
//!     Right(B),
//! }
//!
//!
//! pub fn main() {
//!     let my_map: Either<HashMap<&'static str, u32>, BTreeMap<&'static str, u32>> = Either::Left([("a", 1)].into());
//!     assert_eq!(my_map.get("a"), Some(&1));
//! }
//! ```

extern crate proc_macro;

mod derive;
mod register;
mod util;

use proc_macro::TokenStream;
use quote::quote;

use crate::register::build_register_trait;

///Delegate the implementation of a trait to a struct field/enum variants by adding `#[derive(Delegate)]` and its associated attribute `#[delegate(Trait)]` to it:
///
/// ```
/// use ambassador::{Delegate, delegatable_trait};
///
/// #[delegatable_trait]
/// pub trait Shout {
///     fn shout(&self, input: &str) -> String;
/// }
///
/// pub struct Cat;
///
/// impl Shout for Cat {
///     fn shout(&self, input: &str) -> String {
///         format!("{} - meow!", input)
///     }
/// }
///
/// #[derive(Delegate)] // <-------
/// #[delegate(Shout)] // <-------- Delegate implementation of Shout to struct field
/// pub struct WrappedCat(Cat);
/// ```
///
///#### `#[delegate(..., target = "foo")]` - `target` key
///
/// For structs with multiple fields, the field that should act as delegation target can be specified via the `target` key:
///
/// ```
/// # use ambassador::{Delegate, delegatable_trait};
/// # #[delegatable_trait]
/// # pub trait Shout {
/// #     fn shout(&self, input: &str) -> String;
/// # }
/// # pub struct Cat;
/// #
/// # impl Shout for Cat {
/// #     fn shout(&self, input: &str) -> String {
/// #         format!("{} - meow!", input)
/// #     }
/// # }
///
/// #[derive(Delegate)]
/// #[delegate(Shout, target = "foo")] // <-------- Delegate implementation of Shout to struct field .foo
/// pub struct WrappedCats {
///   foo: Cat,
///   bar: Cat,
/// }
/// ```
/// This also works for tuple structs with multiple fields, by using their index as a target key:
///
/// ```
/// # use ambassador::{Delegate, delegatable_trait};
/// # #[delegatable_trait]
/// # pub trait Shout {
/// #     fn shout(&self, input: &str) -> String;
/// # }
/// # pub struct Cat;
/// #
/// # impl Shout for Cat {
/// #     fn shout(&self, input: &str) -> String {
/// #         format!("{} - meow!", input)
/// #     }
/// # }
///
/// #[derive(Delegate)]
/// #[delegate(Shout, target = "1")] // <-------- Delegate implementation of Shout to second field
/// pub struct WrappedCats(Cat, Cat);
/// ```
///
/// #### `#[delegate(..., target = "self")]` - `target="self"`
/// Types that implement all the methods of a trait without implementing the trait itself,
/// can be made to implement that trait by setting `target="self"`.
/// This doesn't work for traits with associated types and constants, and requires the where clause to be added explicitly (see `where` key).
/// If the type doesn't actually implement the methods (possibly due to an incomplete `where` clause) this can cause a `[unconditional_recursion]` error.
///
/// A possible use case of this is when refactoring some methods of a public type into a trait,
/// the type still needs to implement the methods outside the trait for semver reasons,
/// and using this feature reduces the boilderplate of implementing the trait with the same methods.
///
///
/// ```
/// # use ambassador::{Delegate, delegatable_trait};
/// # #[delegatable_trait]
/// # pub trait Shout {
/// #     fn shout(&self, input: &str) -> String;
/// # }
/// #[derive(Delegate)]
/// #[delegate(Shout, target="self")]
/// pub struct Cat;
///
/// impl Cat {
///     fn shout(&self, input: &str) -> String {
///         format!("{} - meow!", input)
///     }
/// }
/// ```
///
/// #### `#[delegate(..., where = "A: Debug")]` - `where` key
///
/// To make a delegation apply only for certain generic bounds, similar to a [native where clause](https://doc.rust-lang.org/stable/rust-by-example/generics/where.html), you can specify a `where` attribute:
///
/// A where clause is automatically applied that makes sure the target field implements the trait being delegated
/// ```
/// # use ambassador::{Delegate, delegatable_trait};
/// # #[delegatable_trait]
/// # pub trait Shout {
/// #     fn shout(&self, input: &str) -> String;
/// # }
/// use std::fmt::Debug;
///
/// #[derive(Delegate)]
/// #[delegate(Shout, where = "A: Debug")] // <---- Delegate implementation of Shout to .foo field if foo field implements Debug
/// // "A: Shout" is automatically added
/// pub struct WrappedFoo<A> {
///   foo: A,
/// }
/// ```
///
///
/// #### `#[delegate(Shout<X>, generics = "X")]` - trait generics
///
/// We can also delegate traits with generics.
/// The type parameters listed in the `generics` key are treated as fully generic.
/// The automatically added where clause ensures they are valid for the inner type being delegated to.
/// Explict where clauses to further refine these types can be added as normal.
///
/// ```
/// use ambassador::{delegatable_trait, Delegate};
/// use std::fmt::Display;
///
/// #[delegatable_trait]
/// pub trait Shout<T> {
///     fn shout(&self, input: T) -> String;
/// }
///
/// pub struct Cat;
///
/// impl<T: Display> Shout<T> for Cat {
///     fn shout(&self, input: T) -> String {
///         format!("{} - meow!", input)
///     }
/// }
///
/// #[derive(Delegate)]
/// #[delegate(Shout<X>, generics = "X")] // <-------- `X` is fully generic
/// // The automatic where clause ensures X: Display
/// // We could also use #[delegate(Shout<& 'a str>, generics = "'a")] to only delegate for &str
/// pub struct WrappedCat(Cat);
/// ```
#[proc_macro_derive(Delegate, attributes(delegate))]
pub fn delegate_macro(input: TokenStream) -> TokenStream {
    derive::delegate_macro(input)
}

/// Make an existing type that lives outside you crate delegate traits to it's members
///
/// This can be done by copy-pasting it's definition into your code under this attribute.
///
/// If the type is a struct, not all the fields have to be public, only the ones being delegated to.
///
/// ```
/// use ambassador::{delegate_remote, delegatable_trait};
///
/// #[delegatable_trait]
/// pub trait Shout {
///     fn shout(&self, input: &str) -> String;
/// }
///
/// pub struct Cat;
///
/// impl Shout for Cat {
///     fn shout(&self, input: &str) -> String {
///         format!("{} - meow!", input)
///     }
/// }
///
/// mod wrapped {
///     pub struct WrappedAnimals<A, B> {
///         pub foo: A,
///         pub bar: B,
///         baz: u32, // private field
///     }
/// }
///
/// use wrapped::*;
///
/// #[delegate_remote]
/// #[delegate(Shout, target = "bar")]
/// struct WrappedAnimals<A, B> {
///     foo: A,
///     bar: B,
///     // We don't even have to include baz since we don't delegate to it
/// }
/// ```
#[proc_macro_attribute]
pub fn delegate_remote(_attr: TokenStream, input: TokenStream) -> TokenStream {
    derive::delegate_macro(input)
}

/// Make a trait available for delegation
///
/// This also makes your trait delegatable in other crates:
///
/// ```
/// use ambassador::delegatable_trait;
///
/// #[delegatable_trait] // <-------
/// pub trait Shout {
///     fn shout(&self, input: &str) -> String;
/// }
/// ```
#[proc_macro_attribute]
pub fn delegatable_trait(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let original_item: syn::ItemTrait = syn::parse(item).unwrap();
    let register_trait = build_register_trait(&original_item);

    let expanded = quote! {
        #original_item

        #register_trait
    };
    TokenStream::from(expanded)
}

/// Make an existing trait that lives outside you crate available for delegation.
///
/// This can be done by copy-pasting the existing traits signature into your code under this attribute
///
/// ```
/// use ambassador::{Delegate, delegatable_trait_remote};
/// use std::fmt::Display;
///
/// #[delegatable_trait_remote]
/// trait Display {
///     fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error>;
/// }
///
/// struct Cat;
///
/// impl Display for Cat {
///     fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error>{
///         f.write_str("Cat")
///     }
/// }
///
/// #[derive(Delegate)]
/// #[delegate(Display)] // <-------- Delegate implementation of Display to struct field
/// pub struct WrappedCat(Cat);
/// ```
#[proc_macro_attribute]
pub fn delegatable_trait_remote(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let original_item: syn::ItemTrait = syn::parse(item).unwrap();
    let register_trait = build_register_trait(&original_item);

    let expanded = quote! {
        #register_trait
    };
    TokenStream::from(expanded)
}
