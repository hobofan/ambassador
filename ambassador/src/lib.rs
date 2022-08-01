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
//! **The minimum supported Rust version is 1.53.0.**
//!
//! See individual macro documentation for detailed instructions.
//!
//! This is one example combining a large number of features:
//!
//! ```
//! extern crate ambassador;
//!
//! use std::collections::{HashMap, BTreeMap};
//! use std::borrow::Borrow;
//! use std::cmp::{Eq, Ord};
//! use std::hash::{Hash, BuildHasher};
//! use std::ops::Deref;
//! use ambassador::{delegatable_trait, delegate_remote, delegate_to_remote_methods, Delegate};
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
//! #[delegate_to_remote_methods]
//! #[delegate(Map, target_ref = "deref")]
//! impl<M: ?Sized + Map> Map for Box<M> {
//!     fn deref(&self) -> &M;
//! }
//!
//! fn takes_map(_m: &impl Map<K = &'static str, V = u32>) { }
//!
//! pub fn main() {
//!     let my_map: Either<HashMap<&'static str, u32>, BTreeMap<&'static str, u32>> = Either::Left([("a", 1)].into());
//!     assert_eq!(my_map.get("a"), Some(&1));
//!
//!     let boxed: Box<dyn Map<K = &'static str, V = u32>> = Box::new(my_map);
//!     takes_map(&boxed);
//! }
//! ```
//!
//! # Backwards Compatibility
//! Since delegateable traits from one crate can be used in anther crate backwards compatibility of switching to 0.3.x depends on the use case
//! ## Self Contained Crate
//! Switching to 0.3.x should just work,
//! in this case it safe to disable the "backward_compatible" feature
//! ## Library with public delegatable traits
//! Make sure use the "backward_compatible" feature (enabled by default),
//! this makes sure users of your library using an older version of ambassador aren't affected by the upgrade
//! ## Users of a library with public delegatable traits
//! Try to use the same version of ambassador as the library you're using

extern crate core;
extern crate proc_macro;

mod delegate_shared;
mod delegate_to_methods;
mod derive;
mod register;
mod util;

use proc_macro::TokenStream;
use quote::quote;

use crate::register::build_register_trait;

/// Delegate the implementation of a trait to a struct field/enum variants by adding `#[derive(Delegate)]` and its associated attribute `#[delegate(Trait)]` to it:
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
///
///
/// #### `#[delegate(Shout, automatic_where_clause = "false")]` - inhibit automatic generation of `where` clause.
///
/// Normally `#[derive(Delegate)]` generates code to ensure that chosen field indeed implementes  the trait
/// you want to implement for the container struct.
/// However, you may want to delegate implementation to something that does not in fact fully implement the trait
/// in question, instead just have compatible methods that can be called as if the trait were in fact implemented.
///
/// One notable case of this is implementing `MyTrait` for `Box<dyn MyTrait>` or similar trait object things.
/// It is not always possible even for object-safe traits, but is often possible.
///
/// ```
/// use ambassador::{delegatable_trait, Delegate};
/// use std::fmt::Display;
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
/// #[derive(Delegate)]
/// #[delegate(Shout, automatic_where_clause="false")]
/// pub struct BoxedAnimal(pub Box<dyn Shout + Send + Sync>);
///
/// // Can accept both `Cat` and `BoxedAnimal`.
/// fn recording_studio<S: Shout>(voice_actor: S){}
/// ```
#[proc_macro_derive(Delegate, attributes(delegate))]
pub fn delegate_macro(input: TokenStream) -> TokenStream {
    derive::delegate_macro(input)
}

/// Delegate the implementation of a trait to a type's methods by adding `#[delegate_to_methods]` and its associated attribute `#[delegate(Trait)]` to the relevant impl block
///
/// #### `#[delegate(..., target_owned = "foo", target_ref = "bar", target_mut = "baz")]` - `target` keys
/// Three different target methods can be specified depending on the receiver of of the trait method being delegated.
/// These methods must have the signatures target_owned: "fn foo(self) -> X", target_ref: "fn bar(&self) -> &X", and target_mut: "fn baz(&mut self) -> &mut X"
/// where X is the same type for all three.
/// Excluding some of these attributes is allowed as long as the trait being delegated to doesn't have any methods with the relevant receiver
/// Additional methods that don't have any of the relevant signature types may be included in the impl block as long as they are never used as targets.
///
/// #### The `where` and `generics` keys described in [`Delegate`] are also supported and function the same way
///
/// ```
/// use std::ops::{Deref, DerefMut};
/// use ambassador::{delegate_to_methods, delegatable_trait};
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
/// pub struct BoxedCat(Box<Cat>); // Target is hidden behind a box
///
/// #[delegate_to_methods]
/// #[delegate(Shout, target_ref = "inner", target_mut = "inner_mut")]
/// impl BoxedCat {
///    fn inner(&self) -> &Cat {
///         self.0.deref()
///     }
///
///     // Note we don't need target_mut = "inner_mut" in this case but it is included as an example
///     // The return type must be &mut Cat to match inner's return type &Cat
///     fn inner_mut(&mut self) -> &mut Cat {
///         self.0.deref_mut()
///     }
///
///     // You can also have extra methods here:
///     fn another_one(&self) { }
/// }
/// ```
///
/// #### `delegate_to_methods` on an `impl Trait for ...` block
///
/// It's also valid to use `delegate_to_methods` with a trait impl; i.e. to use
/// the methods from a different trait to delegate a trait. For example:
/// ```
/// # use std::ops::{Deref, DerefMut};
/// # use ambassador::{delegate_to_methods, delegatable_trait};
/// # #[delegatable_trait]
/// # pub trait Shout {
/// #     fn shout(&self, input: &str) -> String;
/// # }
/// # pub struct Cat;
/// # impl Shout for Cat {
/// #     fn shout(&self, input: &str) -> String {
/// #         format!("{} - meow!", input)
/// #     }
/// # }
///
/// pub struct RefCat<'a>(&'a Cat);
///
/// #[delegate_to_methods]
/// #[delegate(Shout, target_ref = "deref")]
/// impl<'a> Deref for RefCat<'a> {
///     type Target = Cat;
///
///     fn deref(&self) -> &Cat { &self.0 }
/// }
/// ```
///
/// Note that this has a caveat: if the target methods you are delegating to
/// share the same name as any of the methods in the trait being delegated you
/// will likely get errors about ambiguity. For example:
///
/// ```rust,compile_fail
/// # use std::ops::{Deref, DerefMut};
/// # use ambassador::{delegate_to_methods, delegatable_trait};
/// # #[delegatable_trait]
/// # pub trait Shout {
/// #     fn shout(&self, input: &str) -> String;
/// # }
/// # pub struct Cat;
/// # impl Shout for Cat {
/// #     fn shout(&self, input: &str) -> String {
/// #         format!("{} - meow!", input)
/// #     }
/// # }
///
/// pub struct RefCat<'a>(&'a Cat);
///
/// trait GetAShouter {
///   type Shouter: Shout;
///
///   fn shout(&self) -> &Self::Shouter;
/// }
///
/// #[delegate_to_methods]
/// #[delegate(Shout, target_ref = "shout")]
/// impl<'a> GetAShouter for RefCat<'a> {
///     type Shouter = Cat;
///
///     fn shout(&self) -> &Cat { &self.0 }
/// }
/// ```
///
/// Yields:
/// ```text
/// error[E0034]: multiple applicable items in scope
///   --> src/lib.rs:363:32
///    |
/// 26 | #[delegate(Shout, target_ref = "shout")]
///    |                                ^^^^^^^ multiple `shout` found
///    |
/// note: candidate #1 is defined in an impl of the trait `Shout` for the type `RefCat<'a>`
///   --> src/lib.rs:345:5
///    |
/// 8  |     fn shout(&self, input: &str) -> String;
///    |     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
/// ...
/// 25 | #[delegate_to_methods]
///    | ---------------------- in this procedural macro expansion
/// note: candidate #2 is defined in an impl of the trait `GetAShouter` for the type `RefCat<'a>`
///   --> src/lib.rs:367:5
///    |
/// 30 |     fn shout(&self) -> &Cat { &self.0 }
///    |     ^^^^^^^^^^^^^^^^^^^^^^^
///    = note: this error originates in the macro `ambassador_impl_Shout` (in Nightly builds, run with -Z macro-backtrace for more info)
/// ```
///
/// This is not an issue when the target methods are _inherent_ since inherent
/// methods explicitly [have priority over trait methods](https://dtolnay.github.io/rust-quiz/23)
/// during method resolution.
///
/// The workaround is to create wrapper inherent methods for the trait's methods:
/// ```
/// # use std::ops::{Deref, DerefMut};
/// # use ambassador::{delegate_to_methods, delegatable_trait};
/// # #[delegatable_trait]
/// # pub trait Shout {
/// #     fn shout(&self, input: &str) -> String;
/// # }
/// # pub struct Cat;
/// # impl Shout for Cat {
/// #     fn shout(&self, input: &str) -> String {
/// #         format!("{} - meow!", input)
/// #     }
/// # }
///
/// pub struct RefCat<'a>(&'a Cat);
///
/// trait GetAShouter {
///   type Shouter: Shout;
///
///   fn shout(&self) -> &Self::Shouter;
/// }
///
/// impl<'a> GetAShouter for RefCat<'a> {
///     type Shouter = Cat;
///
///     fn shout(&self) -> &Cat { &self.0 }
/// }
///
/// #[delegate_to_methods]
/// #[delegate(Shout, target_ref = "get_a_shouter")]
/// impl<'a> RefCat<'a> { fn get_a_shouter(&self) -> &Cat { GetAShouter::shout(self) } }
/// ```
#[proc_macro_attribute]
pub fn delegate_to_methods(_attr: TokenStream, input: TokenStream) -> TokenStream {
    delegate_to_methods::delegate_macro(input, true)
}

/// Delegate the implementation of a trait to methods on a type that are defined
/// _elsewhere_.
///
/// This macro is identical to [`delegate_to_methods`] except that it does not
/// actually produce an `impl` block on the type for the target methods; instead
/// it assumes that the methods are implemented elsewhere.
/// ```
/// use ambassador::{delegate_to_remote_methods, delegatable_trait};
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
/// pub struct BoxedCat(Box<Cat>);
///
/// impl BoxedCat {
///    fn inner(&self) -> &Cat { &self.0 }
/// }
///
/// #[delegate_to_remote_methods]
/// #[delegate(Shout, target_ref = "inner")]
/// impl BoxedCat {
///     // `inner` can be defined anywhere: trait method or inherent method, in
///     // this crate or elsewhere
///     fn inner(&self) -> &Cat;
/// }
/// ```
///
/// As such, this macro will actually error if you provide it with methods with
/// blocks, method signatures that aren't used by a `delegate` attribute on the
/// impl, or other impl items:
/// ```rust,compile_fail
/// # use ambassador::{delegate_to_remote_methods, delegatable_trait};
/// # #[delegatable_trait]
/// # pub trait Shout { fn shout(&self, input: &str) -> String; }
/// # pub struct Cat;
/// # impl Shout for Cat {
/// #     fn shout(&self, input: &str) -> String { format!("{} - meow!", input) }
/// # }
/// # pub struct BoxedCat(Box<Cat>);
/// # impl BoxedCat { fn inner(&self) -> &Cat { &self.0 } }
///
/// #[delegate_to_remote_methods]
/// #[delegate(Shout, target_ref = "inner")]
/// impl BoxedCat {
///     fn inner(&self) -> &Cat { &self.0 } // This is an error, method bodies
///                                         // aren't accepted
///
///     fn extra(&self);                    // Extra methods are also error since no
///                                         // `impl BoxedCat { ... }` is actually
///                                         // emitted
///
///     const CONST: () = ();               // This is also an error.
/// }
/// ```
///
/// Target methods can come from a trait or be inherent methods whose actual
/// implementation lives elsewhere. You can mix and match:
/// ```
/// # use ambassador::{delegate_to_remote_methods, delegatable_trait};
/// # #[delegatable_trait]
/// # pub trait Shout { fn shout(&self, input: &str) -> String; }
/// # pub struct Cat;
/// # impl Shout for Cat {
/// #     fn shout(&self, input: &str) -> String { format!("{} - meow!", input) }
/// # }
/// # pub struct BoxedCat(Box<Cat>);
/// # impl BoxedCat { fn inner(&self) -> &Cat { &self.0 } }
/// use std::ops::{Deref, DerefMut};
///
/// impl Deref for BoxedCat {
///     type Target = Cat;
///
///     fn deref(&self) -> &Self::Target { &self.0 }
/// }
///
/// impl DerefMut for BoxedCat {
///     fn deref_mut(&mut self) -> &mut Self::Target { &mut self.0 }
/// }
///
/// #[delegate_to_remote_methods]
/// #[delegate(Shout, target_ref = "inner", target_mut = "deref_mut")]
/// impl BoxedCat {
///     fn inner(&self) -> &Cat;
///     fn deref_mut(&mut self) -> &mut Cat;
/// }
/// ```
/// Note that if you do use target methods from a trait, the trait must be in
/// scope.
///
/// Because this macro does not implement any inherent methods on the type being
/// delegated to, the type can be remote (like with [`delegate_remote`]):
/// ```
/// # use ambassador::{delegate_to_remote_methods, delegatable_trait};
/// # #[delegatable_trait]
/// # pub trait Shout { fn shout(&self, input: &str) -> String; }
/// # pub struct Cat;
/// # impl Shout for Cat {
/// #     fn shout(&self, input: &str) -> String { format!("{} - meow!", input) }
/// # }
/// use std::ops::{Deref, DerefMut};
///
/// // Note that this impl says `Deref for Box<Cat>`.
/// //
/// // The trait in this impl is ignored and only serves as documentation here.
/// #[delegate_to_remote_methods]
/// #[delegate(Shout, target_ref = "deref")]
/// impl Deref for Box<Cat> {
///     fn deref(&self) -> &Cat;
/// }
///
/// fn shout(_: &impl Shout) { }
///
/// fn main() {
///     let c = Cat;
///     shout(&c);
///
///     let boxed: Box<Cat> = Box::new(c);
///     shout(&boxed);
/// }
/// ```
///
/// This can be used in conjunction with generics to provide blanket delegated
/// implementations of a local trait without needing to create intermediary
/// local traits that provide target methods to use:
/// ```
/// # use ambassador::{delegatable_trait, delegate_to_remote_methods};
/// # use std::ops::{Deref, DerefMut};
/// # #[delegatable_trait]
/// # pub trait Shout { fn shout(&self, input: &str) -> String; }
/// # pub struct Cat;
/// # impl Shout for Cat { fn shout(&self, input: &str) -> String { format!("{} - meow!", input) } }
/// use std::{sync::Arc, rc::Rc};
///
/// pub struct Dog;
/// impl Shout for Dog {
///     fn shout(&self, input: &str) -> String { format!("{} - wuff!", input) }
/// }
///
/// #[delegate_to_remote_methods]
/// #[delegate(Shout, target_ref = "deref")]
/// impl<S: ?Sized + Shout, T: Deref<Target = S>> T {
///     fn deref(&self) -> &S;
/// }
///
/// pub fn shout(pet: &impl Shout) { println!("{}", pet.shout("hi")); }
///
/// pub fn main() {
///     shout(&Cat);
///     shout(&Dog);
///
///     let a: Box<dyn Shout> = Box::new(Cat);
///     let b: Arc<dyn Shout + Send> = Arc::new(Dog);
///     let c: Rc<dyn Shout + Send + Sync + 'static> = Rc::new(Cat);
///     shout(&a);
///     shout(&b);
///     shout(&c);
///
///     let d: Box<Cat> = Box::new(Cat);
///     shout(&d);
/// }
/// ```
#[proc_macro_attribute]
pub fn delegate_to_remote_methods(_attr: TokenStream, input: TokenStream) -> TokenStream {
    delegate_to_methods::delegate_macro(input, false)
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
