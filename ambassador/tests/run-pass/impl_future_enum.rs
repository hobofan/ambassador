extern crate ambassador;

use ambassador::{delegatable_trait, Delegate};

#[delegatable_trait]
pub trait Hello {
    type Error;

    async fn hello(&self) -> Result<(), Self::Error>;
    fn bye(&self) -> impl ::core::future::Future<Output = Result<(), Self::Error>> + Send;
}

pub struct Base1 {}
impl Hello for Base1 {
    type Error = ();
    async fn hello(&self) -> Result<(), ()> {
        Ok(())
    }
    fn bye(&self) -> impl ::core::future::Future<Output = Result<(), Self::Error>> + Send {
        async { Ok(()) }
    }
}
pub struct Base2 {}
impl Hello for Base2 {
    type Error = ();
    async fn hello(&self) -> Result<(), ()> {
        Ok(())
    }
    fn bye(&self) -> impl ::core::future::Future<Output = Result<(), Self::Error>> + Send {
        async { Ok(()) }
    }
}

#[derive(Delegate)]
#[delegate(Hello)]
pub enum Either {
    Base1(Base1),
    Base2(Base2),
}

#[derive(Delegate)]
#[delegate(Hello)]
pub struct One {
    base: Base1,
}

pub fn main() {}
