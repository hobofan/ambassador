extern crate ambassador;

use ambassador::*;

pub struct Base1 {}

#[derive(Delegate)]
#[delegate(Hello, target = "self")]
pub struct Base2 {}

impl Base2 {
    async fn hello(&self) {}
}

#[delegatable_trait]
trait Hello {
    async fn hello(&self);
}

impl Hello for Base1 {
    async fn hello(&self) {}
}

#[derive(Delegate)]
#[delegate(Hello)]
pub struct Dram {
    inner: Base1,
}

#[derive(Delegate)]
#[delegate(Hello)]
pub enum Either {
    Base1(Base1),
    Base2(Base2),
}

struct OpBase(Option<Base1>);

#[delegate_to_methods]
#[delegate(Hello, target_ref = "unwrap_ref")]
impl OpBase {
    fn unwrap_ref(&self) -> &Base1 {
        self.0.as_ref().unwrap()
    }
}

fn main() {
    let d = Dram { inner: Base1 {} };

    let _ = d.hello();
}
