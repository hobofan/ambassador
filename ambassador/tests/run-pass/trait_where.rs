extern crate ambassador;
use ambassador::{delegatable_trait, Delegate};

trait GenericType {}

#[delegatable_trait]
trait GenericJob<T>
where
    T: GenericType,
{
}

struct ConcreteType;
impl GenericType for ConcreteType {}

struct Job1;
impl GenericJob<ConcreteType> for Job1 {}

#[derive(Delegate)]
#[delegate(GenericJob<T>, generics = "T", where = "T: GenericType")]
enum ConcreteJob {
    Job1(Job1),
}

fn main() {
    ConcreteJob::Job1(Job1 {});
}
