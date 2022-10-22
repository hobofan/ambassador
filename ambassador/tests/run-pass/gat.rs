extern crate ambassador;

use ambassador::{delegatable_trait, Delegate};

#[delegatable_trait]
pub trait StreamingIterator {
    type Output<'a>
    where
        Self: 'a;
    fn next(&mut self) -> Option<Self::Output<'_>>;
}

struct RepeatMut<T>(T);

impl<T> StreamingIterator for RepeatMut<T> {
    type Output<'a> = &'a mut T where T: 'a;

    fn next(&mut self) -> Option<Self::Output<'_>> {
        Some(&mut self.0)
    }
}

#[derive(Delegate)]
#[delegate(StreamingIterator)]
pub struct Wrap<X>(X);

pub fn main() {
    let mut x = Wrap(RepeatMut("forever".into()));
    let m: &mut String = x.next().unwrap();
    m.push('?');
    assert_eq!(x.next().unwrap(), "forever?");
}
