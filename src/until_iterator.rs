use std::marker::PhantomData;
use crate::marking_iterator::MarkingIterator;

pub struct UntilIterator<'a, I: Iterator<Item: Copy>, M: Fn(I::Item) -> bool> {
    inner: &'a mut dyn MarkingIterator<I>,
    matcher: M,
}

impl<I: Iterator<Item: Copy>, M: Fn(I::Item) -> bool> Iterator for UntilIterator<'_, I, M> {
    type Item = I::Item;

    fn next(&mut self) -> Option<Self::Item> {
        let mut mark = self.inner.mark();
        if let Some(t) = mark.next() {
            if (self.matcher)(t) {
                mark.reset();
                None
            } else {
                Some(t)
            }
        } else {
            None
        }
    }
}

pub fn until_iter<
    I: Iterator<Item: Copy>,
    M: Fn(I::Item) -> bool
>(iter: &mut dyn MarkingIterator<I>, matcher: M)
    -> UntilIterator<I, M> {
    UntilIterator {
        inner: iter,
        matcher: matcher,
    }
}