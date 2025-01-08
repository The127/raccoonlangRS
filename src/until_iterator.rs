use std::marker::PhantomData;
use crate::marking_iterator::MarkingIterator;

pub struct UntilIterator<'a, I_: Iterator<Item: Copy>, I: MarkingIterator<I_>, M: Fn(I::Item) -> bool> {
    inner: &'a mut I,
    matcher: M,
    _phantom: PhantomData<I_>,
}

impl<I_: Iterator<Item: Copy>, I: MarkingIterator<I_>, M: Fn(I::Item) -> bool> Iterator for UntilIterator<'_, I_, I, M> {
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

pub fn until_iter<I_: Iterator<Item: Copy>, I: MarkingIterator<I_>, M: Fn(I::Item) -> bool>(iter: &mut I, matcher: M) -> UntilIterator<I_, I, M> {
    UntilIterator {
        inner: iter,
        matcher: matcher,
        _phantom: PhantomData::default(),
    }
}