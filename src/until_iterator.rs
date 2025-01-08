use crate::marking_iterator::MarkingIterator;
use crate::parser::RecoverMatcher;

pub struct UntilIterator<'a, I: Iterator<Item: Copy>> {
    inner: &'a mut dyn MarkingIterator<I>,
    matcher: RecoverMatcher<I>,
}

impl<I: Iterator<Item: Copy>> Iterator for UntilIterator<'_, I> {
    type Item = I::Item;

    fn next(&mut self) -> Option<Self::Item> {
        if (self.matcher)(self.inner) {
            None
        } else {
            self.inner.next()
        }
    }
}

pub fn until_iter<
    I: Iterator<Item: Copy>
>(iter: &mut dyn MarkingIterator<I>, matcher: RecoverMatcher<I>)
    -> UntilIterator<I> {
    UntilIterator {
        inner: iter,
        matcher: matcher,
    }
}