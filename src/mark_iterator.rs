use std::collections::VecDeque;

pub struct MarkIterator<I>
where
    I: Iterator<Item: Copy>,
{
    inner: I,
    buffer: VecDeque<I::Item>,
    marked: bool,
    read_buffer: bool,
}

impl<I> MarkIterator<I>
where
    I: Iterator<Item: Copy>,
{
    pub fn mark(&mut self) {
        self.marked = true;
    }

    pub fn reset(&mut self) {
        self.read_buffer = true;
    }

    pub fn discard(&mut self) {
        self.marked = false;
        self.buffer.clear();
    }
}

pub fn mark<I>(inner: I) -> MarkIterator<I>
where
    I: Iterator<Item: Copy>,
{
    MarkIterator {
        inner: inner,
        marked: false,
        read_buffer: false,
        buffer: VecDeque::new(),
    }
}

impl<I> Iterator for MarkIterator<I>
where
    I: Iterator<Item: Copy>,
{
    type Item = I::Item;

    fn next(&mut self) -> Option<Self::Item> {
        if self.read_buffer {
            if let Some(item) = self.buffer.pop_front() {
                return Some(item);
            }

            self.read_buffer = false;
        }

        if !self.marked {
            return self.inner.next();
        }

        if let Some(result) = self.inner.next(){
            self.buffer.push_back(result);
            return Some(result);
        }

        None
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn next_wraps_internal_iter() {
        // arrange
        let source: Vec<i32> = vec![1, 2, 3, 4, 5];
        let iter = mark(source.clone().into_iter());

        // act
        let collected = iter.collect::<Vec<i32>>();

        // assert
        assert_eq!(source, collected);
    }

    #[test]
    fn mark_has_no_direct_effect() {
        // arrange
        let source: Vec<i32> = vec![1, 2, 3, 4, 5];
        let mut iter = mark(source.clone().into_iter());

        // act
        iter.next();
        iter.mark();
        let next = iter.next();

        // assert
        assert_eq!(next, Some(2));
    }

    #[test]
    fn mark_and_reset_returns_jumps_to_mark() {
        // arrange
        let source: Vec<i32> = vec![1, 2, 3, 4, 5];
        let mut iter = mark(source.clone().into_iter());

        // act
        iter.next();
        iter.mark();
        iter.next();
        iter.reset();
        let next = iter.next();

        // assert
        assert_eq!(next, Some(2));
    }

    #[test]
    fn mark_and_reset_returns_jumps_to_start() {
        // arrange
        let source: Vec<i32> = vec![1, 2, 3, 4, 5];
        let mut iter = mark(source.clone().into_iter());

        // act
        iter.mark();
        iter.next();
        iter.next();
        iter.reset();
        let collected = iter.collect::<Vec<i32>>();

        // assert
        assert_eq!(source, collected);
    }

    #[test]
    fn mark_and_discard_continues() {
        // arrange
        let source: Vec<i32> = vec![1, 2, 3, 4, 5];
        let mut iter = mark(source.clone().into_iter());

        // act
        iter.next();
        iter.mark();
        iter.next();
        iter.discard();
        let next = iter.next();

        // assert
        assert_eq!(next, Some(3));
    }

    #[test]
    fn mark_and_discard_remarks() {
        // arrange
        let source: Vec<i32> = vec![1, 2, 3, 4, 5];
        let mut iter = mark(source.clone().into_iter());

        // act
        iter.next();
        iter.mark();
        iter.next();
        iter.discard();
        iter.next();
        iter.mark();
        iter.next();
        iter.reset();
        let next = iter.next();

        // assert
        assert_eq!(next, Some(4));
    }
}
