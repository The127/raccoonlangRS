use std::collections::VecDeque;

pub struct PutBackIterator<I>
where
    I: Iterator,
{
    inner: I,
    put_back_items: VecDeque<I::Item>,
}

impl<I> PutBackIterator<I>
where
    I: Iterator,
{
    pub(crate) fn put_back(&mut self, item: I::Item) {
        self.put_back_items.push_back(item);
    }
}

pub fn put_back<I>(inner: I) -> PutBackIterator<I>
where
    I: Iterator,
{
    PutBackIterator {
        inner: inner,
        put_back_items: VecDeque::new(),
    }
}

impl<I> Iterator for PutBackIterator<I>
where
    I: Iterator,
{
    type Item = I::Item;

    fn next(&mut self) -> Option<Self::Item> {
        self.put_back_items
            .pop_front()
            .or_else(|| self.inner.next())
    }
}

#[cfg(test)]
mod test {
    use crate::put_back_iterator::*;

    #[test]
    fn next_wraps_internal_iter() {
        // arrange
        let source: Vec<i32> = vec![1, 2, 3, 4, 5];
        let iter = put_back(source.clone().into_iter());

        // act
        let collected = iter.collect::<Vec<i32>>();

        // assert
        assert_eq!(source, collected);
    }

    #[test]
    fn put_back_single_item() {
        // arrange
        let source: Vec<i32> = vec![1, 2, 3, 4, 5];
        let mut iter = put_back(source.clone().into_iter());

        // act
        _ = iter.next();
        iter.put_back(0);
        let next = iter.next();

        // assert
        assert_eq!(next, Some(0));
    }

    #[test]
    fn put_back_multiple_item() {
        // arrange
        let source: Vec<i32> = vec![1, 2, 3, 4, 5];
        let mut iter = put_back(source.clone().into_iter());

        // act
        _ = iter.next();
        iter.put_back(0);
        iter.put_back(6);
        let next1 = iter.next();
        let next2 = iter.next();

        // assert
        assert_eq!(next1, Some(0));
        assert_eq!(next2, Some(6));
    }
}
