use std::collections::VecDeque;

pub struct MarkIterator<I>
where
    I: Iterator<Item: Copy>,
{
    inner: I,
    buffers: Vec<VecDeque<I::Item>>,
    read_buffer: bool,
}

impl<I> MarkIterator<I>
where
    I: Iterator<Item: Copy>,
{
    pub fn mark(&mut self) {
        self.buffers.push(VecDeque::new());
    }

    pub fn reset(&mut self) {
        if self.read_buffer {
            if let Some(mut top) = self.buffers.pop(){
                if let Some(new_top) = self.buffers.last_mut() {
                    new_top.append(&mut top);
                }
            }
        }

        self.read_buffer = true;
    }

    pub fn discard(&mut self) {
        self.buffers.pop();
    }
}

pub fn mark<I>(inner: I) -> MarkIterator<I>
where
    I: Iterator<Item: Copy>,
{
    MarkIterator {
        inner: inner,
        read_buffer: false,
        buffers: vec![],
    }
}

impl<I> Iterator for MarkIterator<I>
where
    I: Iterator<Item: Copy>,
{
    type Item = I::Item;

    fn next(&mut self) -> Option<Self::Item> {
        if self.read_buffer {
            if let Some(item) = self.buffers.last_mut().expect("stack is empty").pop_front() {
                let num_buffers = self.buffers.len();
                if num_buffers > 1 {
                    self.buffers.get_mut(num_buffers - 2).unwrap().push_back(item);
                }
                return Some(item);
            }

            self.read_buffer = false;
        }

        if self.buffers.is_empty() {
            return self.inner.next();
        }

        if let Some(result) = self.inner.next() {
            self.buffers.last_mut().expect("stack is empty").push_back(result);
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
        let remaining: Vec<_> = iter.collect();

        // assert
        assert_eq!(remaining, source);
    }

    #[test]
    fn mark_has_no_direct_effect() {
        // arrange
        let source: Vec<i32> = vec![1, 2, 3, 4, 5];
        let mut iter = mark(source.clone().into_iter());

        // act
        iter.next();
        iter.mark();
        let remaining: Vec<_> = iter.collect();

        // assert
        assert_eq!(remaining, vec![2, 3, 4, 5]);
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
        let remaining: Vec<_> = iter.collect();

        // assert
        assert_eq!(remaining, vec![2, 3, 4, 5]);
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
        let remaining: Vec<_> = iter.collect();

        // assert
        assert_eq!(remaining, source);
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
        let remaining: Vec<_> = iter.collect();

        // assert
        assert_eq!(remaining, vec![3, 4, 5]);
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
        let remaining: Vec<_> = iter.collect();

        // assert
        assert_eq!(remaining, vec![4, 5]);
    }

    #[test]
    fn mark_and_reset_nested() {
        //arrange
        let source: Vec<i32> = vec![1, 2, 3, 4, 5];
        let mut iter = mark(source.into_iter());

        // act
        iter.next();
        iter.mark();
        iter.next();
        iter.mark();
        iter.next();
        iter.next();
        iter.reset();
        iter.next();
        iter.reset();
        let remaining: Vec<_> = iter.collect();

        // assert
        assert_eq!(remaining, vec![2, 3, 4, 5]);
    }

    #[test]
    fn mark_and_reset_nested_unbalanced() {
        //arrange
        let source: Vec<i32> = vec![1, 2, 3, 4, 5];
        let mut iter = mark(source.into_iter());

        // act
        iter.next();
        iter.mark();
        iter.next();
        iter.mark();
        iter.next();
        iter.next();
        iter.reset();
        let remaining: Vec<_> = iter.collect();

        // assert
        assert_eq!(remaining, vec![3, 4, 5]);
    }
}
