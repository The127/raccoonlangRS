use crate::parser::RecoverMatcher;
use std::collections::VecDeque;

pub fn make_awesome<I: Iterator<Item: Copy>>(inner: I) -> impl AwesomeIterator<I> {
    AwesomeContainer {
        inner: inner,
        current_read_buffer: None,
        buffers: vec![],
        until_matchers: vec![],
    }
}

pub trait AwesomeIterator<I: Iterator<Item: Copy>>: Iterator<Item = I::Item> {
    #[must_use]
    fn mark(&mut self) -> IteratorMark<I>;

    #[must_use]
    fn until(&mut self, matcher: RecoverMatcher<I>) -> IteratorUntil<I>;

    fn peek(&mut self) -> Option<Self::Item>;
}

struct AwesomeContainer<I: Iterator<Item: Copy>> {
    inner: I,
    buffers: Vec<VecDeque<I::Item>>,
    current_read_buffer: Option<VecDeque<I::Item>>,
    until_matchers: Vec<RecoverMatcher<I>>,
}

impl<I: Iterator<Item: Copy>> AwesomeContainer<I> {
    fn create_mark(&mut self) {
        self.buffers.push(VecDeque::new());
    }

    fn reset_mark(&mut self) {
        if let Some(current) = &mut self.current_read_buffer {
            let mut new = self.buffers.pop().expect("no marking to reset to");
            new.append(current);
            self.current_read_buffer = Some(new);
        } else {
            self.current_read_buffer = Some(self.buffers.pop().expect("no marking to reset to"));
        }
    }

    fn discard_mark(&mut self) {
        let mut top = self.buffers.pop().expect("no marking to discard");
        if let Some(new_top) = self.buffers.last_mut() {
            new_top.append(&mut top);
        }
    }

    fn push_until(&mut self, matcher: RecoverMatcher<I>) {
        self.until_matchers.push(matcher);
    }

    fn pop_until(&mut self) {
        self.until_matchers
            .pop()
            .expect("no until matcher to discard");
    }

    fn marking_next(&mut self) -> Option<I::Item> {
        if let Some(buf) = &mut self.current_read_buffer {
            if let Some(item) = buf.pop_front() {
                if let Some(top) = self.buffers.last_mut() {
                    top.push_back(item);
                }

                return Some(item);
            }

            if buf.is_empty() {
                self.current_read_buffer = None;
            }
        }

        if self.buffers.is_empty() {
            return self.inner.next();
        }

        if let Some(result) = self.inner.next() {
            self.buffers
                .last_mut()
                .expect("stack is empty")
                .push_back(result);
            return Some(result);
        }

        None
    }

    fn until_next(&mut self, skip_matchers: usize) -> Option<I::Item> {
        let num_matchers = self.until_matchers.len();
        for i in 0..(num_matchers - skip_matchers) {
            let matcher = self.until_matchers[i].clone();
            let mut wrapper = UntilHelperWrapper(self, num_matchers - i);
            if matcher(&mut wrapper) {
                return None
            }
        }

        self.marking_next()
    }

    fn peek_internal(&mut self, skip_until: usize) -> Option<I::Item> {
        let mut mark = self.mark().auto_reset();
        if skip_until > 0 {
            let mut wrapper = UntilHelperWrapper(mark.iter, skip_until);
            wrapper.next()
        } else {
            mark.next()
        }
    }
}

impl<I: Iterator<Item: Copy>> Iterator for AwesomeContainer<I> {
    type Item = I::Item;

    fn next(&mut self) -> Option<Self::Item> {
        self.until_next(0)
    }
}

impl<I: Iterator<Item: Copy>> AwesomeIterator<I> for AwesomeContainer<I> {
    fn mark(&mut self) -> IteratorMark<I> {
        self.create_mark();
        IteratorMark {
            iter: self,
            used_up: false,
            auto_reset: false,
            skip_until: 0,
        }
    }

    fn until(&mut self, matcher: RecoverMatcher<I>) -> IteratorUntil<I> {
        self.push_until(matcher);
        IteratorUntil {
            iter: self,
            used_up: false,
        }
    }

    fn peek(&mut self) -> Option<Self::Item> {
        self.peek_internal(0)
    }
}

struct UntilHelperWrapper<'a, I: Iterator<Item: Copy>>(&'a mut AwesomeContainer<I>, usize);

impl<'a, I: Iterator<Item: Copy>> AwesomeIterator<I> for UntilHelperWrapper<'a, I> {
    fn mark(&mut self) -> IteratorMark<I> {
        self.0.create_mark();
        IteratorMark {
            iter: self.0,
            used_up: false,
            auto_reset: false,
            skip_until: self.1,
        }
    }

    fn until(&mut self, _: RecoverMatcher<I>) -> IteratorUntil<I> {
        panic!("not supported");
    }

    fn peek(&mut self) -> Option<Self::Item> {
        self.0.peek_internal(self.1)
    }
}

impl<'a, I: Iterator<Item: Copy>> Iterator for UntilHelperWrapper<'a, I> {
    type Item = I::Item;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.until_next(self.1)
    }
}

pub struct IteratorUntil<'a, I: Iterator<Item: Copy>> {
    iter: &'a mut AwesomeContainer<I>,
    used_up: bool,
}

impl<I: Iterator<Item: Copy>> IteratorUntil<'_, I> {
    pub fn discard(mut self) {
        self.iter.pop_until();
        self.used_up = true;
    }
}

impl<I: Iterator<Item: Copy>> AwesomeIterator<I> for IteratorUntil<'_, I> {
    fn mark(&mut self) -> IteratorMark<I> {
        self.iter.mark()
    }

    fn until(&mut self, matcher: RecoverMatcher<I>) -> IteratorUntil<I> {
        self.iter.until(matcher)
    }

    fn peek(&mut self) -> Option<Self::Item> {
        self.iter.peek()
    }
}

impl<I: Iterator<Item: Copy>> Iterator for IteratorUntil<'_, I> {
    type Item = I::Item;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next()
    }
}

impl<I: Iterator<Item: Copy>> Drop for IteratorUntil<'_, I> {
    fn drop(&mut self) {
        if self.used_up {
            return;
        }
        self.iter.pop_until();
    }
}

pub struct IteratorMark<'a, I: Iterator<Item: Copy>> {
    iter: &'a mut AwesomeContainer<I>,
    used_up: bool,
    auto_reset: bool,
    skip_until: usize,
}

impl<I: Iterator<Item: Copy>> IteratorMark<'_, I> {
    pub fn reset(mut self) {
        self.iter.reset_mark();
        self.used_up = true;
    }

    pub fn discard(mut self) {
        self.iter.discard_mark();
        self.used_up = true;
    }

    pub fn auto_reset(mut self) -> Self {
        self.auto_reset = true;
        self
    }
}

impl<I: Iterator<Item: Copy>> AwesomeIterator<I> for IteratorMark<'_, I> {
    fn mark(&mut self) -> IteratorMark<I> {
        self.iter.mark()
    }

    fn until(&mut self, matcher: RecoverMatcher<I>) -> IteratorUntil<I> {
        self.iter.until(matcher)
    }

    fn peek(&mut self) -> Option<Self::Item> {
        self.iter.peek()
    }
}

impl<I: Iterator<Item: Copy>> Iterator for IteratorMark<'_, I> {
    type Item = I::Item;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.until_next(self.skip_until)
    }
}

impl<I: Iterator<Item: Copy>> Drop for IteratorMark<'_, I> {
    fn drop(&mut self) {
        if self.used_up {
            return;
        }
        if self.auto_reset {
            self.iter.reset_mark();
        } else {
            self.iter.discard_mark();
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn peek() {
        // arrange
        let source = vec![1, 2, 3, 4, 5];
        let mut iter = make_awesome(source.into_iter());

        // act
        let peeked = iter.peek();
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(peeked, Some(1));
        assert_eq!(remaining, vec![1, 2, 3, 4, 5]);
    }

    #[test]
    fn repeated_peek() {
        // arrange
        let source = vec![1, 2, 3, 4, 5];
        let mut iter = make_awesome(source.into_iter());

        // act
        let peeked1 = iter.peek();
        let peeked2 = iter.peek();
        let peeked3 = iter.peek();
        let peeked4 = iter.peek();
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(peeked1, Some(1));
        assert_eq!(peeked2, Some(1));
        assert_eq!(peeked3, Some(1));
        assert_eq!(peeked4, Some(1));
        assert_eq!(remaining, vec![1, 2, 3, 4, 5]);
    }

    #[test]
    fn multiple_peek() {
        // arrange
        let source = vec![1, 2, 3, 4, 5];
        let mut iter = make_awesome(source.into_iter());

        // act
        let peeked1 = iter.peek();
        let next1 = iter.next();
        let peeked2 = iter.peek();
        let next2 = iter.next();
        let peeked3 = iter.peek();
        let next3 = iter.next();
        let peeked4 = iter.peek();
        let next4 = iter.next();
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(peeked1, Some(1));
        assert_eq!(next1, Some(1));
        assert_eq!(peeked2, Some(2));
        assert_eq!(next2, Some(2));
        assert_eq!(peeked3, Some(3));
        assert_eq!(next3, Some(3));
        assert_eq!(peeked4, Some(4));
        assert_eq!(next4, Some(4));
        assert_eq!(remaining, vec![5]);
    }

    #[test]
    fn until() {
        // arrange
        let source = vec![1, 2, 3, 4, 5];
        let mut iter = make_awesome(source.into_iter());

        // act
        let until = iter.until(|i| i.peek() == Some(3));
        let before_3 = until.collect::<Vec<_>>();
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(before_3, vec![1, 2]);
        assert_eq!(remaining, vec![3, 4, 5]);
    }

    #[test]
    fn stacked_until() {
        // arrange
        let source = vec![1, 2, 3, 4, 5];
        let mut iter = make_awesome(source.into_iter());

        // act
        let remaining3 = {
            let mut until3 = iter.until(|i| i.peek() == Some(3));
            let mark = until3.mark().auto_reset();
            mark.collect::<Vec<_>>()
        };

        let remaining5 = {
            let mut until5 = iter.until(|i| i.peek() == Some(5));
            let mark = until5.mark().auto_reset();
            mark.collect::<Vec<_>>()
        };

        // assert
        assert_eq!(remaining3, vec![1, 2,]);
        assert_eq!(remaining5, vec![1, 2, 3, 4,]);
    }

    #[test]
    fn nested_until_inner_hits_first() {
        // arrange
        let source = vec![1, 2, 3, 4, 5];
        let mut iter = make_awesome(source.into_iter());

        let mut iter_until_5 = iter.until(|i| i.peek() == Some(5));
        let iter_until_3 = iter_until_5.until(|i| i.peek() == Some(3));

        // act
        let values = iter_until_3.collect::<Vec<_>>();

        // assert
        assert_eq!(values, vec![1,2]);
    }

    #[test]
    fn nested_until_outer_hits_first() {
        // arrange
        let source = vec![1, 2, 3, 4, 5];
        let mut iter = make_awesome(source.into_iter());

        let mut iter_until_3 = iter.until(|i| i.peek() == Some(3));
        let mut iter_until_5 = iter_until_3.until(|i| i.peek() == Some(5));

        // act
        let values = iter_until_5.collect::<Vec<_>>();

        // assert
        assert_eq!(values, vec![1,2]);
    }

    #[test]
    fn until_marked() {
        // arrange
        let source = vec![1, 2, 3, 4, 5];
        let mut iter = make_awesome(source.into_iter());

        // act
        let (before_4, remaining) = {
            let mut mark = iter.mark().auto_reset();
            mark.next();
            {
                let until = mark.until(|i| i.peek() == Some(4));
                let before_4 = until.collect::<Vec<_>>();
                let remaining = mark.collect::<Vec<_>>();
                (before_4, remaining)
            }
        };
        let remaining_after_reset = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(before_4, vec![2, 3]);
        assert_eq!(remaining, vec![4, 5]);
        assert_eq!(remaining_after_reset, vec![1, 2, 3, 4, 5]);
    }

    #[test]
    fn mark_until_reset() {
        // arrange
        let source = vec![1, 2, 3, 4, 5];
        let mut iter = make_awesome(source.into_iter());

        // act
        let mut until = iter.until(|i| i.peek() == Some(4));
        until.next();
        let before_4 = {
            let mark = until.mark().auto_reset();
            mark.collect::<Vec<_>>()
        };
        until.discard();
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(before_4, vec![2, 3]);
        assert_eq!(remaining, vec![2, 3, 4, 5]);
    }

    #[test]
    fn until_after_reset() {
        // arrange
        let source = vec![1, 2, 3, 4, 5];
        let mut iter = make_awesome(source.into_iter());

        // act
        {
            let mark = iter.mark().auto_reset();
            let _ = mark.collect::<Vec<_>>();
        }

        let until = iter.until(|i| i.peek() == Some(4));
        let before_4 = until.collect::<Vec<_>>();
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(before_4, vec![1, 2, 3]);
        assert_eq!(remaining, vec![4, 5]);
    }

    #[test]
    fn peek_after_reset() {
        // arrange
        let source = vec![1, 2, 3, 4, 5];
        let mut iter = make_awesome(source.into_iter());

        // act
        {
            let mark = iter.mark().auto_reset();
            let _ = mark.collect::<Vec<_>>();
        }

        let peeked = iter.peek();
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(peeked, Some(1));
        assert_eq!(remaining, vec![1, 2, 3, 4, 5]);
    }

    #[test]
    fn peek_after_discard() {
        // arrange
        let source = vec![1, 2, 3, 4, 5];
        let mut iter = make_awesome(source.into_iter());

        // act
        {
            let mut mark = iter.mark();
            mark.next();
            mark.next();
        }

        let peeked = iter.peek();
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(peeked, Some(3));
        assert_eq!(remaining, vec![3, 4, 5]);
    }

    #[test]
    fn mark_reset_between_peeks() {
        // arrange
        let source = vec![1, 2, 3, 4, 5];
        let mut iter = make_awesome(source.into_iter());

        // act
        let peek1 = iter.peek();
        {
            let mut mark = iter.mark().auto_reset();
            mark.next();
            mark.next();
        }
        let peek2 = iter.peek();
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(peek1, Some(1));
        assert_eq!(peek2, Some(1));
        assert_eq!(remaining, vec![1, 2, 3, 4, 5]);
    }

    #[test]
    fn mark_drop() {
        // arrange
        let source = vec![1, 2, 3, 4, 5];
        let mut iter = make_awesome(source.into_iter());

        // act
        let mut mark = iter.mark();
        mark.next();
        mark.next();
        {
            let mut mark2 = mark.mark();
            mark2.next();
            mark2.next();
        }
        mark.reset();

        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(remaining, vec![1, 2, 3, 4, 5]);
    }

    #[test]
    fn mark_drop_autoreset() {
        // arrange
        let source = vec![1, 2, 3, 4, 5];
        let mut iter = make_awesome(source.into_iter());

        // act
        {
            let mut mark2 = iter.mark().auto_reset();
            mark2.next();
            mark2.next();
        }

        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(remaining, vec![1, 2, 3, 4, 5]);
    }

    #[test]
    fn mark_discard() {
        // arrange
        let source = vec![1, 2, 3, 4, 5];
        let mut iter = make_awesome(source.into_iter());

        // act
        let mut mark = iter.mark();
        mark.next();
        mark.next();
        let mut mark2 = mark.mark();
        mark2.next();
        mark2.next();
        mark2.discard();
        mark.reset();

        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(remaining, vec![1, 2, 3, 4, 5]);
    }

    #[test]
    fn mark_reset() {
        // arrange
        let source = vec![1, 2, 3, 4, 5];
        let mut iter = make_awesome(source.into_iter());

        // act
        let mut mark = iter.mark();
        mark.next();
        mark.next();

        mark.reset();

        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(remaining, vec![1, 2, 3, 4, 5]);
    }

    #[test]
    fn mark_and_reset_returns_jumps_to_mark2() {
        let source = vec![1, 2, 3, 4, 5];
        let mut orig_iter = make_awesome(source.into_iter());
        let iter = &mut orig_iter;

        iter.next();
        let mut iter = iter.mark();
        iter.next();
        iter.reset();

        let remaining: Vec<_> = orig_iter.collect();

        assert_eq!(remaining, vec![2, 3, 4, 5]);
    }

    macro_rules! mark_iter_test_helper {
        ($name:ident: [$($input:expr),*] -> [$($step:ident),*] -> panic) => {
            #[test]
            #[should_panic]
            fn $name () {
                // arrange
                let source = vec![$($input,)*];
                let mut iter = make_awesome(source.into_iter());


                // act
                $(
                    let mut iter = mark_iter_test_step!(iter, $step);
                )*
            }
        };
        ($name:ident: [$($input:expr),*] -> $iter:ident => $steps:tt -> [$($expected:expr),*]) => {
            #[test]
            fn $name () {
                // arrange
                let source = vec![$($input,)*];
                let mut $iter = make_awesome(source.into_iter());

                // act
                $steps;

                let remaining: Vec<_> = $iter.collect();

                // assert
                assert_eq!(remaining, vec![$($expected,)*]);
            }
        };
    }
    macro_rules! mark_iter_tests {
        ($($name:ident: $input:tt -> $iter:ident => $steps:tt -> $result:tt;)*) => {
            $(
                mark_iter_test_helper! { $name: $input -> $iter => $steps -> $result }
            )*
        }
    }

    mark_iter_tests! {
        wraps_internal_iter: [1,2,3,4,5] -> iter => {} -> [1,2,3,4,5];

        strings: ["foo", "bar", "foobar", "fuzz"] -> iter => {} -> ["foo", "bar", "foobar", "fuzz"];

        mark_has_no_direct_effect: [1,2,3,4,5] -> iter => {
            iter.next();
            let _ = iter.mark();
        } -> [2,3,4,5];

        mark_and_reset_returns_jumps_to_mark: [1,2,3,4,5] -> iter => {
            iter.next();
            let mut iter2 = iter.mark();
            iter2.next();
            iter2.reset();
        } -> [2,3,4,5];

        mark_and_reset_returns_jumps_to_start: [1,2,3,4,5] -> iter => {
            let mut iter2 = iter.mark();
            iter2.next();
            iter2.next();
            iter2.reset();
        } -> [1,2,3,4,5];

        mark_and_discard_continues: [1,2,3,4,5] -> iter => {
            iter.next();
            let mut iter2 = iter.mark();
            iter2.next();
            iter2.discard();
        } -> [3,4,5];

        mark_and_discard_remarks: [1,2,3,4,5] -> iter => {
            iter.next();
            let mut iter2 = iter.mark();
            iter2.next();
            iter2.discard();
            iter.next();
            let mut iter3 = iter.mark();
            iter3.next();
            iter3.reset();
        } -> [4,5];

        mark_and_discard_and_next: [1,2,3,4,5] -> iter => {
            iter.next();
            let mut iter2 = iter.mark();
            iter2.next();
            iter2.discard();
            iter.next();
        } -> [4,5];

        mark_and_reset_nested: [1,2,3,4,5] -> iter => {
            iter.next();
            let mut iter2 = iter.mark();
            iter2.next();
            let mut iter3 = iter2.mark();
            iter3.next();
            iter3.next();
            iter3.reset();
            iter2.next();
            iter2.reset();
        } -> [2,3,4,5];

        mark_and_reset_nested_middle: [1,2,3,4,5] -> iter => {
            iter.next();
            let mut iter2 = iter.mark();
            iter2.next();
            let mut iter3 = iter2.mark();
            iter3.next();
            let mut iter4 = iter3.mark();
            iter4.next();
            iter4.reset();
            iter3.next();
            iter3.reset();
        } -> [3,4,5];

        mark_and_discard_nested: [1,2,3,4,5] -> iter => {
            iter.next();
            let mut iter2 = iter.mark();
            iter2.next();
            let mut iter3 = iter2.mark();
            iter3.next();
            iter3.next();
            iter3.discard();
            iter2.next();
            iter2.discard();
        } -> [];

        mark_reset_discard_nested: [1,2,3,4,5] -> iter => {
            let mut iter2 = iter.mark();
            iter2.next();
            iter2.next();
            let mut iter3 = iter2.mark();
            iter3.next();
            iter3.next();
            iter3.discard();
            iter2.reset();
        } -> [1,2,3,4,5];

        mark_after_reset: [1,2,3,4,5] -> iter => {
            let mut iter2 = iter.mark();
            iter2.next();
            iter2.reset();
            iter.next();
            let mut iter3 = iter.mark();
            iter3.next();
            iter3.reset();
        } -> [2,3,4,5];

        reset_then_next_past: [1,2,3,4,5] -> iter => {
            let mut iter2 = iter.mark();
            iter2.next();
            iter2.next();
            iter2.reset();
            iter.next();
            iter.next();
            iter.next();
        } -> [4,5];

        multiple_mark_and_reset: [1,2,3,4,5] -> iter => {
            let mut iter2 = iter.mark();
            iter2.next();
            let mut iter3 = iter2.mark();
            iter3.next();
            iter3.next();
            iter3.reset();
            iter2.next();
            iter2.next();
            iter2.reset();
            iter.next();
        } -> [2,3,4,5];
    }
}
