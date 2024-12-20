use std::collections::VecDeque;


pub fn marking<I>(inner: I) -> impl MarkingIterator<I>
where
    I: Iterator<Item: Copy>,
{
    MarkContainer {
        inner: inner,
        current_read_buffer: None,
        buffers: vec![],
    }
}

pub trait MarkingIterator<I>: Iterator<Item=I::Item> where
    I: Iterator<Item: Copy>,
{
    #[must_use]
    fn mark(&mut self) -> IteratorMark<I>;
}

struct MarkContainer<I>
where
    I: Iterator<Item: Copy>,
{
    inner: I,
    buffers: Vec<VecDeque<I::Item>>,
    current_read_buffer: Option<VecDeque<I::Item>>,
}

impl<I> MarkContainer<I>
where
    I: Iterator<Item: Copy>,
{
    fn create_mark(&mut self) {
        self.buffers.push(VecDeque::new());
    }

    fn reset(&mut self) {
        if let Some(current) = &mut self.current_read_buffer {
            let mut new = self.buffers.pop().expect("no marking to reset to");
            new.append(current);
            self.current_read_buffer = Some(new);
        } else {
            self.current_read_buffer = Some(self.buffers.pop().expect("no marking to reset to"));
        }
    }

    fn discard(&mut self) {
        let mut top = self.buffers.pop().expect("no marking to discard");
        if let Some(new_top) = self.buffers.last_mut() {
            new_top.append(&mut top);
        }
    }
}

impl<I> Iterator for MarkContainer<I>
where
    I: Iterator<Item: Copy>,
{
    type Item = I::Item;

    fn next(&mut self) -> Option<Self::Item> {
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
}

impl<I> MarkingIterator<I> for MarkContainer<I>
where
    I: Iterator<Item: Copy>,
{
    fn mark(&mut self) -> IteratorMark<I> {
        self.create_mark();
        IteratorMark { iter: self, used_up: false, }
    }
}

pub struct IteratorMark<'a, I>
where
    I: Iterator<Item: Copy>,
{
    iter: &'a mut MarkContainer<I>,
    used_up: bool,
}

impl<I> IteratorMark<'_, I>
where
    I: Iterator<Item: Copy>,
{
    pub fn reset(mut self) {
        self.iter.reset();
        self.used_up = true;
    }

    pub fn discard(mut self) {
        self.iter.discard();
        self.used_up = true;
    }
}

impl<I> MarkingIterator<I> for IteratorMark<'_, I>
where
    I: Iterator<Item: Copy>,
{
    fn mark(&mut self) -> IteratorMark<I> {
        self.iter.create_mark();
        IteratorMark { iter: self.iter, used_up: false, }
    }
}

impl<I> Iterator for IteratorMark<'_, I>
where
    I: Iterator<Item: Copy>,
{
    type Item = I::Item;

    fn next(&mut self) -> Option<Self::Item> {
        return self.iter.next();
    }
}

impl<I> Drop for IteratorMark<'_, I>
where
    I: Iterator<Item: Copy>, {
    fn drop(&mut self) {
        if self.used_up {
            return;
        }
        self.iter.discard();
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use paste::paste;

    #[test]
    fn test_mark_drop() {
        // arrange
        let source = vec![1, 2, 3, 4, 5];
        let mut iter = marking(source.into_iter());

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
    fn test_mark_discard() {
        // arrange
        let source = vec![1, 2, 3, 4, 5];
        let mut iter = marking(source.into_iter());

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
    fn test_mark_reset() {
        // arrange
        let source = vec![1, 2, 3, 4, 5];
        let mut iter = marking(source.into_iter());

        // act
        let mut mark = iter.mark();
        mark.next();
        mark.next();

        mark.reset();

        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(remaining, vec![1, 2, 3, 4, 5]);
    }

    macro_rules! mark_iter_test_step {
        ($iter:ident, next) => {
            {
                $iter.next();
                $iter
            }
        };
        ($iter:ident, mark) => {
            $iter.mark()
        };
        ($iter:ident, reset) => {
            $iter.reset()

        };
        ($iter:ident, discard) => {
            $iter.discard()
        };
    }

    macro_rules! mark_iter_test_helper {
        ($name:ident: [$($input:expr),*] -> [$($step:ident),*] -> panic) => {
        paste! {
            #[test]
            #[should_panic]
            fn [<test_ $name >] () {
                // arrange
                let source = vec![$($input,)*];
                let mut iter = marking(source.into_iter());


                // act
                $(
                    let mut iter = mark_iter_test_step!(iter, $step);
                )*
            }
        }
        };
        ($name:ident: [$($input:expr),*] -> $iter:ident => $steps:tt -> [$($expected:expr),*]) => {
        paste! {
            #[test]
            fn [<test_ $name >] () {
                // arrange
                let source = vec![$($input,)*];
                let mut $iter = marking(source.into_iter());

                // act
                $steps;

                let remaining: Vec<_> = $iter.collect();

                // assert
                assert_eq!(remaining, vec![$($expected,)*]);
            }
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

    #[test]
    fn test_mark_and_reset_returns_jumps_to_mark2() {
        let source = vec![1, 2, 3, 4, 5];
        let mut orig_iter = marking(source.into_iter());
        let iter = &mut orig_iter;


        iter.next();
        let mut iter = iter.mark();
        iter.next();
        iter.reset();

        let remaining: Vec<_> = orig_iter.collect();


        assert_eq!(remaining, vec![2,3,4,5]);

    }

    mark_iter_tests! {
        wraps_internal_iter: [1,2,3,4,5] -> iter => {} -> [1,2,3,4,5];

        strings: ["foo", "bar", "foobar", "fuzz"] -> iter => {} -> ["foo", "bar", "foobar", "fuzz"];

        mark_has_no_direct_effect: [1,2,3,4,5] -> iter => {
            iter.next();
            let mut iter2 = iter.mark();
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


    //     reset_without_mark: [1,2,3,4,5] -> [reset] -> panic;
    //     discard_without_mark: [1,2,3,4,5] -> [discard] -> panic;
    //     reset_too_often: [1,2,3,4,5] -> [marking, reset, reset] -> panic;
    //     discard_too_often: [1,2,3,4,5] -> [marking, discard, discard] -> panic;
    //     discard_after_reset_too_often: [1,2,3,4,5] -> iter => {
    //         let mut marking = iter.marking();
    //         marking.reset();
    //         iter.discard();
    //         iter
    //     } -> panic;
    }
}
