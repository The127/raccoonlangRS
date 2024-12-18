use std::collections::VecDeque;

pub struct MarkIterator<I>
where
    I: Iterator<Item: Copy>,
{
    inner: I,
    buffers: Vec<VecDeque<I::Item>>,
    current_read_buffer: Option<VecDeque<I::Item>>,
}

impl<I> MarkIterator<I>
where
    I: Iterator<Item: Copy>,
{
    pub fn mark(&mut self) {
        self.buffers.push(VecDeque::new());
    }

    pub fn reset(&mut self) {
        if let Some(current) = &mut self.current_read_buffer {
            let mut new = self.buffers.pop().expect("no mark to reset to");
            new.append(current);
            self.current_read_buffer = Some(new);
        } else {
            self.current_read_buffer = Some(self.buffers.pop().expect("no mark to reset to"));
        }
    }

    pub fn discard(&mut self) {
        self.buffers.pop().expect("no mark to discard");
    }
}

pub fn mark<I>(inner: I) -> MarkIterator<I>
where
    I: Iterator<Item: Copy>,
{
    MarkIterator {
        inner: inner,
        current_read_buffer: None,
        buffers: vec![],
    }
}

impl<I> Iterator for MarkIterator<I>
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

#[cfg(test)]
mod test {
    use super::*;
    use paste::paste;


    macro_rules! mark_iter_test_helper {
        ($name:ident: [$($input:expr),*] -> [$($step:ident),*] -> panic) => {
        paste! {
            #[test]
            #[should_panic]
            fn [<test_ $name >] () {
                // arrange
                let source = vec![$($input,)*];
                let mut iter = mark(source.clone().into_iter());

                // act
                $(
                    iter.$step();
                )*
            }
        }
        };
        ($name:ident: [$($input:expr),*] -> [$($step:ident),*] -> [$($expected:expr),*]) => {
        paste! {
            #[test]
            fn [<test_ $name >] () {
                // arrange
                let source = vec![$($input,)*];
                let mut iter = mark(source.clone().into_iter());

                // act
                $(
                    iter.$step();
                )*

                let remaining: Vec<_> = iter.collect();

                // assert
                assert_eq!(remaining, vec![$($expected,)*]);
            }
        }
        };
    }
    macro_rules! mark_iter_tests {
        ($($name:ident: $input:tt -> $steps:tt -> $result:tt;)*) => {
            $(
                mark_iter_test_helper! { $name: $input -> $steps -> $result }
            )*
        }
    }

    mark_iter_tests! {
        wraps_internal_iter: [1,2,3,4,5] -> [] -> [1,2,3,4,5];
        strings: ["foo", "bar", "foobar", "fuzz"] -> [] -> ["foo", "bar", "foobar", "fuzz"];
        mark_has_no_direct_effect: [1,2,3,4,5] -> [next, mark] -> [2,3,4,5];
        mark_and_reset_returns_jumps_to_mark: [1,2,3,4,5] -> [next, mark, next, reset] -> [2,3,4,5];
        mark_and_reset_returns_jumps_to_start: [1,2,3,4,5] -> [mark, next, next, reset] -> [1,2,3,4,5];
        mark_and_discard_continues: [1,2,3,4,5] -> [next, mark, next, discard] -> [3,4,5];
        mark_and_discard_remarks: [1,2,3,4,5] -> [next, mark, next, discard, next, mark, next, reset] -> [4,5];
        mark_and_discard_and_next: [1,2,3,4,5] -> [next, mark, next, discard, next] -> [4,5];
        mark_and_reset_nested: [1,2,3,4,5] -> [next, mark, next, mark, next, next, reset, next, reset] -> [2,3,4,5];
        mark_and_reset_nested_unbalanced: [1,2,3,4,5] -> [next, mark, next, mark, next, next, reset] -> [3,4,5];
        mark_and_reset_nested_middle: [1,2,3,4,5] -> [next, mark, next, mark, next, mark, next, reset, next, reset] -> [3,4,5];
        mark_and_discard_nested: [1,2,3,4,5] -> [next, mark, next, mark, next, next, discard, next, discard] -> [];
        mark_and_discard_nested_unbalanced: [1,2,3,4,5] -> [next, mark, next, mark, next, next, discard] -> [5];
        mark_after_reset: [1,2,3,4,5] -> [mark, next, mark, next, reset, next, mark, next, reset] -> [3,4,5];
        reset_then_next_past: [1,2,3,4,5] -> [mark, next, next, reset, next, next, next] -> [4,5];
        multiple_mark_and_reset: [1,2,3,4,5] -> [mark, next, mark, next, next, reset, next, next, reset, next] -> [2,3,4,5];
        reset_without_mark: [1,2,3,4,5] -> [reset] -> panic;
        discard_without_mark: [1,2,3,4,5] -> [discard] -> panic;
        reset_too_often: [1,2,3,4,5] -> [mark, reset, reset] -> panic;
        discard_too_often: [1,2,3,4,5] -> [mark, discard, discard] -> panic;
        discard_after_reset_too_often: [1,2,3,4,5] -> [mark, reset, discard] -> panic;
    }
}
