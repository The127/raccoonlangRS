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
    use paste::paste;

    macro_rules! mark_iter_tests {
        ($($name:ident: $input:tt -> [$($step:ident),*] -> $expected:tt;)*) => {
        $(
        paste! {
            #[test]
            fn [<test_ $name >] () {
                // arrange
                let source = vec! $input;
                let mut iter = mark(source.clone().into_iter());

                // act
                $(
                    iter.$step();
                )*

                let remaining: Vec<_> = iter.collect();

                // assert
                assert_eq!(remaining, vec! $expected);
            }
        }
        )*
        }
    }

    mark_iter_tests!{
        wraps_internal_iter: [1, 2, 3, 4, 5] -> [] -> [1, 2, 3, 4, 5];
        strings: ["foo", "bar", "foobar", "fuzz"] -> [] -> ["foo", "bar", "foobar", "fuzz"];
        mark_has_no_direct_effect: [1,2,3,4,5] -> [next, mark] -> [2,3,4,5];
        mark_and_reset_returns_jumps_to_mark: [1,2,3,4,5] -> [next, mark, next, reset] -> [2, 3, 4, 5];
        mark_and_reset_returns_jumps_to_start: [1,2,3,4,5] -> [mark, next, next, reset] -> [1, 2, 3, 4, 5];
        mark_and_discard_continues: [1,2,3,4,5] -> [next, mark, next, discard] -> [3, 4, 5];
        mark_and_discard_remarks: [1,2,3,4,5] -> [next, mark, next, discard, next, mark, next, reset] -> [4, 5];
        mark_and_reset_nested: [1,2,3,4,5] -> [next, mark, next, mark, next, next, reset, next, reset] -> [2,3,4,5];
        mark_and_reset_nested_unbalanced: [1,2,3,4,5] -> [next, mark, next, mark, next, next, reset] -> [3,4,5];
    }
}
