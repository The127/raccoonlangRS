use std::collections::{Bound, HashMap};
use std::error::Error;
use std::fmt::{Debug, Formatter};
use std::fs;
use std::cmp::{max, min};
use std::ops::{Add, AddAssign, Range, RangeBounds};
use std::path::{Path, PathBuf};
use icu::normalizer::ComposingNormalizer;
use icu::segmenter::GraphemeClusterSegmenter;
use ustr::{ustr, Ustr};

#[derive(Debug)]
pub struct SourceCollection {
    sources: Vec<Source>,
    loaded_source_indexes: HashMap<PathBuf, usize>,
}

const NFC_NORMALIZER: ComposingNormalizer = ComposingNormalizer::new_nfc();

impl SourceCollection {
    pub fn new() -> Self {
        SourceCollection {
            sources: vec![],
            loaded_source_indexes: HashMap::new(),
        }
    }

    pub fn get_identifier<T : Into<Span>>(&self, span: T) -> Ustr {
        let str = self.get_str(span);

        ustr(NFC_NORMALIZER.normalize(str).as_str())
    }

    pub fn get_str<T : Into<Span>>(&self, span: T) -> &str {
        let span: Span = span.into();

        for source in &self.sources {
            if source.span_.end() < span.end() {
                continue;
            }

            let start_grapheme_idx = span.start() - source.span_.start();
            let end_grapheme_idx = span.end() - source.span_.start();

            let source_start = source.grapheme_breakpoints[start_grapheme_idx];

            let source_end = source.grapheme_breakpoints[end_grapheme_idx];
            return &source.content[source_start..source_end];
        }

        if self.sources.is_empty() {
            panic!("Trying to access data {:?} in empty source map.", span);
        } else {
            panic!("Trying to access data {:?} outside the source map {:?}.", span, Span(
                self.sources.first().unwrap().span_.start(),
                self.sources.last().unwrap().span_.end()
            ));
        }
    }

    pub fn load<P: AsRef<Path>>(&mut self, path: P) -> Result<Span, Box<dyn Error>> {
        let canonical_path = fs::canonicalize(&path)?;
        if let Some(index) = self.loaded_source_indexes.get(&canonical_path) {
            return Ok(self.sources[*index].span_);
        }

        let content = fs::read_to_string(canonical_path.clone())?;
        let span = self.load_internal(path.as_ref().to_str().unwrap(), content);

        self.loaded_source_indexes
            .insert(canonical_path, self.sources.len() - 1);

        Ok(span)
    }

    pub fn load_content<T: Into<String>>(&mut self, content: T) -> Span {
        self.load_internal("<unknown>", content.into())
    }

    fn load_internal(&mut self, name: &str, content_str: String) -> Span {
        let grapheme_segmenter = GraphemeClusterSegmenter::new();
        let grapheme_breakpoints: Vec<usize> = grapheme_segmenter.segment_str(&content_str).collect();

        let mut linebreaks = vec![];
        for i in 0..(grapheme_breakpoints.len() - 1) {
            let start = grapheme_breakpoints[i];
            let end = grapheme_breakpoints[i+1];
            let current = &content_str[start..end];
            if current == "\n" || current == "\r" || current == "\r\n" {
                linebreaks.push(i);
            }
        }

        let last_span_end = self.sources.last().span().end();

        let span = Span(
            last_span_end,
            last_span_end + grapheme_breakpoints.len() - 1
        );

        let source = Source {
            name: ustr(name),
            span_: span,
            content: content_str,
            grapheme_breakpoints: grapheme_breakpoints,
            linebreaks: linebreaks,
        };

        self.sources.push(source);

        span
    }

    pub fn get_location(&self, loc: usize) -> SourceLocation {
        for source in &self.sources {
            if source.span_.end() < loc {
                continue;
            }

            let offset = loc - source.span_.start();

            /*

            qux\nbar => [3]
            q => 0 => err(0) => (1,1)
            u => 1 => err(0) => (1,2)
            x => 2 => err(0) => (1,3)
            x| => 3 => ok(0) => (1,4)
            b => 4 => err(1) => (2,1)
            a => 5 => err(1) => (2,2)
            r => 6 => err(1) => (2,3)
            r| => 7 => err(1) => (2,4)


             */

            let (line, column) = match source.linebreaks.binary_search(&offset) {
                Err(0)|Ok(0) => (1, offset + 1),
                Ok(line)|Err(line) => (line + 1, offset - source.linebreaks[line-1]),
            };

            return SourceLocation {
                file: source.name,
                line: line,
                column: column,
            };
        }

        if self.sources.is_empty() {
            panic!("Trying to get location {:?} in empty source map.", loc);
        } else {
            panic!("Trying to get location {:?} outside the source map {:?}.", loc, Span(
                self.sources.first().unwrap().span_.start(),
                self.sources.last().unwrap().span_.end()
            ));
        }
    }
}

#[derive(Debug)]
pub struct Source {
    name: Ustr,
    span_: Span,
    content: String,
    grapheme_breakpoints: Vec<usize>,
    linebreaks: Vec<usize>,
}

impl HasSpan for Source {
    fn span(&self) -> Span {
        self.span_
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct SourceLocation {
    pub file: Ustr,
    pub line: usize,
    pub column: usize
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Default)]
pub struct Span(pub usize, pub usize);

impl Span {
    pub fn empty() -> Self {
        Self(0,0)
    }

    pub fn start(&self) -> usize {
        self.0
    }

    pub fn end(&self) -> usize {
        self.1
    }

    pub fn is_empty(&self) -> bool {
        self.start() == self.end()
    }

    pub fn sub(&self, sub: impl RangeBounds<i32>) -> Self {
        let offset_start = sub.start_bound();
        let offset_end = sub.end_bound();

        let start = match offset_start {
            Bound::Unbounded => self.0,
            Bound::Included(x) if *x >= 0 => self.0 + (*x as usize),
            Bound::Included(x) => self.1 - (-*x as usize),
            Bound::Excluded(x) => panic!("unsupported"),
        };

        let end = match offset_end {
            Bound::Unbounded => self.1,
            Bound::Excluded(x) if *x > 0 => self.0 + (*x as usize),
            Bound::Excluded(x) => self.1 - (-*x as usize),
            Bound::Included(_) => panic!("unsupported"),
        };

        assert!(start >= self.0);
        assert!(end <= self.1);

        Self(start, end)
    }
}

impl Add for Span {
    type Output = Span;

    fn add(self, rhs: Self) -> Self::Output {
        if rhs.is_empty() {
            return self
        }
        if self.is_empty() {
            return rhs
        }
        Span(
            min(self.start(), rhs.start()),
            max(self.end(), rhs.end())
        )
    }
}

impl AddAssign for Span {
    fn add_assign(&mut self, rhs: Self) {
        *self = *self + rhs;
    }
}


impl Debug for Span{
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}..{}", self.start(), self.end())
    }
}

impl Into<Range<usize>> for Span {
    fn into(self) -> Range<usize> {
        self.start()..self.end()
    }
}

impl From<Range<usize>> for Span {
    fn from(value: Range<usize>) -> Self {
        Span (
            min(value.start, value.end),
            max(value.start, value.end),
        )
    }
}

impl From<usize> for Span {
    fn from(value: usize) -> Self {
        Span(value, value + 1)
    }
}

pub trait HasSpan {
    fn span(&self) -> Span;
}

impl<T: HasSpan> HasSpan for &T {
    fn span(&self) -> Span {
        (*self).span()
    }
}

impl<T: HasSpan> HasSpan for Box<T> {
    fn span(&self) -> Span {
        self.as_ref().span()
    }
}

impl<T: HasSpan> HasSpan for Option<T> {
    fn span(&self) -> Span {
        // if there is nothing then it's an empty span, otherwise the span of the thing
        match self {
            None => Span::empty(),
            Some(x) => x.span(),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::source_map::*;
    use std::fs::File;
    use std::io::Write;
    use std::path::PathBuf;
    use parameterized::parameterized;
    use tempdir::TempDir;

    struct TestContext {
        tmp_dir: TempDir,
    }

    impl TestContext {
        fn new() -> Self {
            let tmp_dir = TempDir::new("").unwrap();

            {
                let mut tmp_file = File::create(tmp_dir.path().join("test.racc")).unwrap();
                write!(tmp_file, "1234567890").unwrap();
            }

            {
                let mut tmp_file = File::create(tmp_dir.path().join("test2.racc")).unwrap();
                write!(tmp_file, "abcdefghij").unwrap();
            }

            {
                let mut tmp_file = File::create(tmp_dir.path().join("test3.racc")).unwrap();
                write!(tmp_file, "qrstuvwxyz").unwrap();
            }

            {
                let mut tmp_file = File::create(tmp_dir.path().join("emoji.racc")).unwrap();
                write!(tmp_file, "🔥😂😊😁🙏😎💪😋😇🎉").unwrap();
            }

            {
                let mut tmp_file = File::create(tmp_dir.path().join("graphemes.racc")).unwrap();
                write!(tmp_file, "🤷🏽‍♀️👷‍♀️👯‍♂️").unwrap();
            }

            {
                let mut tmp_file = File::create(tmp_dir.path().join("languages.racc")).unwrap();
                write!(tmp_file, "नमस्ते").unwrap();
            }

            {
                let mut tmp_file = File::create(tmp_dir.path().join("multiline.racc")).unwrap();
                write!(tmp_file, "foo\r\nbar\nhello world").unwrap();
            }

            TestContext { tmp_dir: tmp_dir }
        }

        fn get_file_path(&self, file_name: &str) -> PathBuf {
            self.tmp_dir.path().join(file_name)
        }
    }

    #[test]
    fn load_first_file() {
        // arrange
        let ctx = TestContext::new();
        let mut source_collection = SourceCollection::new();

        // act
        let span = source_collection
            .load(ctx.get_file_path("test.racc"))
            .unwrap();

        // assert
        assert_eq!(span, Span(0, 10));
    }

    #[test]
    fn load_second_file() {
        // arrange
        let ctx = TestContext::new();
        let mut source_collection = SourceCollection::new();

        // act
        _ = source_collection
            .load(ctx.get_file_path("test.racc"))
            .unwrap();
        let span = source_collection
            .load(ctx.get_file_path("test2.racc"))
            .unwrap();

        // assert

        assert_eq!(span, Span(10, 20));
    }

    #[test]
    fn load_file_twice() {
        // arrange
        let ctx = TestContext::new();
        let mut source_collection = SourceCollection::new();

        // act
        let span1 = source_collection
            .load(ctx.get_file_path("test.racc"))
            .unwrap();
        let span2 = source_collection
            .load(ctx.get_file_path("test.racc"))
            .unwrap();

        // assert

        assert_eq!(span1, span2);
    }

    #[test]
    #[should_panic]
    fn load_file_doesnt_exist() {
        // arrange
        let ctx = TestContext::new();
        let mut source_collection = SourceCollection::new();

        // act
        _ = source_collection
            .load(ctx.get_file_path("not_found.racc"))
            .unwrap();
    }

    #[test]
    #[should_panic]
    fn get_no_sources() {
        // arrange
        let source_collection = SourceCollection::new();

        // act
        _ = source_collection.get_str(Span::empty());
    }

    #[test]
    fn get_empty_span() {
        // arrange
        let ctx = TestContext::new();
        let mut source_collection = SourceCollection::new();
        _ = source_collection
            .load(ctx.get_file_path("test.racc"))
            .unwrap();

        // act
        let result = source_collection.get_str(Span::empty());

        // assert
        assert_eq!(result, "");
    }

    #[test]
    fn get_size_1_span() {
        // arrange
        let ctx = TestContext::new();
        let mut source_collection = SourceCollection::new();
        _ = source_collection
            .load(ctx.get_file_path("test.racc"))
            .unwrap();

        // act
        let result = source_collection.get_str(0..1);

        // assert
        assert_eq!(result, "1");
    }

    #[test]
    fn get_span_is_file() {
        // arrange
        let ctx = TestContext::new();
        let mut source_collection = SourceCollection::new();
        let span = source_collection
            .load(ctx.get_file_path("test.racc"))
            .unwrap();

        // act
        let result = source_collection.get_str(span);

        // assert
        assert_eq!(result, "1234567890");
    }

    #[test]
    fn get_span_is_file_with_multiple_files() {
        // arrange
        let ctx = TestContext::new();
        let mut source_collection = SourceCollection::new();
        _ = source_collection
            .load(ctx.get_file_path("test.racc"))
            .unwrap();
        let span = source_collection
            .load(ctx.get_file_path("test2.racc"))
            .unwrap();

        // act
        let result = source_collection.get_str(span);

        // assert
        assert_eq!(result, "abcdefghij");
    }

    #[test]
    #[should_panic]
    fn get_span_is_out_of_file_bounds() {
        // arrange
        let ctx = TestContext::new();
        let mut source_collection = SourceCollection::new();
        _ = source_collection
            .load(ctx.get_file_path("test.racc"))
            .unwrap();
        let span = source_collection
            .load(ctx.get_file_path("test2.racc"))
            .unwrap();
        let oob_span = Span(span.start() - 1, span.end());

        // act
        _ = source_collection.get_str(oob_span);
    }

    #[test]
    #[should_panic]
    fn get_span_is_out_of_bounds() {
        // arrange
        let ctx = TestContext::new();
        let mut source_collection = SourceCollection::new();
        _ = source_collection
            .load(ctx.get_file_path("test.racc"))
            .unwrap();
        let span = source_collection
            .load(ctx.get_file_path("test2.racc"))
            .unwrap();
        let oob_span = Span(span.start(), span.end() + 1);

        // act
        _ = source_collection.get_str(oob_span);
    }

    #[test]
    fn get_span_is_subset_1() {
        // arrange
        let ctx = TestContext::new();
        let mut source_collection = SourceCollection::new();
        let span = source_collection
            .load(ctx.get_file_path("test.racc"))
            .unwrap();
        _ = source_collection
            .load(ctx.get_file_path("test2.racc"))
            .unwrap();
        _ = source_collection
            .load(ctx.get_file_path("test3.racc"))
            .unwrap();

        let subset_span = span.sub(1..-1);

        // act
        let result = source_collection.get_str(subset_span);

        // assert
        assert_eq!(result, "23456789");
    }

    #[test]
    fn get_span_is_subset_2() {
        // arrange
        let ctx = TestContext::new();
        let mut source_collection = SourceCollection::new();
        _ = source_collection
            .load(ctx.get_file_path("test.racc"))
            .unwrap();
        let span = source_collection
            .load(ctx.get_file_path("test2.racc"))
            .unwrap();
        _ = source_collection
            .load(ctx.get_file_path("test3.racc"))
            .unwrap();

        let subset_span = span.sub(1..-1);

        // act
        let result = source_collection.get_str(subset_span);

        // assert
        assert_eq!(result, "bcdefghi");
    }

    #[test]
    fn get_span_is_subset_3() {
        // arrange
        let ctx = TestContext::new();
        let mut source_collection = SourceCollection::new();
        _ = source_collection
            .load(ctx.get_file_path("test.racc"))
            .unwrap();
        _ = source_collection
            .load(ctx.get_file_path("test2.racc"))
            .unwrap();
        let span = source_collection
            .load(ctx.get_file_path("test3.racc"))
            .unwrap();

        let subset_span = span.sub(1..-1);

        // act
        let result = source_collection.get_str(subset_span);

        // assert
        assert_eq!(result, "rstuvwxy");
    }

    #[test]
    #[should_panic]
    fn get_span_out_of_range() {
        // arrange
        let ctx = TestContext::new();
        let mut source_collection = SourceCollection::new();
        _ = source_collection
            .load(ctx.get_file_path("test.racc"))
            .unwrap();
        _ = source_collection
            .load(ctx.get_file_path("test2.racc"))
            .unwrap();
        let span = source_collection
            .load(ctx.get_file_path("test3.racc"))
            .unwrap();

        let oob_span = Span(span.start(), span.end() + 1);

        // act
        let _ = source_collection.get_str(oob_span);
    }

    #[test]
    #[should_panic]
    fn get_span_empty_source_collection() {
        // arrange
        let source_collection = SourceCollection::new();

        // act
        let _ = source_collection.get_str(1);
    }

    #[test]
    fn get_emojis() {
        // arrange
        let ctx = TestContext::new();
        let mut source_collection = SourceCollection::new();
        _ = source_collection
            .load(ctx.get_file_path("emoji.racc"))
            .unwrap();

        // act
        let result = source_collection.get_str(2..4);

        // assert
        assert_eq!(result, "😊😁");
    }

    #[test]
    fn get_graphemes() {
        // arrange
        let ctx = TestContext::new();
        let mut source_collection = SourceCollection::new();
        _ = source_collection
            .load(ctx.get_file_path("graphemes.racc"))
            .unwrap();

        // act
        let result = source_collection.get_str(1..2);

        // assert
        assert_eq!(result, "👷‍♀️");
    }

    #[test]
    fn get_languages() {
        // arrange
        let ctx = TestContext::new();
        let mut source_collection = SourceCollection::new();
        _ = source_collection
            .load(ctx.get_file_path("languages.racc"))
            .unwrap();

        // act
        let result = source_collection.get_str(1..2);

        // assert
        assert_eq!(result, "म");
    }

    #[test]
    fn get_location_line_middle() {
        // arrange
        let ctx = TestContext::new();
        let mut source_collection = SourceCollection::new();
        let file_path = ctx.get_file_path("multiline.racc");
        let span = source_collection.load(&file_path).unwrap();
        let pos = span.start() + 5;

        // act
        let result = source_collection.get_location(pos);

        // assert
        assert_eq!(result, SourceLocation {
            file: ustr(file_path.to_str().unwrap()),
            line: 2,
            column: 2,
        });
    }

    #[test]
    fn get_location_line_start() {
        // arrange
        let ctx = TestContext::new();
        let mut source_collection = SourceCollection::new();
        let file_path = ctx.get_file_path("multiline.racc");
        let span = source_collection.load(&file_path).unwrap();
        let pos = span.start() + 4;

        // act
        let result = source_collection.get_location(pos);

        // assert
        assert_eq!(result, SourceLocation {
            file: ustr(file_path.to_str().unwrap()),
            line: 2,
            column: 1,
        });
    }

    #[test]
    fn get_location_line_end() {
        // arrange
        let ctx = TestContext::new();
        let mut source_collection = SourceCollection::new();
        let file_path = ctx.get_file_path("multiline.racc");
        let span = source_collection.load(&file_path).unwrap();
        let pos = span.start() + 7;

        // act
        let result = source_collection.get_location(pos);

        // assert
        assert_eq!(result, SourceLocation {
            file: ustr(file_path.to_str().unwrap()),
            line: 2,
            column: 4,
        });
    }

    #[test]
    fn get_location_file_start() {
        // arrange
        let ctx = TestContext::new();
        let mut source_collection = SourceCollection::new();
        let file_path = ctx.get_file_path("multiline.racc");
        let span = source_collection.load(&file_path).unwrap();
        let pos = span.start();

        // act
        let result = source_collection.get_location(pos);

        // assert
        assert_eq!(result, SourceLocation {
            file: ustr(file_path.to_str().unwrap()),
            line: 1,
            column: 1,
        });
    }

    #[test]
    fn get_location_file_end() {
        // arrange
        let ctx = TestContext::new();
        let mut source_collection = SourceCollection::new();
        let file_path = ctx.get_file_path("multiline.racc");
        let span = source_collection.load(&file_path).unwrap();
        let pos = span.end();

        // act
        let result = source_collection.get_location(pos);

        // assert
        assert_eq!(result, SourceLocation {
            file: ustr(file_path.to_str().unwrap()),
            line: 3,
            column: 12,
        });
    }

    #[test]
    fn span_to_range() {
        // arrange
        let span = Span(5, 10);

        // act
        let range: Range<usize> = span.into();

        // assert
        assert_eq!(range, 5..10);
    }

    #[test]
    fn range_to_span() {
        // arrange
        let range = 5..10;

        // act
        let span: Span = range.into();

        // assert
        assert_eq!(span, Span(5, 10));
    }

    #[test]
    fn range_to_span_reverse() {
        // arrange
        let range = 10..5;

        // act
        let span: Span = range.into();

        // assert
        assert_eq!(span, Span(5, 10));
    }

    #[test]
    fn span_add_nonoverlapping() {
        // arrange
        let span1: Span = (1..5).into();
        let span2: Span = (7..10).into();

        // act
        let result = span1 + span2;

        // assert
        assert_eq!(result, (1..10).into());
    }

    #[test]
    fn span_add_touching() {
        // arrange
        let span1: Span = (1..5).into();
        let span2: Span = (5..10).into();

        // act
        let result = span1 + span2;

        // assert
        assert_eq!(result, (1..10).into());
    }

    #[test]
    fn span_add_overlapping() {
        // arrange
        let span1: Span = (1..7).into();
        let span2: Span = (5..10).into();

        // act
        let result = span1 + span2;

        // assert
        assert_eq!(result, (1..10).into());
    }

    #[test]
    fn span_add_contained() {
        // arrange
        let span1: Span = (1..10).into();
        let span2: Span = (5..7).into();

        // act
        let result = span1 + span2;

        // assert
        assert_eq!(result, (1..10).into());
    }

    #[test]
    fn span_add_equal() {
        // arrange
        let span1: Span = (1..10).into();
        let span2: Span = (1..10).into();

        // act
        let result = span1 + span2;

        // assert
        assert_eq!(result, (1..10).into());
    }

    #[test]
    fn span_add_reverse_order() {
        // arrange
        let span1: Span = (1..7).into();
        let span2: Span = (5..10).into();

        // act
        let result = span2 + span1;

        // assert
        assert_eq!(result, (1..10).into());
    }

    #[test]
    fn span_add_empty() {
        // arrange
        let span1: Span = (1..7).into();
        let span2: Span = Span::empty();

        // act
        let result = span1 + span2;

        // assert
        assert_eq!(result, span1);
    }

    #[test]
    fn span_add_empty_nonzero() {
        // arrange
        let span1: Span = (1..7).into();
        let span2: Span = (9..9).into();

        // act
        let result = span1 + span2;

        // assert
        assert_eq!(result, span1);
    }

    #[test]
    fn span_add_empty_reverse() {
        // arrange
        let span1: Span = (1..7).into();
        let span2: Span = Span::empty();

        // act
        let result = span2 + span1;

        // assert
        assert_eq!(result, span1);
    }

    #[test]
    fn span_sub() {
        // arrange
        let span = Span(3, 17);

        // act
        let result = span.sub(2..5);

        // assert
        assert_eq!(result, Span(5, 8));
    }

    #[test]
    fn span_sub_open_end() {
        // arrange
        let span = Span(3, 17);

        // act
        let result = span.sub(2..);

        // assert
        assert_eq!(result, Span(5, 17));
    }

    #[test]
    fn span_sub_open_start() {
        // arrange
        let span = Span(3, 17);

        // act
        let result = span.sub(..5);

        // assert
        assert_eq!(result, Span(3, 8));
    }

    #[test]
    fn span_sub_open_both() {
        // arrange
        let span = Span(3, 17);

        // act
        let result = span.sub(..);

        // assert
        assert_eq!(result, span);
    }

    #[test]
    fn span_sub_negative_start() {
        // arrange
        let span = Span(3, 17);

        // act
        let result = span.sub(-10..12);

        // assert
        assert_eq!(result, Span(7, 15));
    }

    #[test]
    fn span_sub_negative_end() {
        // arrange
        let span = Span(3, 17);

        // act
        let result = span.sub(4..-4);

        // assert
        assert_eq!(result, Span(7, 13));
    }

    #[test]
    fn span_sub_negative_both() {
        // arrange
        let span = Span(3, 17);

        // act
        let result = span.sub(-10..-3);

        //assert
        assert_eq!(result, Span(7, 14));
    }

    #[parameterized(values = {
        ("foo", "foo"),
        ("Pin\u{0303}a", "Pi\u{00F1}a"),
        ("\u{1e69}", "s\u{0323}\u{0307}"),
        ("\u{1e69}", "s\u{0307}\u{0323}"),
        ("\u{1e63}\u{0307}", "s\u{0307}\u{0323}"),
        ("가", "\u{1100}\u{1161}")
    })]
    fn get_identifier_normalization(values: (&str, &str)) {
        // arrange
        let (str1, str2) = values;
        let mut sources = SourceCollection::new();
        let span1 = sources.load_content(str1);
        let span2 = sources.load_content(str2);

        // act
        let ident1 = sources.get_identifier(span1);
        let ident2 = sources.get_identifier(span2);

        // assert
        assert_eq!(ident1, ident2);
    }

    #[parameterized(values = {
        ("ſ", "s"),
        ("①", "1"),
        ("⁹", "9"),
        ("\u{01c6}", "dž"),
        ("㌀", "アパート"),
    })]
    fn get_identifier_no_compatibility_normalization(values: (&str, &str)) {
        // arrange
        let (str1, str2) = values;
        let mut sources = SourceCollection::new();
        let span1 = sources.load_content(str1);
        let span2 = sources.load_content(str2);

        // act
        let ident1 =sources.get_identifier(span1);
        let ident2 =sources.get_identifier(span2);

        // assert
        assert_ne!(ident1, ident2);
    }
}
