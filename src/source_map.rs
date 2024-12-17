use std::collections::HashMap;
use std::error::Error;
use std::fs;
use std::ops::Range;
use std::path::{Path, PathBuf};
use icu::segmenter::GraphemeClusterSegmenter;

pub struct SourceCollection {
    sources: Vec<Source>,
    loaded_source_indexes: HashMap<PathBuf, usize>,
}

impl SourceCollection {
    pub fn new() -> Self {
        SourceCollection {
            sources: vec![],
            loaded_source_indexes: HashMap::new(),
        }
    }

    pub fn get<T : Into<Span>>(&self, span: T) -> &str {
        let span: Span = span.into();

        for source in &self.sources {
            if source.span.end < span.end {
                continue;
            }

            let start_grapheme_idx = span.start - source.span.start;
            let end_grapheme_idx = span.end - source.span.start;

            let source_start = source.grapheme_breakpoints[start_grapheme_idx];

            let source_end = source.grapheme_breakpoints[end_grapheme_idx];
            return &source.content[source_start..source_end];
        }

        panic!("Trying to access data {:?} outside the source map {:?}.", span, Span{
            start: 0,
            end: self.sources.last().unwrap().grapheme_breakpoints.last().unwrap() + 1
        })
    }

    pub fn load<P: AsRef<Path>>(&mut self, path: P) -> Result<Span, Box<dyn Error>> {
        let canonical_path = fs::canonicalize(path)?;
        if let Some(index) = self.loaded_source_indexes.get(&canonical_path) {
            return Ok(self.sources[*index].span);
        }

        let content = fs::read_to_string(canonical_path.clone())?;
        let span = self.load_content(content);

        self.loaded_source_indexes
            .insert(canonical_path, self.sources.len() - 1);

        Ok(span)
    }

    pub fn load_content(&mut self, content: String) -> Span {
        let grapheme_segmenter = GraphemeClusterSegmenter::new();
        let mut grapheme_breakpoints: Vec<usize> = grapheme_segmenter.segment_str(&content).collect();

        let last_span_end = self.sources.last().map(|x| x.span.end).unwrap_or(0);

        let span = Span {
            start: last_span_end,
            end: last_span_end + grapheme_breakpoints.len() - 1,
        };

        let source = Source {
            span: span,
            content: content,
            grapheme_breakpoints: grapheme_breakpoints,
        };

        self.sources.push(source);

        span
    }
}

pub struct Source {
    span: Span,
    content: String,
    grapheme_breakpoints: Vec<usize>,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Into<Range<usize>> for Span {
    fn into(self) -> Range<usize> {
        self.start..self.end
    }
}

impl From<Range<usize>> for Span {
    fn from(value: Range<usize>) -> Self {
        Span {
            start: value.start,
            end: value.end,
        }
    }
}

impl From<usize> for Span {
    fn from(value: usize) -> Self {
        Span {
            start: value,
            end: value + 1,
        }
    }
}

#[cfg(test)]
mod test {
    use crate::source_map::*;
    use std::fs::File;
    use std::io::Write;
    use std::path::PathBuf;
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
        assert_eq!(span, Span { start: 0, end: 10 });
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

        assert_eq!(span, Span { start: 10, end: 20 });
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
        _ = source_collection.get(Span { start: 0, end: 0 });
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
        let result = source_collection.get(Span { start: 0, end: 0 });

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
        let result = source_collection.get(Span { start: 0, end: 1 });

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
        let result = source_collection.get(span);

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
        let result = source_collection.get(span);

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
        let mut span = source_collection
            .load(ctx.get_file_path("test2.racc"))
            .unwrap();
        span.start -= 1;

        // act
        _ = source_collection.get(span);
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
        let mut span = source_collection
            .load(ctx.get_file_path("test2.racc"))
            .unwrap();
        span.end += 1;

        // act
        _ = source_collection.get(span);
    }

    #[test]
    fn get_span_is_subset_1() {
        // arrange
        let ctx = TestContext::new();
        let mut source_collection = SourceCollection::new();
        let mut span = source_collection
            .load(ctx.get_file_path("test.racc"))
            .unwrap();
        _ = source_collection
            .load(ctx.get_file_path("test2.racc"))
            .unwrap();
        _ = source_collection
            .load(ctx.get_file_path("test3.racc"))
            .unwrap();

        span.start += 1;
        span.end -= 1;

        // act
        let result = source_collection.get(span);

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
        let mut span = source_collection
            .load(ctx.get_file_path("test2.racc"))
            .unwrap();
        _ = source_collection
            .load(ctx.get_file_path("test3.racc"))
            .unwrap();

        span.start += 1;
        span.end -= 1;

        // act
        let result = source_collection.get(span);

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
        let mut span = source_collection
            .load(ctx.get_file_path("test3.racc"))
            .unwrap();

        span.start += 1;
        span.end -= 1;

        // act
        let result = source_collection.get(span);

        // assert
        assert_eq!(result, "rstuvwxy");
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
        let result = source_collection.get(2..4);

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
        let result = source_collection.get(1..2);

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
        let result = source_collection.get(1..2);

        // assert
        assert_eq!(result, "म");
    }

    #[test]
    fn span_to_range() {
        // arrange
        let span = Span { start: 5, end: 10 };

        // act
        let range: Range<usize> = span.into();

        // assert
        assert_eq!(range.start, span.start);
        assert_eq!(range.end, span.end);
    }

    #[test]
    fn range_to_span() {
        // arrange
        let range = 5..10;

        // act
        let span: Span = range.clone().into();

        // assert
        assert_eq!(range.start, span.start);
        assert_eq!(range.end, span.end);
    }
}
