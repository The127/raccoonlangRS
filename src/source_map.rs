use std::collections::HashMap;
use std::error::Error;
use std::fs;
use std::path::{Path, PathBuf};
use unicode_segmentation::UnicodeSegmentation;

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

    pub fn get(&self, span: Span) -> &str {
        for source in &self.sources {
            if source.span.end < span.end {
                continue;
            }

            let start_grapheme_idx = span.start - source.span.start;
            let end_grapheme_idx = span.end - source.span.start;

            let source_start = source.graphemes[start_grapheme_idx].position;

            if end_grapheme_idx >= source.graphemes.len() {
                return &source.content[source_start..];
            }

            let source_end = source.graphemes[end_grapheme_idx].position;
            return &source.content[source_start..source_end];
        }

        panic!("trying to access data outside the source map")
    }

    pub fn load<P: AsRef<Path>>(&mut self, path: P) -> Result<Span, Box<dyn Error>> {
        let canonical_path = fs::canonicalize(path)?;
        if let Some(index) = self.loaded_source_indexes.get(&canonical_path) {
            return Ok(self.sources[*index].span);
        }

        let content = fs::read_to_string(canonical_path.clone())?;

        let content_ptr = content.as_ptr() as usize;
        let graphemes = content
            .graphemes(true)
            .map(|x| Grapheme {
                position: (x.as_ptr() as usize) - content_ptr,
            })
            .collect::<Vec<_>>();

        let last_span_end = self.sources.last().map(|x| x.span.end).unwrap_or(0);

        let span = Span {
            start: last_span_end,
            end: last_span_end + graphemes.len(),
        };

        let source = Source {
            span: span,
            content: content,
            graphemes: graphemes,
        };

        self.sources.push(source);
        self.loaded_source_indexes
            .insert(canonical_path, self.sources.len() - 1);

        Ok(span)
    }
}

pub struct Source {
    span: Span,
    content: String,
    graphemes: Vec<Grapheme>,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct Span {
    start: usize,
    end: usize,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct Grapheme {
    position: usize,
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
        let result = source_collection.get(Span { start: 2, end: 4 });

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
        let result = source_collection.get(Span { start: 1, end: 2 });

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
        let result = source_collection.get(Span { start: 1, end: 2 });

        // assert
        assert_eq!(result, "म");
    }
}
