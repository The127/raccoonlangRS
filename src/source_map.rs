use std::collections::HashMap;
use std::error::Error;
use std::fs;
use std::path::{Path, PathBuf};

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

    pub fn get(&self, span: Span) -> &[u8] {
        for source in &self.sources {
            if source.span.end < span.end {
                continue;
            }

            let source_start = span.start - source.span.start;
            let source_end = span.end - source.span.start;

            return &source.content[source_start..source_end];
        }

        panic!("trying to access data outside the source map")
    }

    pub fn load<P: AsRef<Path>>(&mut self, path: P) -> Result<Span, Box<dyn Error>> {
        let canonical_path = fs::canonicalize(path)?;
        if let Some(index) = self.loaded_source_indexes.get(&canonical_path) {
            return Ok(self.sources[*index].span);
        }

        let content = fs::read(canonical_path.clone())?;

        let last_span_end = self.sources.last().map(|x| x.span.end).unwrap_or(0);

        let span = Span {
            start: last_span_end,
            end: last_span_end + content.len(),
        };

        let source = Source {
            span: span,
            content: content,
        };

        self.sources.push(source);
        self.loaded_source_indexes
            .insert(canonical_path, self.sources.len() - 1);

        Ok(span)
    }
}

pub struct Source {
    span: Span,
    content: Vec<u8>,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub struct Span {
    start: usize,
    end: usize,
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
        let bytes = source_collection.get(Span { start: 0, end: 0 });

        //assert
        assert_eq!(bytes, vec![]);
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
        let bytes = source_collection.get(span);

        //assert
        assert_eq!(bytes, "1234567890".as_bytes());
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
        let bytes = source_collection.get(span);

        //assert
        assert_eq!(bytes, "abcdefghij".as_bytes());
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
        let bytes = source_collection.get(span);

        //assert
        assert_eq!(bytes, "23456789".as_bytes());
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
        let bytes = source_collection.get(span);

        //assert
        assert_eq!(bytes, "bcdefghi".as_bytes());
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
        let bytes = source_collection.get(span);

        //assert
        assert_eq!(bytes, "rstuvwxy".as_bytes());
    }
}
