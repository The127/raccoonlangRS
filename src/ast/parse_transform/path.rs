use crate::ast::path::Path;
use crate::parser::path_node::PathNode;
use crate::source_map::{HasSpan, SourceCollection};

pub fn transform_path(node: &PathNode, sources: &SourceCollection) -> Path {
    let parts = node
        .parts
        .iter()
        .map(|p| sources.get_identifier(p.span()))
        .collect();

    Path::new(parts, node.is_rooted)
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::parser::path_node::PathNode;
    use crate::source_map::SourceCollection;
    use crate::test_tokens;
    use crate::tokenizer::TokenType::Identifier;
    use ustr::ustr;

    #[test]
    fn name() {
        // arrange
        let mut sources = SourceCollection::new();
        let span = sources.load_content("foo");
        let node = PathNode::new(span, test_tokens!(Identifier:span), false);

        // act
        let path = transform_path(&node, &sources);

        // assert
        assert_eq!(path, Path::name("foo"));
    }

    #[test]
    fn multiple_parts() {
        // arrange
        let mut sources = SourceCollection::new();
        let span = sources.load_content("foo::bar::baz");
        let span_foo = span.sub(..3);
        let span_bar = span.sub(5..8);
        let span_baz = span.sub(10..);
        let node = PathNode::new(
            span,
            test_tokens!(Identifier:span_foo, Identifier:span_bar, Identifier:span_baz),
            false,
        );

        // act
        let path = transform_path(&node, &sources);

        // assert
        assert_eq!(
            path,
            Path::new(vec![ustr("foo"), ustr("bar"), ustr("baz")], false)
        );
    }

    #[test]
    fn rooted() {
        // arrange
        let mut sources = SourceCollection::new();
        let span = sources.load_content("::foo");
        let span_foo = span.sub(2..);
        let node = PathNode::new(span, test_tokens!(Identifier:span_foo), true);

        // act
        let path = transform_path(&node, &sources);

        // assert
        assert_eq!(path, Path::new(vec![ustr("foo")], true));
    }
}
