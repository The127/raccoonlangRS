use crate::parser::pattern_node::PatternNode;
use crate::source_map::{HasSpan, SourceCollection};
use ustr::Ustr;


// TODO: what about spans?
#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Pattern {
    Discard,
    Name(Ustr),
    Tuple(Vec<Pattern>),
}

pub fn transform_pattern(
    node: &PatternNode,
    sources: &SourceCollection,
) -> Pattern{
    match node {
        PatternNode::Discard(_) => Pattern::Discard,
        PatternNode::Name(token) => Pattern::Name(sources.get_identifier(token.span())),
        PatternNode::Tuple(sub_patterns) => Pattern::Tuple(sub_patterns.value.iter().map(|x| transform_pattern(x, sources)).collect())
    }
}

#[cfg(test)]
mod test {
    use ustr::ustr;
    use super::*;
    use crate::parser::pattern_node::PatternNode;
    use crate::parser::Spanned;
    use crate::test_token;
    use crate::tokenizer::TokenType::*;

    #[test]
    fn discard_pattern() {
        // arrange
        let mut sources = SourceCollection::new();
        let span = sources.load_content("_");
        let node = PatternNode::Discard(test_token!(Discard:span));

        // act
        let pattern = transform_pattern(&node, &sources);

        // assert
        assert_eq!(pattern, Pattern::Discard);
    }

    #[test]
    fn name_pattern() {
        // arrange
        let mut sources = SourceCollection::new();
        let span = sources.load_content("foo");
        let node = PatternNode::Name(test_token!(Identifier:span));

        // act
        let pattern = transform_pattern(&node, &sources);

        // assert
        assert_eq!(pattern, Pattern::Name(ustr("foo")));
    }

    #[test]
    fn tuple_pattern() {
        // arrange
        let mut sources = SourceCollection::new();
        let span = sources.load_content("(foo, bar)");
        let span_foo = span.sub(1..4);
        let span_bar = span.sub(6..9);
        let node = PatternNode::Tuple(Spanned::new(span, vec![
            PatternNode::Name(test_token!(Identifier:span_foo)),
            PatternNode::Name(test_token!(Identifier:span_bar)),
        ]));

        // act
        let pattern = transform_pattern(&node, &sources);

        // assert
        assert_eq!(pattern, Pattern::Tuple(vec![
            Pattern::Name(ustr("foo")),
            Pattern::Name(ustr("bar")),
        ]));
    }
}