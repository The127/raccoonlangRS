use crate::consume_token;
use crate::errors::Errors;
use crate::awesome_iterator::AwesomeIterator;
use crate::parser::{consume_tokens};
use crate::source_map::{HasSpan, Span};
use crate::tokenizer::Token;
use crate::tokenizer::TokenType::{Identifier, PathSeparator};
use crate::treeizer::TokenTree;

#[derive(Debug, Eq, PartialEq)]
pub struct PathNode {
    span_: Span,
    pub parts: Vec<Token>,
    pub is_rooted: bool,
}

impl HasSpan for PathNode {
    fn span(&self) -> Span {
        self.span_
    }
}

pub fn parse_path<'a, I: Iterator<Item = &'a TokenTree>>(
    iter: &mut dyn AwesomeIterator<I>,
    _: &mut Errors,
) -> Option<PathNode> {
    let mut iter = iter.mark();

    let mut node = PathNode {
        span_: Span::empty(),
        parts: vec![],
        is_rooted: false,
    };

    if let Some(root) = consume_token!(&mut iter, PathSeparator) {
        node.is_rooted = true;
        node.span_ = root.span();
    }

    if let Some(first) = consume_token!(&mut iter, Identifier) {
        node.span_ += first.span();
        node.parts.push(first);
    } else {
        iter.reset();
        return None;
    }

    while let Some([_, id]) = consume_tokens(&mut iter, [PathSeparator, Identifier]) {
        node.span_ += id.span();
        node.parts.push(id);
    }

    Some(node)
}

// make_starter_matcher_thingie!(path_starter, [Identifier], [PathSeparator Identifier]);
// make_starter_matcher_thingie!(struct_starter, [Struct], [Pub Struct]);

pub fn path_starter<'a, I: Iterator<Item = &'a TokenTree>>(
    iter: &mut dyn AwesomeIterator<I>,
) -> bool {
    let mut mark = iter.mark().auto_reset();
    let result = match mark.next() {
        Some(TokenTree::Token(Token {
            token_type: Identifier,
            ..
        })) => true,
        Some(TokenTree::Token(Token {
            token_type: PathSeparator,
            ..
        })) => match mark.next() {
            Some(TokenTree::Token(Token {
                token_type: Identifier,
                ..
            })) => true,
            _ => false,
        },
        _ => false,
    };

    result
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::awesome_iterator::make_awesome;
    use crate::tokenizer::TokenType::Unknown;
    use crate::{test_tokens, test_tokentree};

    #[test]
    fn empty_input() {
        // arrange
        let tokens = test_tokentree!();
        let mut iter = make_awesome(tokens.iter());
        let mut errors = Errors::new();

        // act
        let path = parse_path(&mut iter, &mut errors);

        // assert
        assert_eq!(path, None);
        assert!(errors.get_errors().is_empty());
        assert!(iter.collect::<Vec<_>>().is_empty());
    }

    #[test]
    fn non_matching_input() {
        // arrange
        let tokens = test_tokentree!(Unknown);
        let mut iter = make_awesome(tokens.iter());
        let mut errors = Errors::new();

        // act
        let path = parse_path(&mut iter, &mut errors);

        // assert
        assert_eq!(path, None);
        assert!(errors.get_errors().is_empty());
        assert_eq!(
            iter.collect::<Vec<_>>(),
            test_tokentree!(Unknown).iter().collect::<Vec<_>>()
        );
    }

    #[test]
    fn single_identifier() {
        // arrange
        let tokens = test_tokentree!(Identifier:5..10);
        let mut iter = make_awesome(tokens.iter());
        let mut errors = Errors::new();

        // act
        let path = parse_path(&mut iter, &mut errors);

        // assert
        assert_eq!(
            path,
            Some(PathNode {
                parts: test_tokens!(Identifier:5..10),
                is_rooted: false,
                span_: (5..10).into(),
            })
        );
        assert!(errors.get_errors().is_empty());
        assert!(iter.collect::<Vec<_>>().is_empty());
    }

    #[test]
    fn rooted_identifier() {
        // arrange
        let tokens = test_tokentree!(PathSeparator, Identifier);
        let mut iter = make_awesome(tokens.iter());
        let mut errors = Errors::new();

        // act
        let path = parse_path(&mut iter, &mut errors);

        // assert
        assert_eq!(
            path,
            Some(PathNode {
                parts: test_tokens!(Identifier),
                is_rooted: true,
                span_: Span::empty(),
            })
        );
        assert!(errors.get_errors().is_empty());
        assert!(iter.collect::<Vec<_>>().is_empty());
    }

    #[test]
    fn two_identifiers() {
        // arrange
        let tokens = test_tokentree!(Identifier, PathSeparator, Identifier);
        let mut iter = make_awesome(tokens.iter());
        let mut errors = Errors::new();

        // act
        let path = parse_path(&mut iter, &mut errors);

        // assert
        assert_eq!(
            path,
            Some(PathNode {
                parts: test_tokens!(Identifier, Identifier),
                is_rooted: false,
                span_: Span::empty(),
            })
        );
        assert!(errors.get_errors().is_empty());
        assert!(iter.collect::<Vec<_>>().is_empty());
    }

    #[test]
    fn two_identifiers_rooted() {
        // arrange
        let tokens = test_tokentree!(PathSeparator, Identifier, PathSeparator, Identifier);
        let mut iter = make_awesome(tokens.iter());
        let mut errors = Errors::new();

        // act
        let path = parse_path(&mut iter, &mut errors);

        // assert
        assert_eq!(
            path,
            Some(PathNode {
                parts: test_tokens!(Identifier, Identifier),
                is_rooted: true,
                span_: Span::empty(),
            })
        );
        assert!(errors.get_errors().is_empty());
        assert!(iter.collect::<Vec<_>>().is_empty());
    }

    #[test]
    fn remaining_tokens() {
        // arrange
        let tokens = test_tokentree!(
            PathSeparator,
            Identifier,
            PathSeparator,
            Identifier,
            Identifier
        );
        let mut iter = make_awesome(tokens.iter());
        let mut errors = Errors::new();

        // act
        let path = parse_path(&mut iter, &mut errors);
        let remaining: Vec<_> = iter.collect();

        // assert
        assert_eq!(
            path,
            Some(PathNode {
                parts: test_tokens!(Identifier, Identifier),
                is_rooted: true,
                span_: Span::empty(),
            })
        );
        assert!(errors.get_errors().is_empty());
        assert_eq!(
            remaining,
            test_tokentree!(Identifier).iter().collect::<Vec<_>>()
        );
    }

    #[test]
    fn trailing_path_separator() {
        // arrange
        let tokens = test_tokentree!(
            PathSeparator,
            Identifier,
            PathSeparator,
            Identifier,
            PathSeparator
        );
        let mut iter = make_awesome(tokens.iter());
        let mut errors = Errors::new();

        // act
        let path = parse_path(&mut iter, &mut errors);
        let remaining: Vec<_> = iter.collect();

        // assert
        assert_eq!(
            path,
            Some(PathNode {
                parts: test_tokens!(Identifier, Identifier),
                is_rooted: true,
                span_: Span::empty(),
            })
        );
        assert!(errors.get_errors().is_empty());
        assert_eq!(
            remaining,
            test_tokentree!(PathSeparator).iter().collect::<Vec<_>>()
        );
    }

    #[test]
    fn path_starter_empty() {
        // arrange
        let tokens = test_tokentree!();
        let mut iter = make_awesome(tokens.iter());

        // act
        let matches = path_starter(&mut iter);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(matches, false);
        assert_eq!(remaining, tokens.iter().collect::<Vec<_>>())
    }

    #[test]
    fn path_starter_identifier() {
        // arrange
        let tokens = test_tokentree!(Identifier);
        let mut iter = make_awesome(tokens.iter());

        // act
        let matches = path_starter(&mut iter);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(matches, true);
        assert_eq!(remaining, tokens.iter().collect::<Vec<_>>())
    }

    #[test]
    fn path_starter_two_identifiers() {
        // arrange
        let tokens = test_tokentree!(Identifier, Identifier);
        let mut iter = make_awesome(tokens.iter());

        // act
        let matches = path_starter(&mut iter);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(matches, true);
        assert_eq!(remaining, tokens.iter().collect::<Vec<_>>())
    }

    #[test]
    fn path_starter_identifier_pathsep() {
        // arrange
        let tokens = test_tokentree!(Identifier, PathSeparator);
        let mut iter = make_awesome(tokens.iter());

        // act
        let matches = path_starter(&mut iter);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(matches, true);
        assert_eq!(remaining, tokens.iter().collect::<Vec<_>>())
    }

    #[test]
    fn path_starter_pathsep_identifier() {
        // arrange
        let tokens = test_tokentree!(PathSeparator, Identifier);
        let mut iter = make_awesome(tokens.iter());

        // act
        let matches = path_starter(&mut iter);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(matches, true);
        assert_eq!(remaining, tokens.iter().collect::<Vec<_>>())
    }

    #[test]
    fn path_starter_pathsep() {
        // arrange
        let tokens = test_tokentree!(PathSeparator);
        let mut iter = make_awesome(tokens.iter());

        // act
        let matches = path_starter(&mut iter);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(matches, false);
        assert_eq!(remaining, tokens.iter().collect::<Vec<_>>())
    }

    #[test]
    fn path_starter_unknown_identifier() {
        // arrange
        let tokens = test_tokentree!(Unknown, Identifier);
        let mut iter = make_awesome(tokens.iter());

        // act
        let matches = path_starter(&mut iter);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(matches, false);
        assert_eq!(remaining, tokens.iter().collect::<Vec<_>>())
    }
}
