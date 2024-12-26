use crate::errors::Errors;
use crate::marking_iterator::MarkingIterator;
use crate::parser::fn_node::FnNode;
use crate::parser::path_node::{parse_path, path_starter, PathNode};
use crate::parser::Visibility;
use crate::source_map::Span;
use crate::treeizer::TokenTree;

#[derive(Debug, Eq, PartialEq)]
pub enum TypeNode {
    Named(NamedType),
}

pub fn parse_type<'a, I: Iterator<Item = &'a TokenTree>>(
    iter: &mut impl MarkingIterator<I>,
    errors: &mut Errors,
) -> Option<TypeNode> {
    let path = parse_path(iter, errors)?;

    Some(TypeNode::Named(NamedType{
        span: path.span,
        path: path,
    }))
}

#[derive(Debug, Eq, PartialEq)]
pub struct NamedType {
    pub span: Span,
    pub path: PathNode,
}

#[cfg(test)]
mod test {
    use crate::errors::Errors;
    use crate::marking_iterator::marking;
    use crate::parser::path_node::PathNode;
    use crate::parser::type_node::{parse_type, NamedType, TypeNode};
    use crate::{test_tokens, test_tokentree};
    use crate::tokenizer::TokenType::{Identifier, Unknown};
    use crate::treeizer::TokenTree;

    #[test]
    fn parse_type_empty() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!();
        let mut iter = marking(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_type(&mut iter, &mut errors);

        // assert
        assert_eq!(result, None);
        assert!(errors.get_errors().is_empty());
    }

    #[test]
    fn parse_type_wrong_token(){
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(Unknown:2..10);
        let mut iter = marking(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_type(&mut iter, &mut errors);
        let remaining: Vec<_> = iter.collect();

        // assert
        assert_eq!(result, None);
        assert!(errors.get_errors().is_empty());
        assert_eq!(remaining, test_tokentree!(Unknown:2..10).iter().collect::<Vec<_>>())
    }

    #[test]
    fn parse_type_path(){
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(Identifier:2..10);
        let mut iter = marking(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_type(&mut iter, &mut errors);

        // assert
        assert_eq!(result, Some(TypeNode::Named(NamedType{
            span: (2..10).into(),
            path: PathNode {
                span: (2..10).into(),
                parts: test_tokens!(Identifier:2..10),
                is_rooted: false,
            }
        })));
        assert!(errors.get_errors().is_empty());
    }
}
