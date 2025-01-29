use crate::awesome_iterator::AwesomeIterator;
use crate::consume_token;
use crate::errors::Errors;
use crate::parser::arg_node::ArgNode;
use crate::parser::consume_group;
use crate::parser::expression_node::ExpressionNode;
use crate::parser::path_node::{parse_path, PathNode};
use crate::source_map::{HasSpan, Span};
use crate::tokenizer::TokenType::OpenParen;
use crate::treeizer::TokenTree;

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct NewExpressionNode {
    span_: Span,
    pub path: Option<PathNode>,
    pub args: Vec<ArgNode>,
}

impl NewExpressionNode {
    pub fn new<S: Into<Span>>(span: S, path: Option<PathNode>, args: Vec<ArgNode>) -> Self {
        Self {
            span_: span.into(),
            path,
            args,
        }
    }
}

impl HasSpan for NewExpressionNode {
    fn span(&self) -> Span {
        self.span_
    }
}



pub fn parse_new_expression<'a, I: Iterator<Item = &'a TokenTree>>(
    iter: &mut dyn AwesomeIterator<I>,
    errors: &mut Errors,
) -> Option<ExpressionNode> {
    let new_token = consume_token!(iter, New)?;
    let path = parse_path(iter, errors);
    let args = consume_group(iter, OpenParen).unwrap();

    Some(ExpressionNode::New(NewExpressionNode::new(new_token.span() + args.span(), path, vec![])))
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::awesome_iterator::{make_awesome, AwesomeIterator};
    use crate::errors::Errors;
    use crate::{test_tokens, test_tokentree};
    use crate::tokenizer::TokenType::*;
    use crate::treeizer::TokenTree;

    #[test]
    fn empty() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!();
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_new_expression(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(result, None);
        assert!(remaining.is_empty());
        errors.assert_empty();
    }

    #[test]
    fn unknown() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(Unknown);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_new_expression(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(result, None);
        errors.assert_empty();
        assert_eq!(remaining, test_tokentree!(Unknown).iter().collect::<Vec<_>>());
    }

    #[test]
    fn simple_new() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(New:1, Identifier:2, (:3, ):4,);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_new_expression(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(result, Some(ExpressionNode::New(NewExpressionNode::new(1..5, Some(PathNode::new(2, test_tokens!(Identifier:2), false)), vec![]))));
        errors.assert_empty();
        assert!(remaining.is_empty());
    }
}