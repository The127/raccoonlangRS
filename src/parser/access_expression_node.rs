use crate::awesome_iterator::AwesomeIterator;
use crate::consume_token;
use crate::errors::Errors;
use crate::parser::expression_node::ExpressionNode;
use crate::source_map::{HasSpan, Span};
use crate::tokenizer::Token;
use crate::treeizer::TokenTree;


#[derive(Debug, Eq, PartialEq, Clone)]
pub struct AccessExpressionNode{
    span_: Span,
    pub identifier: Token,
}

impl HasSpan for AccessExpressionNode {
    #[mutants::skip]
    fn span(&self) -> Span {
        self.span_
    }
}

impl AccessExpressionNode {
    pub fn new(token: Token) -> Self {
        Self {
            span_: token.span(),
            identifier: token,
        }
    }
}

pub fn parse_access_expression<'a, I: Iterator<Item = &'a TokenTree>>(
    iter: &mut dyn AwesomeIterator<I>,
    _errors: &mut Errors,
) -> Option<ExpressionNode> {
    consume_token!(iter, Identifier)
        .map(|t| ExpressionNode::Access(AccessExpressionNode {
            span_: t.span(),
            identifier: t,
        }))
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::awesome_iterator::make_awesome;
    use crate::errors::Errors;
    use crate::{test_token, test_tokentree};
    use crate::tokenizer::TokenType::*;
    use crate::treeizer::TokenTree;

    #[test]
    fn empty_input() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!();
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_access_expression(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(result, None);
        errors.assert_empty();
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }

    #[test]
    fn unknown_input() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(Unknown);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_access_expression(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(result, None);
        errors.assert_empty();
        assert_eq!(remaining, test_tokentree!(Unknown).iter().collect::<Vec<_>>());
    }

    #[test]
    fn identifier() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(Identifier:3..10);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_access_expression(&mut iter, &mut errors);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(result, Some(ExpressionNode::Access(AccessExpressionNode {
            span_: Span(3, 10),
            identifier: test_token!(Identifier:3..10),
        })));
        errors.assert_empty();
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }
}