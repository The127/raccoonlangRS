use crate::awesome_iterator::AwesomeIterator;
use crate::errors::Errors;
use crate::parser::expression_node::ExpressionNode;
use crate::source_map::{HasSpan, Span};
use crate::tokenizer::Token;
use crate::treeizer::TokenTree;

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct MulExpressionNode {
    span_: Span,
    pub left: Box<ExpressionNode>,
    pub follows: Vec<MulExpressionNodeFollow>,
}

impl HasSpan for MulExpressionNode {
    fn span(&self) -> Span {
        self.span_
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct MulExpressionNodeFollow {
    pub operator: Token,
    pub operand: Option<ExpressionNode>
}

pub fn parse_mul_expression<'a, I: Iterator<Item = &'a TokenTree>>(
    iter: &mut dyn AwesomeIterator<I>,
    errors: &mut Errors,
    greedy_after_block: bool,
) -> Option<ExpressionNode> {
    None
}

#[cfg(test)]
mod test {
    use crate::awesome_iterator::make_awesome;
    use crate::errors::Errors;
    use crate::parser::literal_expression_node::{IntegerLiteralNode, LiteralExpressionNode};
    use crate::{test_token, test_tokentree};
    use crate::tokenizer::TokenType::{DecInteger, Unknown};
    use crate::treeizer::TokenTree;
    use super::*;

    #[test]
    fn parse_add_expression_empty_input() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!();
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_mul_expression(&mut iter, &mut errors, false);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(result, None);
        assert!(errors.get_errors().is_empty());
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }

    #[test]
    fn parse_add_expression_unknown_input() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(Unknown);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_mul_expression(&mut iter, &mut errors, false);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(result, None);
        assert!(errors.get_errors().is_empty());
        assert_eq!(remaining, test_tokentree!(Unknown).iter().collect::<Vec<_>>());
    }

    #[test]
    fn parse_add_expression_unknown_just_left() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(DecInteger:1..2,);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_mul_expression(&mut iter, &mut errors, false);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(result, Some(ExpressionNode::Literal(LiteralExpressionNode::Integer(
            IntegerLiteralNode::new(1..2, test_token!(DecInteger:1..2), false)
        ))));
        assert!(errors.get_errors().is_empty());
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }
}