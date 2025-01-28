use crate::add_error;
use crate::awesome_iterator::AwesomeIterator;
use crate::errors::Errors;
use crate::parser::expression_node::ExpressionNode;
use crate::source_map::{HasSpan, Span};
use crate::tokenizer::Token;
use crate::treeizer::TokenTree;
use crate::{consume_token, seq_expression};
use crate::parser::subsequent_expression_node::parse_subsequent_expression;

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

impl MulExpressionNode {
    pub fn new<S: Into<Span>>(
        span: S,
        left: Box<ExpressionNode>,
        follows: Vec<MulExpressionNodeFollow>,
    ) -> Self {
        Self {
            span_: span.into(),
            left: left,
            follows: follows,
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct MulExpressionNodeFollow {
    pub operator: Token,
    pub operand: Option<ExpressionNode>
}

seq_expression!(parse_mul_expression, parse_subsequent_expression, Asterisk|Slash, Mul, MulExpressionNode, MulExpressionNodeFollow);

#[cfg(test)]
mod test {
    use super::*;
    // NOTE: seq_expression is sufficiently tested in the add expression
    use crate::awesome_iterator::make_awesome;
    use crate::errors::Errors;
    use crate::parser::literal_expression_node::{NumberLiteralNode, LiteralExpressionNode};
    use crate::tokenizer::TokenType::{DecInteger, Unknown};
    use crate::treeizer::TokenTree;
    use crate::{test_token, test_tokentree};

    #[test]
    fn empty_input() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!();
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_mul_expression(&mut iter, &mut errors, false);
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
        let result = parse_mul_expression(&mut iter, &mut errors, false);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(result, None);
        errors.assert_empty();
        assert_eq!(remaining, test_tokentree!(Unknown).iter().collect::<Vec<_>>());
    }

    #[test]
    fn just_left() {
        // arrange
        let input: Vec<TokenTree> = test_tokentree!(DecInteger:1..2,);
        let mut iter = make_awesome(input.iter());
        let mut errors = Errors::new();

        // act
        let result = parse_mul_expression(&mut iter, &mut errors, false);
        let remaining = iter.collect::<Vec<_>>();

        // assert
        assert_eq!(result, Some(ExpressionNode::Literal(LiteralExpressionNode::Number(
            NumberLiteralNode::new(1..2, test_token!(DecInteger:1..2), false)
        ))));
        errors.assert_empty();
        assert_eq!(remaining, test_tokentree!().iter().collect::<Vec<_>>());
    }
}