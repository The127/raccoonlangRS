use ustr::Ustr;
use crate::ast::expressions::Expression;
use crate::parser::access_expression_node::AccessExpressionNode;
use crate::source_map::{HasSpan, SourceCollection, Span};

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct AccessExpression {
    pub(super) span_: Span,
    pub name: Ustr,
}

impl HasSpan for AccessExpression {
    fn span(&self) -> Span {
        self.span_
    }
}


pub fn transform_access_expression(
    node: &AccessExpressionNode,
    sources: &SourceCollection,
) -> Expression {
    Expression::access(node.span(), sources.get_identifier(node.identifier.span()))
}


#[cfg(test)]
mod test {
    use ustr::ustr;
    use crate::ast::expressions::{transform_expression, Expression};
    use crate::parser::access_expression_node::AccessExpressionNode;
    use crate::parser::expression_node::ExpressionNode;
    use crate::source_map::SourceCollection;
    use crate::test_token;
    use crate::tokenizer::TokenType::Identifier;

    #[test]
    fn transform_access() {
        // arrange
        let mut sources = SourceCollection::new();
        let span = sources.load_content("foobar");

        let expr_node =
            ExpressionNode::Access(AccessExpressionNode::new(test_token!(Identifier:span)));

        // act
        let expr = transform_expression(&expr_node, &sources);

        // assert
        assert_eq!(expr, Expression::access(span, ustr("foobar")));
    }
}