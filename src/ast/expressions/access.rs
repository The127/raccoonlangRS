use crate::ast::expressions::Expression;
use crate::ast::path::Path;
use crate::errors::Errors;
use crate::parser::access_expression_node::AccessExpressionNode;
use crate::source_map::{HasSpan, SourceCollection, Span};

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct AccessExpression {
    pub(super) span_: Span,
    pub path: Path,
}

impl HasSpan for AccessExpression {
    fn span(&self) -> Span {
        self.span_
    }
}

pub fn transform_access_expression(
    node: &AccessExpressionNode,
    errors: &mut Errors,
    sources: &SourceCollection,
) -> Expression {
    // TODO: in the future the AccessExpressionNode should have a path rather than just an identifier
    Expression::access(
        node.span(),
        Path::new(vec![sources.get_identifier(node.identifier.span())], false),
    )
}

#[cfg(test)]
mod test {
    use crate::ast::expressions::{transform_expression, Expression};
    use crate::ast::path::Path;
    use crate::errors::Errors;
    use crate::parser::access_expression_node::AccessExpressionNode;
    use crate::parser::expression_node::ExpressionNode;
    use crate::source_map::SourceCollection;
    use crate::test_token;
    use crate::tokenizer::TokenType::Identifier;

    #[test]
    fn transform_access() {
        // arrange
        let mut sources = SourceCollection::new();
        let mut errors = Errors::new();
        let span = sources.load_content("foobar");

        let expr_node =
            ExpressionNode::Access(AccessExpressionNode::new(test_token!(Identifier:span)));

        // act
        let expr = transform_expression(&expr_node, &mut errors, &sources);

        // assert
        assert_eq!(expr, Expression::access(span, Path::name("foobar")));
    }
}
