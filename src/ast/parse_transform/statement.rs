use crate::ast::parse_transform::transform_expression;
use crate::ast::statement::Statement;
use crate::errors::Errors;
use crate::parser::block_expression_node::{StatementKind, StatementNode};
use crate::parser::expression_node::ExpressionNode;
use crate::source_map::SourceCollection;

pub fn transform_statement(
    node: &StatementNode,
    errors: &mut Errors,
    sources: &SourceCollection,
) -> Option<Statement> {
    match &node.kind {
        StatementKind::Empty => None,
        StatementKind::Expression(x) => Some(transform_expression_statement(x, errors, sources)),
        StatementKind::Declaration(x) => unreachable!(),
    }
}

fn transform_expression_statement(
    node: &ExpressionNode,
    errors: &mut Errors,
    sources: &SourceCollection,
) -> Statement {
    Statement::Expression(transform_expression(node, errors, sources))
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::ast::expressions::Expression;
    use crate::ast::path::Path;
    use crate::errors::Errors;
    use crate::parser::access_expression_node::AccessExpressionNode;
    use crate::parser::block_expression_node::StatementNode;
    use crate::parser::expression_node::ExpressionNode;
    use crate::source_map::SourceCollection;
    use crate::test_token;
    use crate::tokenizer::TokenType::Identifier;

    #[test]
    fn transform_empty() {
        // arrange
        let mut sources = SourceCollection::new();
        let mut errors = Errors::new();
        let span = sources.load_content(";");
        let stmt_node = StatementNode::empty(span);

        // act
        let stmt = transform_statement(&stmt_node, &mut errors, &sources);

        // assert
        assert_eq!(stmt, None);
    }

    #[test]
    fn transform_expression() {
        // arrange
        let mut sources = SourceCollection::new();
        let mut errors = Errors::new();
        let span = sources.load_content("foobar");
        let stmt_node = StatementNode::expr(
            span,
            ExpressionNode::Access(AccessExpressionNode::new(test_token!(Identifier:span))),
        );

        // act
        let stmt = transform_statement(&stmt_node, &mut errors, &sources);

        // assert
        assert_eq!(
            stmt,
            Some(Statement::Expression(Expression::access(
                span,
                Path::name("foobar")
            )))
        );
    }
}
