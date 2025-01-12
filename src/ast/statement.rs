use crate::ast::expressions::{transform_expression, Expression};
use crate::parser::block_expression_node::{StatementKind, StatementNode};
use crate::parser::expression_node::ExpressionNode;
use crate::parser::let_declaration_node::LetDeclarationNode;
use crate::source_map::{HasSpan, SourceCollection, Span};
use ustr::Ustr;

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Statement {
    Expression(Expression),
}


pub fn transform_statement(node: &StatementNode, sources: &SourceCollection) -> Option<Statement> {
    match &node.kind {
        StatementKind::Empty => None,
        StatementKind::Expression(x) => Some(transform_expression_statement(x, sources)),
        StatementKind::Declaration(x) => unreachable!(),
    }
}

fn transform_expression_statement(node: &ExpressionNode, sources: &SourceCollection) -> Statement {
    Statement::Expression(transform_expression(node, sources))
}


#[cfg(test)]
mod test {
    use crate::ast::expressions::Expression;
    use crate::ast::statement::{transform_statement, Statement};
    use crate::parser::access_expression_node::AccessExpressionNode;
    use crate::parser::block_expression_node::StatementNode;
    use crate::parser::expression_node::ExpressionNode;
    use crate::parser::let_declaration_node::LetDeclarationNode;
    use crate::source_map::SourceCollection;
    use crate::test_token;
    use crate::tokenizer::TokenType::Identifier;
    use ustr::ustr;

    #[test]
    fn transform_empty() {
        // arrange
        let mut sources = SourceCollection::new();
        let span = sources.load_content(";");
        let stmt_node = StatementNode::empty(span);

        // act
        let stmt = transform_statement(&stmt_node, &sources);

        // assert
        assert_eq!(stmt, None);
    }

    #[test]
    fn transform_expression() {
        // arrange
        let mut sources = SourceCollection::new();
        let span = sources.load_content("foobar");
        let stmt_node = StatementNode::expr(span, ExpressionNode::Access(AccessExpressionNode::new(test_token!(Identifier:span))));

        // act
        let stmt = transform_statement(&stmt_node, &sources);

        // assert
        assert_eq!(stmt, Some(Statement::Expression(Expression::access(span, ustr("foobar")))));
    }

}
