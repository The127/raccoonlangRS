use crate::ast::expressions::Expression;

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Statement {
    Expression(Expression),
}
