use crate::expression::Expression;

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Statement {
    Assignment(String, Expression, bool),
    While(Expression, Vec<Statement>),
    FuncDef(String, Vec<String>, Vec<Statement>),
    Return(Expression),
    Expression(Expression),
}


