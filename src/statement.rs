use std::fmt::Debug;

use crate::expression::Expression;

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Statement<V>
where
    V: Eq + PartialEq + PartialOrd + Debug + Clone,
{
    Assignment(String, Expression<V>, bool),
    While(Expression<V>, Vec<Statement<V>>),
    FuncDef(String, Vec<String>, Vec<Statement<V>>),
    IfElse(Expression<V>, Vec<Statement<V>>, Vec<Statement<V>>),
    Return(Expression<V>),
    Expression(Expression<V>),
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Func<V>
where
    V: Eq + PartialEq + PartialOrd + Debug + Clone,
{
    pub params: Vec<String>,
    pub statements: Vec<Statement<V>>,
}

impl<V> From<Expression<V>> for Statement<V>
where
    V: Eq + PartialEq + PartialOrd + Debug + Clone,
{
    fn from(x: Expression<V>) -> Self {
        Self::Expression(x)
    }
}
