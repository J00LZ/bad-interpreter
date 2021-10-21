use std::borrow::Borrow;

use crate::Environment;
use crate::value::Value;

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Expression {
    Constant(Value),
    Var(String),
    BinaryOp(Box<Expression>, BinOp, Box<Expression>),
    UnaryOp(UnOp, Box<Expression>),
    Call(String, Vec<Expression>),
}

impl<T: Into<Value>> From<T> for Expression {
    fn from(v: T) -> Self {
        Self::Constant(v.into())
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum BinOp {
    Multiply,
    Divide,
    Add,
    Subtract,
    Power,
    Mod,
    Eq,
    Neq,
    Lt,
    Gt,
    Lte,
    Gte,
    And,
    Or,
    Xor,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum UnOp {
    Negate,
    Not,
}

impl Expression {
    pub fn resolve(&self, env: &dyn Environment) -> Value {
        match self {
            Expression::Constant(x) => x.clone(),
            Expression::Var(key) => env.get_value(key.clone()),
            Expression::BinaryOp(lhs, op, rhs) => match op {
                BinOp::Add => match lhs.clone().resolve(env) {
                    Value::String { value } => match rhs.clone().resolve(env) {
                        Value::String { value: v2 } => (value + v2.clone().borrow()).into(),
                        _ => Value::default(),
                    },
                    Value::Int { value } => match rhs.clone().resolve(env) {
                        Value::Int { value: v2 } => (value.clone() + v2.clone()).into(),
                        _ => Value::default(),
                    },
                    Value::Bool { value: _ } => Value::default(),
                    Value::Nothing => Value::default(),
                },
                _ => Value::Nothing,
            },
            Expression::UnaryOp(_, _) => Value::default(),
            Expression::Call(_, _) => Value::default(),
        }
    }
}
