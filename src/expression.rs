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
            Expression::BinaryOp(lhs, op, rhs) => Expression::bin_op(lhs, op, rhs, env),
            Expression::UnaryOp(_, _) => Value::default(),
            Expression::Call(_, _) => Value::default(),
        }
    }

    fn bin_op(lhs: &Expression, op: &BinOp, rhs: &Expression, env: &dyn Environment) -> Value {
        match op {
            BinOp::Add => match lhs.clone().resolve(env) {
                Value::String { value } => match rhs.clone().resolve(env) {
                    Value::String { value: v2 } => format!("{}{}", value, v2).into(),
                    Value::Int { value: v2 } => format!("{}{}", value, v2).into(),
                    _ => Value::default(),
                },
                Value::Int { value } => match rhs.clone().resolve(env) {
                    Value::Int { value: v2 } => (value + v2).into(),
                    _ => Value::default(),
                },
                Value::Bool { value: _ } | Value::Nothing => Value::default(),
            },
            BinOp::Subtract => Expression::maths(lhs, |a, b| a - b, rhs, env),
            BinOp::Multiply => Expression::maths(lhs, |a, b| a * b, rhs, env),
            BinOp::Divide => Expression::maths(lhs, |a, b| a / b, rhs, env),
            BinOp::Mod => Expression::maths(lhs, |a, b| a % b, rhs, env),
            BinOp::Power => Expression::maths(
                lhs,
                |a, b| {
                    if b < 0 || b > u32::MAX.into() {
                        0
                    } else {
                        #[allow(clippy::cast_sign_loss, clippy::cast_possible_truncation)]
                        a.pow(b as u32)
                    }
                },
                rhs,
                env,
            ),

            BinOp::Eq => Value::default(),
            BinOp::Neq => Value::default(),
            BinOp::Lt => Value::default(),
            BinOp::Gt => Value::default(),
            BinOp::Lte => Value::default(),
            BinOp::Gte => Value::default(),

            BinOp::And | BinOp::Or | BinOp::Xor => Expression::logics(lhs, op, rhs, env),
        }
    }

    fn logics(lhs: &Expression, op: &BinOp, rhs: &Expression, env: &dyn Environment) -> Value {
        match lhs.resolve(env) {
            Value::Bool { value } => match op {
                BinOp::And => {
                    if !value {
                        false
                    } else if let Value::Bool { value: v2 } = rhs.resolve(env) {
                        v2
                    } else {
                        false
                    }
                }
                .into(),
                BinOp::Or => {
                    if value {
                        true
                    } else if let Value::Bool { value: v2 } = rhs.resolve(env) {
                        v2
                    } else {
                        false
                    }
                }
                .into(),
                BinOp::Xor => {
                    if let Value::Bool { value: v2 } = rhs.resolve(env) {
                        value ^ v2
                    } else {
                        false
                    }
                }
                .into(),
                _ => Value::default(),
            },
            _ => Value::default(),
        }
    }

    fn maths<F>(lhs: &Expression, op: F, rhs: &Expression, env: &dyn Environment) -> Value
    where
        F: FnOnce(i64, i64) -> i64,
    {
        match lhs.resolve(env) {
            Value::Int { value } => {
                if let Value::Int { value: v2 } = rhs.resolve(env) {
                    (op(value, v2)).into()
                } else {
                    Value::default()
                }
            }
            _ => Value::default(),
        }
    }
}
