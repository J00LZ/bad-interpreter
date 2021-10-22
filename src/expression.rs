use crate::value::Value;
use crate::Environment;

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
    pub fn resolve(&self, env: &dyn Environment) -> Option<Value> {
        match self {
            Expression::Constant(x) => {
                println!("Resolving a {:?}", x);
                x.clone().into()
            }
            Expression::Var(key) => env.get_value(key.clone()),
            Expression::BinaryOp(lhs, op, rhs) => Expression::bin_op(lhs, op, rhs, env),
            Expression::UnaryOp(op, rhs) => match op {
                UnOp::Negate => match rhs.resolve(env)? {
                    Value::String(_) => Value::Nothing.into(),
                    Value::Int(value) => Value::Int(-value).into(),
                    Value::Bool(_) => Value::Nothing.into(),
                    Value::Nothing => Value::Nothing.into(),
                },
                UnOp::Not => match rhs.resolve(env)? {
                    Value::String(_) => Value::Nothing.into(),
                    Value::Int(_) => Value::Nothing.into(),
                    Value::Bool(value) => Value::Bool(!value).into(),
                    Value::Nothing => Value::Nothing.into(),
                },
            },
            Expression::Call(_, _) => Value::default().into(),
        }
    }

    fn bin_op(
        lhs: &Expression,
        op: &BinOp,
        rhs: &Expression,
        env: &dyn Environment,
    ) -> Option<Value> {
        match op {
            BinOp::Add => Expression::add(lhs, rhs, env),
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

            BinOp::Eq => Expression::equality(lhs, rhs, env),
            BinOp::Neq => Expression::equality(lhs, rhs, env).map(|x| {
                if let Value::Bool(y) = x {
                    (!y).into()
                } else {
                    x
                }
            }),
            BinOp::Lt => Value::default().into(),
            BinOp::Gt => Value::default().into(),
            BinOp::Lte => Value::default().into(),
            BinOp::Gte => Value::default().into(),

            BinOp::And | BinOp::Or | BinOp::Xor => Expression::logics(lhs, op, rhs, env),
        }
    }

    fn add(lhs: &Expression, rhs: &Expression, env: &dyn Environment) -> Option<Value> {
        return if let Value::String(s) = lhs.resolve(env)? {
            Some(if let Value::String(s2) = rhs.resolve(env)? {
                format!("{}{}", s, s2).into()
            } else if let Value::Int(i) = rhs.resolve(env)? {
                format!("{}{}", s, i).into()
            } else {
                Value::Nothing
            })
        } else if let Value::Int(i) = lhs.resolve(env)? {
            Some(if let Value::Int(i2) = rhs.resolve(env)? {
                (i + i2).into()
            } else {
                Value::Nothing
            })
        } else {
            Value::Nothing.into()
        };
    }

    fn logics(
        lhs: &Expression,
        op: &BinOp,
        rhs: &Expression,
        env: &dyn Environment,
    ) -> Option<Value> {
        match lhs.resolve(env)? {
            Value::Bool(value) => match op {
                BinOp::And => Some(
                    {
                        if !value {
                            false
                        } else if let Value::Bool(v2) = rhs.resolve(env)? {
                            v2
                        } else {
                            false
                        }
                    }
                    .into(),
                ),
                BinOp::Or => Some(
                    {
                        if value {
                            true
                        } else if let Value::Bool(v2) = rhs.resolve(env)? {
                            v2
                        } else {
                            false
                        }
                    }
                    .into(),
                ),
                BinOp::Xor => Some(
                    {
                        if let Value::Bool(v2) = rhs.resolve(env)? {
                            value ^ v2
                        } else {
                            false
                        }
                    }
                    .into(),
                ),
                _ => Value::default().into(),
            },
            _ => Value::default().into(),
        }
    }

    fn equality(lhs: &Expression, rhs: &Expression, env: &dyn Environment) -> Option<Value> {
        match lhs.resolve(env)? {
            Value::String(s) => {
                if let Value::String(s2) = rhs.resolve(env)? {
                    Some((s == s2).into())
                } else {
                    Some(false.into())
                }
            }
            Value::Int(i) => {
                if let Value::Int(j) = rhs.resolve(env)? {
                    Some((j == i).into())
                } else {
                    Some(false.into())
                }
            }
            Value::Bool(b) => {
                if let Value::Bool(c) = rhs.resolve(env)? {
                    Some((b == c).into())
                } else {
                    Some(false.into())
                }
            }
            Value::Nothing => {
                if let Value::Nothing = rhs.resolve(env)? {
                    Some(true.into())
                } else {
                    Some(false.into())
                }
            }
        }
    }

    fn maths<F>(lhs: &Expression, op: F, rhs: &Expression, env: &dyn Environment) -> Option<Value>
    where
        F: FnOnce(i64, i64) -> i64,
    {
        match lhs.resolve(env)? {
            Value::Int(value) => {
                if let Value::Int(v2) = rhs.resolve(env)? {
                    Some((op(value, v2)).into())
                } else {
                    Value::default().into()
                }
            }
            _ => Value::default().into(),
        }
    }
}
