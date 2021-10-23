use crate::{Env, Environment, World};
use crate::instruction::Instruction;
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
    pub fn resolve(
        &self,
        env: &Env,
        world: &mut World,
        instructions: &mut Vec<Instruction>,
    ) -> Option<Value> {
        match self {
            Expression::Constant(x) => {
                println!("Resolving a {:?}", x);
                x.clone().into()
            }
            Expression::Var(key) => env.get_value(key.clone()),
            Expression::BinaryOp(lhs, op, rhs) => {
                Expression::bin_op(lhs, op, rhs, env, world, instructions)
            }
            Expression::UnaryOp(op, rhs) => match op {
                UnOp::Negate => match rhs.resolve(env, world, instructions)? {
                    Value::Int(value) => Value::Int(-value).into(),
                    Value::Nothing | Value::String(_) | Value::Bool(_) => Value::Nothing.into(),
                },
                UnOp::Not => match rhs.resolve(env, world, instructions)? {
                    Value::Bool(value) => Value::Bool(!value).into(),
                    Value::String(_) | Value::Int(_) | Value::Nothing => Value::Nothing.into(),
                },
            },
            Expression::Call(name, params) => {
                let mut a = Vec::new();
                for param in params {
                    let p = param.resolve(env, world, instructions)?;
                    a.push(p);
                }
                let func = world.functions.get(name)?;
                let (val, mut instrs) = func(world, a);
                instructions.append(&mut instrs);
                val.into()
            }
        }
    }

    fn bin_op(
        lhs: &Expression,
        op: &BinOp,
        rhs: &Expression,
        env: &Env,
        world: &mut World,
        instructions: &mut Vec<Instruction>,
    ) -> Option<Value> {
        match op {
            BinOp::Add => Expression::add(lhs, rhs, env, world, instructions),
            BinOp::Subtract => Expression::maths(lhs, |a, b| a - b, rhs, env, world, instructions),
            BinOp::Multiply => Expression::maths(lhs, |a, b| a * b, rhs, env, world, instructions),
            BinOp::Divide => Expression::maths(lhs, |a, b| a / b, rhs, env, world, instructions),
            BinOp::Mod => Expression::maths(lhs, |a, b| a % b, rhs, env, world, instructions),
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
                world,
                instructions,
            ),

            BinOp::Eq => Some(
                (lhs.resolve(env, world, instructions)?
                    == rhs.resolve(env, world, instructions)?)
                .into(),
            ),
            BinOp::Neq => Some(
                (lhs.resolve(env, world, instructions)?
                    != rhs.resolve(env, world, instructions)?)
                .into(),
            ),

            BinOp::Lt => Some(
                (lhs.resolve(env, world, instructions)? < rhs.resolve(env, world, instructions)?)
                    .into(),
            ),
            BinOp::Gt => Some(
                (lhs.resolve(env, world, instructions)? > rhs.resolve(env, world, instructions)?)
                    .into(),
            ),
            BinOp::Lte => Some(
                (lhs.resolve(env, world, instructions)?
                    <= rhs.resolve(env, world, instructions)?)
                .into(),
            ),
            BinOp::Gte => Some(
                (lhs.resolve(env, world, instructions)?
                    >= rhs.resolve(env, world, instructions)?)
                .into(),
            ),

            BinOp::And | BinOp::Or | BinOp::Xor => {
                Expression::logics(lhs, op, rhs, env, world, instructions)
            }
        }
    }

    fn add(
        lhs: &Expression,
        rhs: &Expression,
        env: &Env,
        world: &mut World,
        instructions: &mut Vec<Instruction>,
    ) -> Option<Value> {
        return match lhs.resolve(env, world, instructions)? {
            Value::String(s) => {
                let rhs = rhs.resolve(env, world, instructions)?;
                Some(if let Value::String(s2) = rhs {
                    format!("{}{}", s, s2).into()
                } else if let Value::Int(i) = rhs {
                    format!("{}{}", s, i).into()
                } else {
                    Value::Nothing
                })
            }
            Value::Int(i) => Some(
                if let Value::Int(i2) = rhs.resolve(env, world, instructions)? {
                    (i + i2).into()
                } else {
                    Value::Nothing
                },
            ),
            _ => Value::Nothing.into(),
        };
    }

    fn logics(
        lhs: &Expression,
        op: &BinOp,
        rhs: &Expression,
        env: &Env,
        world: &mut World,
        instructions: &mut Vec<Instruction>,
    ) -> Option<Value> {
        match lhs.resolve(env, world, instructions)? {
            Value::Bool(value) => match op {
                BinOp::And => Some(
                    {
                        if !value {
                            false
                        } else if let Value::Bool(v2) = rhs.resolve(env, world, instructions)? {
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
                        } else if let Value::Bool(v2) = rhs.resolve(env, world, instructions)? {
                            v2
                        } else {
                            false
                        }
                    }
                    .into(),
                ),
                BinOp::Xor => Some(
                    {
                        if let Value::Bool(v2) = rhs.resolve(env, world, instructions)? {
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

    fn maths<F>(
        lhs: &Expression,
        op: F,
        rhs: &Expression,
        env: &Env,
        world: &mut World,
        instructions: &mut Vec<Instruction>,
    ) -> Option<Value>
    where
        F: FnOnce(i64, i64) -> i64,
    {
        match lhs.resolve(env, world, instructions)? {
            Value::Int(value) => {
                if let Value::Int(v2) = rhs.resolve(env, world, instructions)? {
                    Some((op(value, v2)).into())
                } else {
                    Value::default().into()
                }
            }
            _ => Value::default().into(),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{Env, World};
    use crate::expression::{BinOp, Expression};
    use crate::instruction::Instruction;
    use crate::tests::TestEnv;
    use crate::value::Value;

    #[test]
    fn add_test() {
        let lhs: Expression = 5.into();
        let rhs: Expression = 10.into();
        let op = Expression::BinaryOp(lhs.into(), BinOp::Add, rhs.into());

        let mut w = World {
            functions: Default::default(),
            map: Default::default(),
        };
        let te = Env::new();
        let mut instructions: Vec<Instruction> = Vec::new();
        let res = op
            .resolve(&te, &mut w, &mut instructions)
            .expect("This should work");
        assert_eq!(res, 15.into())
    }

    #[test]
    fn eq_test() {
        let lhs: Expression = 5.into();
        let rhs: Expression = 10.into();
        let op = Expression::BinaryOp(lhs.into(), BinOp::Eq, rhs.into());

        let mut w = World {
            functions: Default::default(),
            map: Default::default(),
        };
        let te = Env::new();
        let mut instructions: Vec<Instruction> = Vec::new();

        let res = op
            .resolve(&te, &mut w, &mut instructions)
            .expect("This should work");
        assert_eq!(res, false.into())
    }

    #[test]
    fn neq_test() {
        let lhs: Expression = 5.into();
        let rhs: Expression = 10.into();
        let op = Expression::BinaryOp(lhs.into(), BinOp::Neq, rhs.into());

        let mut w = World {
            functions: Default::default(),
            map: Default::default(),
        };
        let te = Env::new();
        let mut instructions: Vec<Instruction> = Vec::new();

        let res = op
            .resolve(&te, &mut w, &mut instructions)
            .expect("This should work");
        assert_eq!(res, true.into())
    }

    #[test]
    fn call_test() {
        let op = Expression::Call(String::from("test"), vec![]);

        let mut w = World {
            functions: Default::default(),
            map: Default::default(),
        };
        w.functions.insert(String::from("test"), |a, b| {
            (
                a.map
                    .insert(String::from("foo"), 10.into())
                    .unwrap_or(Value::Nothing),
                vec![Instruction::Instruction(
                    String::from("test"),
                    String::from("kees"),
                )],
            )
        });
        let te = Env::new();
        let mut instructions: Vec<Instruction> = Vec::new();

        let res = op
            .resolve(&te, &mut w, &mut instructions)
            .expect("This should work");
        assert_eq!(res, Value::Nothing);
        assert_eq!(w.map.get("foo").cloned(), Some(10.into()));
        assert_eq!(
            instructions,
            vec![Instruction::Instruction(
                String::from("test"),
                String::from("kees")
            )]
        );
    }
}
