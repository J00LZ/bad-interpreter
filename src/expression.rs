use std::collections::HashMap;
use std::fmt::Debug;

use crate::environment::Env;
use crate::instruction::Instruction;
use crate::interpreter;
use crate::statement::Func;
use crate::value::Value;
use crate::world::World;

use serde::{Deserialize, Serialize};

#[derive(Debug, Eq, PartialEq, Clone, Serialize, Deserialize)]
#[serde(rename_all = "lowercase", tag = "type")]
pub enum Expression<V>
where
    V: Eq + PartialEq + PartialOrd + Debug + Clone,
{
    Constant {
        #[serde(rename = "const")]
        value: Value<V>,
        #[serde(default)]
        id: String,
    },
    Var {
        name: String,
        #[serde(default)]
        id: String,
    },
    BinaryOp {
        lhs: Box<Expression<V>>,
        op: BinOp,
        rhs: Box<Expression<V>>,
        #[serde(default)]
        id: String,
    },
    UnaryOp {
        op: UnOp,
        expression: Box<Expression<V>>,
        #[serde(default)]
        id: String,
    },
    Call {
        name: String,
        params: Vec<Expression<V>>,
        #[serde(default)]
        id: String,
    },
}

impl<T: Into<Value<V>>, V> From<T> for Expression<V>
where
    V: Eq + PartialOrd + Debug + Clone,
{
    fn from(v: T) -> Self {
        Self::Constant {
            value: v.into(),
            id: String::from("id"),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
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

#[derive(Debug, Eq, PartialEq, Clone, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum UnOp {
    Negate,
    Not,
}

impl<V> Expression<V>
where
    V: Eq + PartialOrd + Debug + Clone,
{
    pub fn resolve<T>(
        &self,
        env: &Env<V>,
        world: &mut World<T, V>,
        instructions: &mut Vec<Instruction>,
        mut functions: HashMap<String, Func<V>>,
    ) -> Option<Value<V>> {
        match self {
            Expression::Constant { value, id } => {
                println!("Resolving a {:?}", value);
                Some(value.clone())
            }
            Expression::Var { name, id } => {
                let (n, mut instrs) = world.get(name.clone());
                instructions.append(&mut instrs);
                if let Some(n) = n {
                    return Some(n);
                }
                env.get_value_rec(name.clone())
            }
            Expression::BinaryOp { lhs, op, rhs, id } => {
                Expression::bin_op(lhs, op, rhs, env, world, instructions, functions)
            }
            Expression::UnaryOp { op, expression, id } => match op {
                UnOp::Negate => match expression.resolve(env, world, instructions, functions)? {
                    Value::Int(value) => Value::Int(-value).into(),
                    Value::Nothing | Value::String(_) | Value::Bool(_) | Value::Custom(_) => {
                        Value::Nothing.into()
                    }
                },
                UnOp::Not => match expression.resolve(env, world, instructions, functions)? {
                    Value::Bool(value) => Value::Bool(!value).into(),
                    Value::String(_) | Value::Int(_) | Value::Nothing | Value::Custom(_) => {
                        Value::Nothing.into()
                    }
                },
            },
            Expression::Call { name, params, id } => {
                let mut a = Vec::new();
                for param in params {
                    let p = param.resolve(env, world, instructions, functions.clone())?;
                    a.push(p);
                }
                let f = functions.get(name).cloned();
                println!(
                    "{}: {:?}, {:?}",
                    name,
                    f.is_some(),
                    functions.keys().collect::<Vec<_>>()
                );
                if let Some(f) = f {
                    let mut enn = env.child();
                    for (n, v) in f.params.iter().zip(a) {
                        enn.set_value(n.clone(), v)
                    }
                    interpreter::interpret_part_2(
                        world,
                        enn,
                        f.statements,
                        instructions,
                        &mut functions,
                    )
                } else {
                    let func = world.functions.get(name)?;
                    let (val, mut instrs) = func(&mut world.t, a);
                    instructions.append(&mut instrs);
                    val.into()
                }
            }
        }
    }

    fn bin_op<T>(
        lhs: &Expression<V>,
        op: &BinOp,
        rhs: &Expression<V>,
        env: &Env<V>,
        world: &mut World<T, V>,
        instructions: &mut Vec<Instruction>,
        functions: HashMap<String, Func<V>>,
    ) -> Option<Value<V>> {
        match op {
            BinOp::Add => Expression::add(lhs, rhs, env, world, instructions, functions),
            BinOp::Subtract => {
                Expression::maths(lhs, |a, b| a - b, rhs, env, world, instructions, functions)
            }
            BinOp::Multiply => {
                Expression::maths(lhs, |a, b| a * b, rhs, env, world, instructions, functions)
            }
            BinOp::Divide => {
                Expression::maths(lhs, |a, b| a / b, rhs, env, world, instructions, functions)
            }
            BinOp::Mod => {
                Expression::maths(lhs, |a, b| a % b, rhs, env, world, instructions, functions)
            }
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
                functions,
            ),

            BinOp::Eq => Some(
                (lhs.resolve(env, world, instructions, functions.clone())?
                    == rhs.resolve(env, world, instructions, functions)?)
                .into(),
            ),
            BinOp::Neq => Some(
                (lhs.resolve(env, world, instructions, functions.clone())?
                    != rhs.resolve(env, world, instructions, functions)?)
                .into(),
            ),

            BinOp::Lt => Some(
                (lhs.resolve(env, world, instructions, functions.clone())?
                    < rhs.resolve(env, world, instructions, functions)?)
                .into(),
            ),
            BinOp::Gt => Some(
                (lhs.resolve(env, world, instructions, functions.clone())?
                    > rhs.resolve(env, world, instructions, functions)?)
                .into(),
            ),
            BinOp::Lte => Some(
                (lhs.resolve(env, world, instructions, functions.clone())?
                    <= rhs.resolve(env, world, instructions, functions)?)
                .into(),
            ),
            BinOp::Gte => Some(
                (lhs.resolve(env, world, instructions, functions.clone())?
                    >= rhs.resolve(env, world, instructions, functions)?)
                .into(),
            ),

            BinOp::And | BinOp::Or | BinOp::Xor => {
                Expression::logics(lhs, op, rhs, env, world, instructions, functions)
            }
        }
    }

    fn add<T>(
        lhs: &Expression<V>,
        rhs: &Expression<V>,
        env: &Env<V>,
        world: &mut World<T, V>,
        instructions: &mut Vec<Instruction>,
        functions: HashMap<String, Func<V>>,
    ) -> Option<Value<V>> {
        return match lhs.resolve(env, world, instructions, functions.clone())? {
            Value::String(s) => {
                let rhs = rhs.resolve(env, world, instructions, functions)?;
                Some(if let Value::String(s2) = rhs {
                    format!("{}{}", s, s2).into()
                } else if let Value::Int(i) = rhs {
                    format!("{}{}", s, i).into()
                } else {
                    Value::Nothing
                })
            }
            Value::Int(i) => Some(
                if let Value::Int(i2) = rhs.resolve(env, world, instructions, functions)? {
                    (i + i2).into()
                } else {
                    Value::Nothing
                },
            ),
            _ => Value::Nothing.into(),
        };
    }

    fn logics<T>(
        lhs: &Expression<V>,
        op: &BinOp,
        rhs: &Expression<V>,
        env: &Env<V>,
        world: &mut World<T, V>,
        instructions: &mut Vec<Instruction>,
        functions: HashMap<String, Func<V>>,
    ) -> Option<Value<V>> {
        match lhs.resolve(env, world, instructions, functions.clone())? {
            Value::Bool(value) => match op {
                BinOp::And => Some(
                    {
                        if !value {
                            false
                        } else if let Value::Bool(v2) =
                            rhs.resolve(env, world, instructions, functions)?
                        {
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
                        } else if let Value::Bool(v2) =
                            rhs.resolve(env, world, instructions, functions)?
                        {
                            v2
                        } else {
                            false
                        }
                    }
                    .into(),
                ),
                BinOp::Xor => Some(
                    {
                        if let Value::Bool(v2) = rhs.resolve(env, world, instructions, functions)? {
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

    fn maths<F, T>(
        lhs: &Expression<V>,
        op: F,
        rhs: &Expression<V>,
        env: &Env<V>,
        world: &mut World<T, V>,
        instructions: &mut Vec<Instruction>,
        functions: HashMap<String, Func<V>>,
    ) -> Option<Value<V>>
    where
        F: FnOnce(i64, i64) -> i64,
    {
        match lhs.resolve(env, world, instructions, functions.clone())? {
            Value::Int(value) => {
                if let Value::Int(v2) = rhs.resolve(env, world, instructions, functions)? {
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
    use crate::environment::Env;
    use crate::expression::{BinOp, Expression};
    use crate::instruction::Instruction;
    use crate::value::{NoCustom, Value};
    use crate::world::World;

    #[test]
    fn add_test() {
        let lhs: Expression<NoCustom> = 5.into();
        let rhs: Expression<NoCustom> = 10.into();
        let op = Expression::BinaryOp {
            lhs: lhs.into(),
            op: BinOp::Add,
            rhs: rhs.into(),
            id: String::from("id"),
        };

        let mut w = World::new(0);
        let te = Env::new();
        let mut instructions: Vec<Instruction> = Vec::new();
        let res = op
            .resolve(&te, &mut w, &mut instructions, Default::default())
            .expect("This should work");
        assert_eq!(res, 15.into())
    }

    #[test]
    fn eq_test() {
        let lhs: Expression<NoCustom> = 5.into();
        let rhs: Expression<NoCustom> = 10.into();
        let op = Expression::BinaryOp {
            lhs: lhs.into(),
            op: BinOp::Eq,
            rhs: rhs.into(),
            id: String::from("id"),
        };

        let mut w = World::new(0);
        let te = Env::new();
        let mut instructions: Vec<Instruction> = Vec::new();

        let res = op
            .resolve(&te, &mut w, &mut instructions, Default::default())
            .expect("This should work");
        assert_eq!(res, false.into())
    }

    #[test]
    fn neq_test() {
        let lhs: Expression<NoCustom> = 5.into();
        let rhs: Expression<NoCustom> = 10.into();
        let op = Expression::BinaryOp {
            lhs: lhs.into(),
            op: BinOp::Neq,
            rhs: rhs.into(),
            id: String::from("id"),
        };

        let mut w = World::new(0);
        let te = Env::new();
        let mut instructions: Vec<Instruction> = Vec::new();

        let res = op
            .resolve(&te, &mut w, &mut instructions, Default::default())
            .expect("This should work");
        assert_eq!(res, true.into())
    }

    #[test]
    fn call_test() {
        let op = Expression::Call {
            name: String::from("test"),
            params: vec![],
            id: String::from("id"),
        };

        let mut w: World<i64, NoCustom> = World::new(0);
        w.functions.insert(String::from("test"), |a, _b| {
            *a += 10;
            (
                Value::Nothing,
                vec![Instruction::Instruction(
                    String::from("test"),
                    String::from("kees"),
                )],
            )
        });
        let te = Env::new();
        let mut instructions: Vec<Instruction> = Vec::new();

        let res = op
            .resolve(&te, &mut w, &mut instructions, Default::default())
            .expect("This should work");
        assert_eq!(res, Value::Nothing);
        assert_eq!(w.t, 10);
        assert_eq!(
            instructions,
            vec![Instruction::Instruction(
                String::from("test"),
                String::from("kees"),
            )]
        );
    }

    #[test]
    fn it_works() {
        let value: Value<NoCustom> = String::from("Hello!").into();
        let expr: Expression<NoCustom> = value.into();
        assert_eq!(expr, Value::String(String::from("Hello!")).into());
    }

    #[test]
    fn add_test_string() {
        let lhs: Expression<NoCustom> = String::from("hello, ").into();
        let rhs: Expression<NoCustom> = String::from("world!").into();
        let mut w = World::new(0);
        let te = Env::new();
        let mut instructions: Vec<Instruction> = Vec::new();

        let res = Expression::BinaryOp {
            lhs: lhs.into(),
            op: BinOp::Add,
            rhs: rhs.into(),
            id: String::from("id"),
        }
        .resolve(&te, &mut w, &mut instructions, Default::default());
        assert_eq!(res, Some(String::from("hello, world!").into()))
    }

    #[test]
    fn add_test_int() {
        let lhs: Expression<NoCustom> = 10.into();
        let rhs: Expression<NoCustom> = 20.into();

        let mut w = World::new(0);
        let te = Env::new();
        let mut instructions: Vec<Instruction> = Vec::new();

        let res = Expression::BinaryOp {
            lhs: lhs.into(),
            op: BinOp::Add,
            rhs: rhs.into(),
            id: String::from("id"),
        }
        .resolve(&te, &mut w, &mut instructions, Default::default());
        assert_eq!(res, Some(30.into()))
    }

    #[test]
    fn add_test_bool() {
        let lhs: Expression<NoCustom> = true.into();
        let rhs: Expression<NoCustom> = false.into();
        let mut w = World::new(0);
        let te = Env::new();
        let mut instructions: Vec<Instruction> = Vec::new();

        let res = Expression::BinaryOp {
            lhs: lhs.into(),
            op: BinOp::Add,
            rhs: rhs.into(),
            id: String::from("id"),
        }
        .resolve(&te, &mut w, &mut instructions, Default::default());
        assert_eq!(res, Some(Value::Nothing))
    }

    #[test]
    fn bool_ops_with_bool() {
        for b1 in [true, false] {
            for b2 in [true, false] {
                let lhs: Expression<NoCustom> = b1.into();
                let rhs: Expression<NoCustom> = b2.into();

                let mut w = World::new(0);
                let te = Env::new();
                let mut instructions: Vec<Instruction> = Vec::new();

                for op in [BinOp::And, BinOp::Or, BinOp::Xor] {
                    let correct_val = match op {
                        BinOp::And => b1 & b2,
                        BinOp::Or => b1 | b2,
                        BinOp::Xor => b1 ^ b2,
                        _ => false,
                    };
                    println!("{:?}", op);
                    let res = Expression::BinaryOp {
                        lhs: lhs.clone().into(),
                        op: op.clone(),
                        rhs: rhs.clone().into(),
                        id: String::from("id"),
                    }
                    .resolve(
                        &te,
                        &mut w,
                        &mut instructions,
                        Default::default(),
                    );

                    assert_eq!(Some(correct_val.into()), res);
                }
            }
        }
    }

    #[test]
    fn var_test() {
        let v: Expression<NoCustom> = Expression::Var {
            name: String::from("x"),
            id: String::from("id"),
        };

        let mut w = World::new(0);
        let mut te = Env::new();
        let mut instructions: Vec<Instruction> = Vec::new();

        te.set_value(String::from("x"), 10.into());
        assert_eq!(
            v.resolve(&te, &mut w, &mut instructions, Default::default()),
            Some(10.into())
        )
    }

    #[test]
    fn serialize_deserialize_test() {
        let expr = Expression::BinaryOp {
            lhs: Expression::Constant {
                value: Value::<NoCustom>::Int(10),
                id: String::from("id"),
            }
            .into(),
            op: BinOp::Add,
            rhs: Expression::Constant {
                value: Value::<NoCustom>::Int(10),
                id: String::from("id"),
            }
            .into(),
            id: String::from("id"),
        };
        let txt = serde_json::to_string(&expr).unwrap();
        println!("{}", txt);
    }
}
