use std::collections::hash_map::Entry;
use std::collections::HashMap;

use expression::Expression;
use value::Value;

mod expression;
mod value;

pub trait Environment {
    fn get_value(&self, key: String) -> Option<Value>;
    fn set_value(&mut self, key: String, value: Value, here: bool);

    fn as_env(&mut self) -> Option<&mut Env>;
}

pub struct Env<'a> {
    map: HashMap<String, Value>,
    parent: &'a mut dyn Environment,
}

impl<'a> Environment for Env<'a> {
    fn get_value(&self, key: String) -> Option<Value> {
        self.map
            .get(&key)
            .cloned()
            .or_else(|| self.parent.get_value(key))
    }
    fn set_value(&mut self, key: String, value: Value, here: bool) {
        if here {
            self.map.insert(key, value);
            return;
        }
        let mut e: &mut dyn Environment = self;
        while let Some(env) = e.as_env() {
            if let Entry::Occupied(mut e) = env.map.entry(key.clone()) {
                e.insert(value);
                return;
            }
            e = env.parent;
        }
        self.map.insert(key, value);
    }

    fn as_env(&mut self) -> Option<&'a mut Env> {
        Some(self)
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Statement {
    Assignment(String, Expression, bool),
    While(Expression, Vec<Statement>),
    FuncDef(String, Vec<String>, Vec<Statement>),
    Return(Expression),
    Expression(Expression),
}

type WorldFunc = fn(World, Vec<Value>) -> Value;
pub struct World {
    pub functions: HashMap<String, WorldFunc>,
    pub map: HashMap<String, Value>,
}

impl Environment for World {
    fn get_value(&self, key: String) -> Option<Value> {
        self.map.get(&key).cloned()
    }

    fn set_value(&mut self, key: String, value: Value, _: bool) {
        self.map.insert(key, value);
    }

    fn as_env(&mut self) -> Option<&mut Env> {
        None
    }
}

pub struct Interpreter {
    map: HashMap<String, World>,
}

pub fn interpret(world: String, instructions: Vec<Statement>) {}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use crate::expression::BinOp;
    use crate::{Env, Environment, Expression, Value};

    struct TestEnv(HashMap<String, Value>);

    impl Environment for TestEnv {
        fn get_value(&self, key: String) -> Option<Value> {
            self.0.get(key.as_str()).cloned()
        }
        fn set_value(&mut self, key: String, value: Value, _: bool) {
            self.0.insert(key, value);
        }

        fn as_env(&mut self) -> Option<&mut Env> {
            None
        }
    }

    impl TestEnv {
        fn new() -> Self {
            Self(HashMap::new())
        }
    }

    #[test]
    fn it_works() {
        let value: Value = String::from("Hello!").into();
        let expr: Expression = value.into();
        assert_eq!(
            expr,
            Expression::Constant(Value::String(String::from("Hello!")))
        );
    }

    #[test]
    fn add_test_string() {
        let lhs: Expression = String::from("hello, ").into();
        let rhs: Expression = String::from("world!").into();
        let te = TestEnv::new();
        let res = Expression::BinaryOp(lhs.into(), BinOp::Add, rhs.into()).resolve(&te);
        assert_eq!(res, Some(String::from("hello, world!").into()))
    }

    #[test]
    fn add_test_int() {
        let lhs: Expression = 10.into();
        let rhs: Expression = 20.into();
        let te = TestEnv::new();
        let res = Expression::BinaryOp(lhs.into(), BinOp::Add, rhs.into()).resolve(&te);
        assert_eq!(res, Some(30.into()))
    }

    #[test]
    fn add_test_bool() {
        let lhs: Expression = true.into();
        let rhs: Expression = false.into();
        let te = TestEnv::new();
        let res = Expression::BinaryOp(lhs.into(), BinOp::Add, rhs.into()).resolve(&te);
        assert_eq!(res, Some(Value::Nothing))
    }

    #[test]
    fn bool_ops_with_bool() {
        for b1 in [true, false] {
            for b2 in [true, false] {
                let lhs: Expression = b1.into();
                let rhs: Expression = b2.into();
                let te = TestEnv::new();
                for op in [BinOp::And, BinOp::Or, BinOp::Xor] {
                    let correct_val = match op {
                        BinOp::And => b1 & b2,
                        BinOp::Or => b1 | b2,
                        BinOp::Xor => b1 ^ b2,
                        _ => false,
                    };
                    println!("{:?}", op);
                    let res =
                        Expression::BinaryOp(lhs.clone().into(), op.clone(), rhs.clone().into())
                            .resolve(&te);

                    assert_eq!(Some(correct_val.into()), res);
                }
            }
        }
    }

    #[test]
    fn var_test() {
        let v = Expression::Var(String::from("x"));
        let mut te = TestEnv::new();
        te.set_value(String::from("x"), 10.into(), false);
        assert_eq!(v.resolve(&te), Some(10.into()))
    }
}
