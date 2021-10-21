use std::collections::HashMap;

use expression::{BinOp, Expression};
use value::Value;
use std::collections::hash_map::Entry;

mod expression;
mod value;

pub trait Environment {
    fn get_value(&self, key: String) -> Value;
    fn set_value(&mut self, key: String, value: Value, here: bool);

    fn as_env(&mut self) -> Option<&mut Env>;
}

pub struct Env<'a> {
    map: HashMap<String, Value>,
    parent: &'a mut dyn Environment,
}

impl<'a> Environment for Env<'a> {
    fn get_value(&self, key: String) -> Value {
        self.map
            .get(&key)
            .cloned()
            .unwrap_or_else(|| self.parent.get_value(key))
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
            } else {
                e = env.parent;
            }
        }
        self.map.insert(key, value);
    }

    fn as_env(&mut self) -> Option<&'a mut Env> {
        Some(self)
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
enum Statement {
    Assignment(String, Expression, bool),
    While(Expression, Vec<Statement>),
    FuncDef(String, Vec<String>, Vec<Statement>),
    Return(Expression),
    Expression(Expression),
}

pub struct Interpreter;

impl Interpreter {}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use crate::{BinOp, Env, Environment, Expression, Value};

    struct TestEnv(HashMap<String, Value>);

    impl Environment for TestEnv {
        fn get_value(&self, key: String) -> Value {
            self.0
                .get(key.as_str())
                .unwrap_or_else(|| &Value::Nothing)
                .clone()
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
            Expression::Constant(Value::String {
                value: String::from("Hello!")
            })
        );
    }

    #[test]
    fn add_test() {
        let lhs: Expression = String::from("hello, ").into();
        let rhs: Expression = String::from("world!").into();
        let te = TestEnv::new();
        let res = Expression::BinaryOp(lhs.into(), BinOp::Add, rhs.into()).resolve(&te);
        assert_eq!(res, String::from("hello, world!").into())
    }

    #[test]
    fn var_test() {
        let v = Expression::Var(String::from("x"));
        let mut te = TestEnv::new();
        te.set_value(String::from("x"), 10.into(), false);
        assert_eq!(v.resolve(&te), 10.into())
    }
}
