use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use expression::Expression;
use value::Value;

use crate::instruction::Instruction;
use crate::statement::Statement;

mod expression;
mod instruction;
mod statement;
mod value;

pub trait Environment {
    fn get_value(&self, key: String) -> Option<Value>;
    fn set_value(&mut self, key: String, value: Value, here: bool);

    fn as_env(&mut self) -> Option<&mut Env>;
}

#[derive(Debug, PartialEq)]
pub struct Inner {
    contents: HashMap<String, Value>,
    parent: Option<Env>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Env(Rc<RefCell<Inner>>);

impl Env {
    pub fn new() -> Self {
        Self(Rc::new(RefCell::new(Inner {
            contents: Default::default(),
            parent: None,
        })))
    }

    pub fn child(&self) -> Self {
        Self(Rc::new(RefCell::new(Inner {
            contents: Default::default(),
            parent: Some(self.clone()),
        })))
    }

    pub fn get_value(&self, key: String) -> Option<Value> {
        self.0.borrow().contents.get(&key).cloned()
    }

    pub fn get_value_rec(&self, key: String) -> Option<Value> {
        if let Some(x) = self.get_value(key.clone()) {
            Some(x)
        } else if let Some(e) = &self.0.borrow().parent {
            e.get_value_rec(key)
        } else {
            None
        }
    }

    pub fn set_value(&mut self, key: String, value: Value) {
        self.0.borrow_mut().contents.insert(key, value);
    }
}

type WorldFunc = fn(&mut World, Vec<Value>) -> (Value, Vec<Instruction>);
pub struct World {
    pub functions: HashMap<String, WorldFunc>,
    pub map: HashMap<String, Value>,
}

pub struct Interpreter {
    map: HashMap<String, World>,
}

pub fn interpret(world: &mut World, instructions: Vec<Statement>) {
    let env = Env::new();
    // figure something out for double borrow as mutable?
    interpret_part_2(world, env, instructions)
}

fn interpret_part_2(mut world: &mut World, mut env: Env, statements: Vec<Statement>) {}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use crate::expression::BinOp;
    use crate::instruction::Instruction;
    use crate::{Env, Environment, Expression, Value, World};

    pub struct TestEnv(HashMap<String, Value>);

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
        pub fn new() -> Self {
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
        let mut w = World {
            functions: Default::default(),
            map: Default::default(),
        };
        let te = Env::new();
        let mut instructions: Vec<Instruction> = Vec::new();

        let res = Expression::BinaryOp(lhs.into(), BinOp::Add, rhs.into()).resolve(
            &te,
            &mut w,
            &mut instructions,
        );
        assert_eq!(res, Some(String::from("hello, world!").into()))
    }

    #[test]
    fn add_test_int() {
        let lhs: Expression = 10.into();
        let rhs: Expression = 20.into();
        
        let mut w = World {
            functions: Default::default(),
            map: Default::default(),
        };
        let te = Env::new();
        let mut instructions: Vec<Instruction> = Vec::new();

        let res = Expression::BinaryOp(lhs.into(), BinOp::Add, rhs.into()).resolve(
            &te,
            &mut w,
            &mut instructions,
        );
        assert_eq!(res, Some(30.into()))
    }

    #[test]
    fn add_test_bool() {
        let lhs: Expression = true.into();
        let rhs: Expression = false.into();
        let mut w = World {
            functions: Default::default(),
            map: Default::default(),
        };
        let te = Env::new();
        let mut instructions: Vec<Instruction> = Vec::new();

        let res = Expression::BinaryOp(lhs.into(), BinOp::Add, rhs.into()).resolve(
            &te,
            &mut w,
            &mut instructions,
        );
        assert_eq!(res, Some(Value::Nothing))
    }

    #[test]
    fn bool_ops_with_bool() {
        for b1 in [true, false] {
            for b2 in [true, false] {
                let lhs: Expression = b1.into();
                let rhs: Expression = b2.into();
                
                let mut w = World {
                    functions: Default::default(),
                    map: Default::default(),
                };
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
                    let res =
                        Expression::BinaryOp(lhs.clone().into(), op.clone(), rhs.clone().into())
                            .resolve(&te, &mut w, &mut instructions);

                    assert_eq!(Some(correct_val.into()), res);
                }
            }
        }
    }

    #[test]
    fn var_test() {
        let v = Expression::Var(String::from("x"));
        
        let mut w = World {
            functions: Default::default(),
            map: Default::default(),
        };
        let mut te = Env::new();
        let mut instructions: Vec<Instruction> = Vec::new();

        te.set_value(String::from("x"), 10.into());
        assert_eq!(v.resolve(&te, &mut w, &mut instructions), Some(10.into()))
    }
}
