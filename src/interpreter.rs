use std::collections::HashMap;
use std::fmt::Debug;

use crate::environment::Env;
use crate::instruction::Instruction;
use crate::statement::{Func, Statement};
use crate::value::Value;
use crate::world::World;

pub fn interpret<T, V>(world: &mut World<T, V>, statements: Vec<Statement<V>>)
where
    V: Eq + PartialEq + PartialOrd + Debug + Clone,
{
    let env = Env::new();
    // figure something out for double borrow as mutable?
    let mut instrs = Vec::new();
    let mut f = Default::default();
    interpret_part_2(world, env, statements, &mut instrs, &mut f);
}

pub(crate) fn interpret_part_2<T, V>(
    world: &mut World<T, V>,
    env: Env<V>,
    statements: Vec<Statement<V>>,
    mut instructions: &mut Vec<Instruction>,
    functions: &mut HashMap<String, Func<V>>,
) -> Option<Value<V>>
where
    V: Eq + PartialEq + PartialOrd + Debug + Clone,
{
    let mut e = env.child();
    let mut funcs = functions.clone();
    for statement in statements.clone() {
        if let Statement::FuncDef {
            name: name,
            params: params,
            body: statements,
            id,
        } = statement
        {
            funcs.insert(name, Func { params, statements });
        }
    }
    for statement in statements {
        match statement {
            Statement::Assignment {
                var_name: name,
                expression: expr,
                uses_explicit_var: _,
                id,
            } => {
                let expr = expr.resolve(&e, world, &mut instructions, funcs.clone());
                if let Some(expr) = expr {
                    if let (Some(_), _) = world.get(name.clone()) {
                        let mut instrs = world.set(name, expr);
                        instructions.append(&mut instrs);
                    } else {
                        e.set_value(name, expr);
                    }
                }
            }
            Statement::While {
                condition: expr,
                body: body,
                id,
            } => {
                let mut instrs = Vec::new();
                let mut x = expr
                    .resolve(&(e.clone()), world, &mut instrs, funcs.clone())
                    .map(|x| x.is_true())
                    .unwrap_or(false);
                while x {
                    let res =
                        interpret_part_2(world, e.clone(), body.clone(), &mut instrs, &mut funcs);
                    instructions.append(&mut instrs);
                    if let Some(x) = res {
                        return Some(x);
                    }
                    x = expr
                        .resolve(&(e.clone()), world, &mut instrs, funcs.clone())
                        .map(|x| x.is_true())
                        .unwrap_or(false)
                }
            }
            Statement::FuncDef {
                name: _,
                params: _,
                body: _,
                id,
            } => {}
            Statement::Return { expr, id } => {
                return expr.resolve(&e, world, &mut instructions, funcs)
            }
            Statement::Expression { expr: expr } => {
                expr.resolve(&e, world, &mut instructions, funcs.clone());
            }
            Statement::IfElse {
                condition: expr,
                if_case: if_case,
                else_case: else_case,
                id,
            } => {
                let mut instrs = Vec::new();
                let x = expr
                    .resolve(&(e.clone()), world, &mut instrs, funcs.clone())
                    .map(|x| x.is_true())
                    .unwrap_or(false);
                if x {
                    let res = interpret_part_2(world, e.clone(), if_case, &mut instrs, &mut funcs);
                    instructions.append(&mut instrs);
                    if let Some(x) = res {
                        return Some(x);
                    }
                } else {
                    let res =
                        interpret_part_2(world, e.clone(), else_case, &mut instrs, &mut funcs);
                    instructions.append(&mut instrs);
                    if let Some(x) = res {
                        return Some(x);
                    }
                }
            }
        }
    }

    None
}

#[cfg(test)]
mod tests {
    use crate::expression::{BinOp, Expression};
    use crate::interpreter::interpret;
    use crate::statement::Statement;
    use crate::value::{NoCustom, Value};
    use crate::world::World;

    #[test]
    fn some_func() {
        let statements = vec![
            Statement::FuncDef {
                name: String::from("test"),
                params: Default::default(),
                body: vec![
                    Statement::FuncDef {
                        name: String::from("test2"),
                        params: Default::default(),
                        body: vec![Expression::Call {
                            name: String::from("test3"),
                            params: Default::default(),
                            id: String::from("id"),
                        }
                        .into()],
                        id: String::from("id"),
                    },
                    Expression::Call {
                        name: String::from("test2"),
                        params: Default::default(),
                        id: String::from("id"),
                    }
                    .into(),
                ],
                id: String::from("id"),
            },
            Statement::FuncDef {
                name: String::from("test3"),
                params: Default::default(),
                body: vec![Expression::Call {
                    name: String::from("assert"),
                    params: Default::default(),
                    id: String::from("id"),
                }
                .into()],
                id: String::from("id"),
            },
            Expression::Call {
                name: String::from("test"),
                params: Default::default(),
                id: String::from("id"),
            }
            .into(),
        ];
        let mut w: World<i64, NoCustom> = World::new(0);
        w.functions.insert(String::from("assert"), |a, _b| {
            *a = 10;
            println!("foo!");
            (Value::Nothing, Default::default())
        });
        interpret(&mut w, statements);
        assert_eq!(10, w.t);
    }

    #[test]
    fn custom_eq() {
        #[derive(Debug, Eq, PartialEq, PartialOrd, Clone)]
        enum Foo {
            Bar,
            Baz,
        }

        let v: Expression<_> = Value::Custom(Foo::Bar).into();
        let b: Expression<_> = Value::Custom(Foo::Baz).into();
        let mut w: World<_, _> = World::new(10);
        w.functions.insert(String::from("teq"), |_, vec1| {
            let p1 = &vec1[0];
            assert_eq!(p1.clone(), false.into());
            (Value::Nothing, Vec::new())
        });
        interpret(
            &mut w,
            vec![Statement::Expression {
                expr: Expression::Call {
                    name: String::from("teq"),
                    params: vec![Expression::BinaryOp {
                        lhs: v.into(),
                        op: BinOp::Eq,
                        rhs: b.into(),
                        id: String::from("id"),
                    }],
                    id: String::from("id"),
                },
            }],
        )
    }
}
