use std::collections::HashMap;
use crate::environment::Env;
use crate::statement::Statement;
use crate::world::World;

pub struct Interpreter {
    map: HashMap<String, World>,
}

pub fn interpret(world: &mut World, instructions: Vec<Statement>) {
    let env = Env::new();
    // figure something out for double borrow as mutable?
    interpret_part_2(world, env, instructions)
}

fn interpret_part_2(mut world: &mut World, mut env: Env, statements: Vec<Statement>) {}
