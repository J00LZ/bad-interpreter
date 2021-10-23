use std::collections::HashMap;
use crate::instruction::Instruction;
use crate::value::Value;

type WorldFunc = fn(&mut World, Vec<Value>) -> (Value, Vec<Instruction>);
pub struct World {
    pub functions: HashMap<String, WorldFunc>,
    pub map: HashMap<String, Value>,
}
