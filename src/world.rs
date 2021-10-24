use crate::instruction::Instruction;
use crate::value::Value;
use std::collections::HashMap;

/// The functions that can work on the world
pub type WorldFunc<T, V> =
    HashMap<String, fn(&mut T, Vec<Value<V>>) -> (Value<V>, Vec<Instruction>)>;
/// The function called when a variable is read from the world, the instructions are ignored if
/// the option value is `Option::None`.
pub type GetWorldVar<T, V> = fn(&mut T, String) -> (Option<Value<V>>, Vec<Instruction>);
/// The function that is called to set a variable in the world, it will not be called if it's
/// get version returns `Option::None` (so if the var does not already exist, it will not be "set".
pub type SetWorldVar<T, V> = fn(&mut T, String, Value<V>) -> Vec<Instruction>;
pub struct World<T, V>
where
    V: Eq + PartialEq + PartialOrd + Clone,
{
    pub functions: WorldFunc<T, V>,
    pub get: GetWorldVar<T, V>,
    pub set: SetWorldVar<T, V>,
    pub t: T,
}

impl<T, V> World<T, V>
where
    V: Eq + PartialEq + PartialOrd + Clone,
{
    pub fn new(t: T) -> Self {
        Self {
            functions: Default::default(),
            get: |_, _| (None, Vec::new()),
            set: |_, _, _| Vec::new(),
            t,
        }
    }

    pub fn get(&mut self, key: String) -> (Option<Value<V>>, Vec<Instruction>) {
        let g = self.get;
        g(&mut self.t, key)
    }

    pub fn set(&mut self, key: String, value: Value<V>) -> Vec<Instruction> {
        let s = self.set;
        s(&mut self.t, key, value)
    }
}
