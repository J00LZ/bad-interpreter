#[derive(Eq, PartialEq, Clone, Debug)]
pub enum Instruction {
    Instruction(String, String),
    Error(String, String),
}
