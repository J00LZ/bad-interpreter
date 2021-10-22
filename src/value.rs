#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Value {
    String(String),
    Int(i64),
    Bool(bool),
    Nothing,
}

impl Default for Value {
    fn default() -> Self {
        Self::Nothing
    }
}

impl From<String> for Value {
    fn from(value: String) -> Self {
        Self::String(value)
    }
}

impl From<i64> for Value {
    fn from(value: i64) -> Self {
        Self::Int(value)
    }
}

impl From<bool> for Value {
    fn from(value: bool) -> Self {
        Self::Bool(value)
    }
}

impl Value {
    pub fn is_true(&self) -> bool {
        match self {
            Value::String(value) => !(value.is_empty() || value.contains("false")),
            Value::Int(value) => *value == 0,
            Value::Bool(value) => *value,
            Value::Nothing => false,
        }
    }
}
