use std::cmp::Ordering;

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

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match self {
            Value::String(s1) => {
                if let Value::String(s2) = other {
                    Some(s1.cmp(s2))
                } else {
                    None
                }
            }
            Value::Int(i1) => {
                if let Value::Int(i2) = other {
                    Some(i1.cmp(i2))
                } else {
                    None
                }
            }
            Value::Bool(b1) => {
                if let Value::Bool(b2) = other {
                    Some(b1.cmp(b2))
                } else {
                    None
                }
            }
            Value::Nothing => {
                if let Value::Nothing = other {
                    Some(Ordering::Equal)
                } else {
                    None
                }
            }
        }
    }
}
