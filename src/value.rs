#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Value {
    String { value: String },
    Int { value: i64 },
    Bool { value: bool },
    Nothing,
}

impl Default for Value {
    fn default() -> Self {
        Self::Nothing
    }
}

impl From<String> for Value {
    fn from(value: String) -> Self {
        Self::String { value }
    }
}

impl From<i64> for Value {
    fn from(value: i64) -> Self {
        Self::Int { value }
    }
}

impl From<bool> for Value {
    fn from(value: bool) -> Self {
        Self::Bool { value }
    }
}
