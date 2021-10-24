use std::cmp::Ordering;
use std::fmt::Debug;

#[derive(Debug, Eq, PartialEq, Clone, PartialOrd)]
pub struct NoCustom;

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Value<V>
where
    V: Eq + PartialEq + PartialOrd + Clone,
{
    String(String),
    Int(i64),
    Bool(bool),
    Custom(V),
    Nothing,
}

impl<V> Default for Value<V>
where
    V: Eq + PartialEq + PartialOrd + Clone,
{
    fn default() -> Self {
        Self::Nothing
    }
}

impl<V> From<String> for Value<V>
where
    V: Eq + PartialEq + PartialOrd + Clone,
{
    fn from(value: String) -> Self {
        Self::String(value)
    }
}

impl<V> From<i64> for Value<V>
where
    V: Eq + PartialEq + PartialOrd + Clone,
{
    fn from(value: i64) -> Self {
        Self::Int(value)
    }
}

impl<V> From<bool> for Value<V>
where
    V: Eq + PartialEq + PartialOrd + Clone,
{
    fn from(value: bool) -> Self {
        Self::Bool(value)
    }
}

impl<V> Value<V>
where
    V: Eq + PartialEq + PartialOrd + Clone,
{
    pub fn is_true(&self) -> bool {
        match self {
            Value::String(value) => !(value.is_empty() || value.contains("false")),
            Value::Int(value) => *value == 0,
            Value::Bool(value) => *value,
            Value::Custom(_) => false,
            Value::Nothing => false,
        }
    }
}

impl<V> PartialOrd for Value<V>
where
    V: Eq + PartialEq + PartialOrd + Clone,
{
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
            Value::Custom(t1) => {
                if let Value::Custom(t2) = other {
                    t1.partial_cmp(t2)
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
