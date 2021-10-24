use std::fmt::Debug;

use crate::expression::Expression;
use serde::ser::{SerializeStruct, SerializeStructVariant};
use serde::{Deserialize, Serialize, Serializer};

#[derive(Debug, Eq, PartialEq, Clone, Deserialize)]
#[serde(rename_all = "snake_case", untagged)]
pub enum Statement<V>
where
    V: Eq + PartialEq + PartialOrd + Debug + Clone,
{
    Assignment {
        var_name: String,
        expression: Expression<V>,
        uses_explicit_var: bool,
        #[serde(default)]
        id: String,
    },
    While {
        condition: Expression<V>,
        body: Vec<Statement<V>>,
        #[serde(default)]
        id: String,
    },
    FuncDef {
        name: String,
        params: Vec<String>,
        body: Vec<Statement<V>>,
        #[serde(default)]
        id: String,
    },
    IfElse {
        condition: Expression<V>,
        if_case: Vec<Statement<V>>,
        else_case: Vec<Statement<V>>,
        #[serde(default)]
        id: String,
    },
    Return {
        expr: Expression<V>,
        #[serde(default)]
        id: String,
    },

    Expression {
        #[serde(flatten)]
        expr: Expression<V>,
    },
}

impl<V> Serialize for Statement<V>
where
    V: Eq + PartialEq + PartialOrd + Debug + Clone + Serialize,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        match self {
            Statement::Assignment {
                var_name,
                expression,
                uses_explicit_var,
                id,
            } => {
                let mut s = serializer.serialize_struct("Assignment", 4)?;
                s.serialize_field("type", "assignment")?;
                s.serialize_field("var_name", var_name)?;
                s.serialize_field("expression", expression)?;
                s.serialize_field("uses_explicit_var", uses_explicit_var)?;
                s.serialize_field("id", id)?;
                s.end()
            }
            Statement::While {
                condition,
                body,
                id,
            } => {
                let mut s = serializer.serialize_struct("While", 3)?;
                s.serialize_field("type", "while")?;
                s.serialize_field("condition", condition)?;
                s.serialize_field("body", body)?;
                s.serialize_field("id", id)?;
                s.end()
            }
            Statement::FuncDef {
                name,
                params,
                body,
                id,
            } => {
                let mut s = serializer.serialize_struct("FuncDef", 4)?;
                s.serialize_field("type", "funcdef")?;
                s.serialize_field("name", name)?;
                s.serialize_field("params", params)?;
                s.serialize_field("body", body)?;
                s.serialize_field("id", id)?;
                s.end()
            }
            Statement::IfElse {
                condition,
                if_case,
                else_case,
                id,
            } => {
                let mut s = serializer.serialize_struct("IfElse", 4)?;
                s.serialize_field("type", "ifelse")?;
                s.serialize_field("condition", condition)?;
                s.serialize_field("if_case", if_case)?;
                s.serialize_field("else_case", else_case)?;
                s.serialize_field("id", id)?;
                s.end()
            }
            Statement::Return { expr, id } => {
                let mut s = serializer.serialize_struct("Return", 2)?;
                s.serialize_field("type", "return")?;
                s.serialize_field("expr", expr)?;
                s.serialize_field("id", id)?;
                s.end()
            }
            Statement::Expression { expr } => {
                serializer.serialize_newtype_struct("Expression", expr)
            }
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Func<V>
where
    V: Eq + PartialEq + PartialOrd + Debug + Clone,
{
    pub params: Vec<String>,
    pub statements: Vec<Statement<V>>,
}

impl<V> From<Expression<V>> for Statement<V>
where
    V: Eq + PartialEq + PartialOrd + Debug + Clone,
{
    fn from(x: Expression<V>) -> Self {
        Self::Expression { expr: x }
    }
}

#[cfg(test)]
mod tests {
    use crate::expression::{BinOp, Expression};
    use crate::statement::Statement;
    use crate::value::{NoCustom, Value};

    #[test]
    fn serialize_deserialize_test() {
        let expr = Statement::While {
            condition: true.into(),
            body: vec![Statement::Expression {
                expr: Expression::BinaryOp {
                    lhs: Expression::Constant {
                        value: Value::<NoCustom>::Int(10),
                        id: String::from("id"),
                    }
                    .into(),
                    op: BinOp::Add,
                    rhs: Expression::Constant {
                        value: Value::<NoCustom>::Int(10),
                        id: String::from("id"),
                    }
                    .into(),
                    id: String::from("id"),
                },
            }],
            id: String::from("id"),
        };
        let txt = serde_json::to_string(&expr).unwrap();
        println!("{}", txt);
        let deser: Statement<NoCustom> = serde_json::from_str(&txt).unwrap();
        assert_eq!(expr, deser);
    }

    #[test]
    fn parse_example() {
        let x = r#"
        [
  {
    "type": "assignment",
    "var_name": "x",
    "expression": {
      "type": "binaryop",
      "lhs": {
        "type": "constant",
        "const": {
          "type": "int",
          "value": 10
        },
        "id": "null"
      },
      "rhs": {
        "type": "binaryop",
        "lhs": {
          "type": "constant",
          "const": {
            "type": "int",
            "value": 3
          },
          "id": "null"
        },
        "rhs": {
          "type": "constant",
          "const": {
            "type": "int",
            "value": 7
          },
          "id": "null"
        },
        "op": "multiply",
        "id": "test2"
      },
      "op": "add",
      "id": "test3"
    },
    "uses_explicit_var": false,
    "id": "test1"
  },
  {
    "type": "while",
    "condition": {
      "type": "binaryop",
      "lhs": {
        "type": "var",
        "name": "x",
        "id": "test4"
      },
      "rhs": {
        "type": "constant",
        "const": {
          "type": "int",
          "value": 3
        },
        "id": "test5"
      },
      "op": "lte",
      "id": "test6"
    },
    "body": [
      {
        "type": "call",
        "name": "print",
        "params": [
          {
            "type": "var",
            "name": "x",
            "id": "null"
          }
        ],
        "id": "null"
      },
      {
        "type": "assignment",
        "var_name": "x",
        "expression": {
          "type": "binaryop",
          "lhs": {
            "type": "var",
            "name": "x",
            "id": "null"
          },
          "rhs": {
            "type": "constant",
            "const": {
              "type": "int",
              "value": 1
            },
            "id": "null"
          },
          "op": "subtract",
          "id": "null"
        },
        "uses_explicit_var": false,
        "id": "null"
      }
    ],
    "id": "null"
  }
]
        "#;
        let z: Vec<Statement<NoCustom>> = serde_json::from_str(x).unwrap();
    }
}
