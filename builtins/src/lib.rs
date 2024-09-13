use ast::{Ast, Node};
use error::Error;

use std::slice;

mod builder;

pub const TYPE_COMPARABLE: &str = "builtin.comparable";
pub const TYPE_EQUALABLE: &str = "builtin.comparable";
pub const TYPE_NUMBER: &str = "builtin.number";

macro_rules! name {
    ($name:tt -> $str:literal) => {
        pub const $name: &str = $str;
    };
}

pub mod name {
    name![ADD -> "+"];
    name![SUB -> "-"];
    name![MUL -> "*"];
    name![DIV -> "/"];

    name![EQ -> "=="];
    name![NE -> "!="];

    name![LT -> "<"];
    name![LTE -> "<="];
    name![GT -> ">"];
    name![GTE -> ">="];

    name![NOT -> "!unary"];
    name![MIN -> "-unary"];
}

/// This enum does not match the actual primitive types of `jinko` on purpose, as builtin operators
/// are a little different. For example, we care about whether or not a builtin is implemented on *numbers*,
/// since all operator builtins that exist for integers also exist for floating point numbers.
#[derive(Clone, Copy)]
pub enum BuiltinType {
    Number,     // `int | float`
    Comparable, // `int | char`
    Equalable,  // `int | char | bool | string`
    Bool,       // `bool`
}

impl BuiltinType {
    pub fn name(&self) -> &str {
        match self {
            BuiltinType::Number => crate::TYPE_NUMBER,
            BuiltinType::Comparable => crate::TYPE_COMPARABLE,
            BuiltinType::Equalable => crate::TYPE_EQUALABLE,
            BuiltinType::Bool => "bool",
        }
    }
}

pub enum Arithmetic {
    Add,
    Sub,
    Mul,
    Div,
}

impl Arithmetic {
    pub fn as_str(&self) -> &'static str {
        match self {
            Arithmetic::Add => name::ADD,
            Arithmetic::Sub => name::SUB,
            Arithmetic::Mul => name::MUL,
            Arithmetic::Div => name::DIV,
        }
    }

    pub fn iter() -> slice::Iter<'static, Arithmetic> {
        static VALUES: &[Arithmetic] = &[
            Arithmetic::Add,
            Arithmetic::Sub,
            Arithmetic::Mul,
            Arithmetic::Div,
        ];

        VALUES.iter()
    }
}

pub enum Mode {
    OrEqual,
    Strict,
}

pub enum Comparison {
    Equals,
    Differs,
    LessThan(Mode),
    GreaterThan(Mode),
}

impl Comparison {
    pub fn as_str(&self) -> &'static str {
        match self {
            Comparison::Equals => name::EQ,
            Comparison::Differs => name::NE,
            Comparison::LessThan(Mode::Strict) => name::LT,
            Comparison::LessThan(Mode::OrEqual) => name::LTE,
            Comparison::GreaterThan(Mode::Strict) => name::GT,
            Comparison::GreaterThan(Mode::OrEqual) => name::GTE,
        }
    }

    pub fn iter() -> slice::Iter<'static, Comparison> {
        static VALUES: &[Comparison] = &[
            Comparison::Equals,
            Comparison::Differs,
            Comparison::LessThan(Mode::Strict),
            Comparison::LessThan(Mode::OrEqual),
            Comparison::GreaterThan(Mode::Strict),
            Comparison::GreaterThan(Mode::OrEqual),
        ];

        VALUES.iter()
    }
}

pub enum Unary {
    Not,
    Minus,
}

impl Unary {
    pub fn as_str(&self) -> &'static str {
        match self {
            Unary::Not => name::NOT,
            Unary::Minus => name::MIN,
        }
    }

    pub fn ty(&self) -> BuiltinType {
        match self {
            Unary::Not => BuiltinType::Bool,
            Unary::Minus => BuiltinType::Number,
        }
    }

    pub fn iter() -> slice::Iter<'static, Unary> {
        static VALUES: &[Unary] = &[Unary::Not, Unary::Minus];

        VALUES.iter()
    }
}

pub enum Operator {
    Arithmetic(Arithmetic),
    Comparison(Comparison),
    Unary(Unary),
}

impl Operator {
    pub fn as_str(&self) -> &'static str {
        match self {
            Operator::Arithmetic(inner) => inner.as_str(),
            Operator::Comparison(inner) => inner.as_str(),
            Operator::Unary(inner) => inner.as_str(),
        }
    }

    pub fn ty(&self) -> BuiltinType {
        match self {
            Operator::Arithmetic(_) => BuiltinType::Number,
            Operator::Comparison(Comparison::Equals)
            | Operator::Comparison(Comparison::Differs) => BuiltinType::Equalable,
            Operator::Comparison(_) => BuiltinType::Comparable,
            Operator::Unary(Unary::Not) => BuiltinType::Bool,
            Operator::Unary(Unary::Minus) => BuiltinType::Number,
        }
    }

    pub fn try_from_str(s: &str) -> Option<Operator> {
        use Arithmetic::*;
        use Comparison::*;
        use Unary::*;

        use Operator as Op;

        match s {
            name::ADD => Some(Op::Arithmetic(Add)),
            name::SUB => Some(Op::Arithmetic(Sub)),
            name::MUL => Some(Op::Arithmetic(Mul)),
            name::DIV => Some(Op::Arithmetic(Div)),

            name::EQ => Some(Op::Comparison(Equals)),
            name::NE => Some(Op::Comparison(Differs)),
            name::LT => Some(Op::Comparison(LessThan(Mode::Strict))),
            name::LTE => Some(Op::Comparison(LessThan(Mode::OrEqual))),
            name::GT => Some(Op::Comparison(GreaterThan(Mode::Strict))),
            name::GTE => Some(Op::Comparison(GreaterThan(Mode::OrEqual))),

            name::NOT => Some(Op::Unary(Not)),
            name::MIN => Some(Op::Unary(Minus)),
            _ => None,
        }
    }
}

pub trait AppendAstBuiltins: Sized {
    fn append_builtins(self) -> Result<Self, Error>;
}

impl AppendAstBuiltins for Ast {
    fn append_builtins(self) -> Result<Self, Error> {
        let mut new_stmts = vec![];

        let (stmts, last_is_expr) = match self.node {
            Node::Block {
                stmts,
                last_is_expr,
            } => (stmts, last_is_expr),
            _ => unreachable!(),
        };

        // the types used by the builtin arithmetic and comparison functions
        let builtin_types = [
            (
                BuiltinType::Number,
                vec![builder::type_symbol("int"), builder::type_symbol("float")],
            ),
            (
                BuiltinType::Comparable,
                vec![
                    builder::type_symbol("int"),
                    builder::type_symbol("float"),
                    builder::type_symbol("char"),
                    builder::type_symbol("bool"),
                ],
            ),
        ]
        .map(|(builtin, variants)| builder::union_type(builtin, variants));

        // this creates a list of functions named "+", "-", "*", etc which we
        // can then add to the nodes of our AST.
        let arithmetic_builtins = Arithmetic::iter().map(|op| {
            builder::function(
                op.as_str(),
                vec![
                    builder::argument("lhs", builder::builtin_type_symbol(BuiltinType::Number)),
                    builder::argument("rhs", builder::builtin_type_symbol(BuiltinType::Number)),
                ],
                Some(builder::builtin_type_symbol(BuiltinType::Number)),
            )
        });

        let cmp_builtins = Comparison::iter().map(|op| {
            builder::function(
                op.as_str(),
                vec![
                    builder::argument("lhs", builder::builtin_type_symbol(BuiltinType::Comparable)),
                    builder::argument("rhs", builder::builtin_type_symbol(BuiltinType::Comparable)),
                ],
                Some(builder::builtin_type_symbol(BuiltinType::Bool)),
            )
        });

        // unary operators, like !bool or -int
        let unary_builtins = Unary::iter().map(|op| {
            builder::function(
                op.as_str(),
                vec![builder::argument(
                    "value",
                    builder::builtin_type_symbol(op.ty()),
                )],
                Some(builder::builtin_type_symbol(op.ty())),
            )
        });

        // actually, we don't want extend - we want *prepend*. otherwise this causes the
        // potential last expression of the `stmts` to not be the last expression anymore
        new_stmts.extend(builtin_types);
        new_stmts.extend(arithmetic_builtins);
        new_stmts.extend(cmp_builtins);
        new_stmts.extend(unary_builtins);

        // and finally
        new_stmts.extend(stmts);

        Ok(Ast {
            node: Node::Block {
                stmts: new_stmts,
                last_is_expr,
            },
            ..self
        })
    }
}

#[cfg(test)]
mod tests {}
