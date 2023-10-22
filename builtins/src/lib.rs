use ast::{Ast, Node};
use error::Error;

pub const TYPE_COMPARABLE: &'static str = "builtin.comparable";
pub const TYPE_NUMBER: &'static str = "builtin.number";

/// This enum does not match the actual primitive types of `jinko` on purpose, as builtin operators
/// are a little different. For example, we care about whether or not a builtin is implemented on *numbers*,
/// since all operator builtins that exist for integers also exist for floating point numbers.
#[derive(Clone, Copy)]
pub enum BuiltinType {
    Number,     // `int | float`
    Comparable, // `int | float | char | bool`
    Bool,       // `bool`
}

impl BuiltinType {
    pub fn name(&self) -> &str {
        match self {
            BuiltinType::Number => crate::TYPE_NUMBER,
            BuiltinType::Comparable => crate::TYPE_COMPARABLE,
            BuiltinType::Bool => "bool",
        }
    }
}

// Do we need some type information here? probably, right?
pub const ARITHMETIC: &[(&str, BuiltinType)] = &[
    ("+", BuiltinType::Number),
    ("-", BuiltinType::Number),
    ("*", BuiltinType::Number),
    ("/", BuiltinType::Number),
];
pub const COMPARISON: &[(&str, BuiltinType)] = &[
    ("==", BuiltinType::Comparable),
    ("=!", BuiltinType::Comparable),
    ("<=", BuiltinType::Comparable),
    (">=", BuiltinType::Comparable),
    ("<", BuiltinType::Comparable),
    (">", BuiltinType::Comparable),
];
pub const UNARY: &[(&str, BuiltinType)] = &[
    ("!unary", BuiltinType::Bool),
    ("-unary", BuiltinType::Number),
];

mod builder;

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
        let builtin_types = [BuiltinType::Number, BuiltinType::Comparable].map(builder::ty);

        // this creates a list of functions named "+", "-", "*", etc which we
        // can then add to the nodes of our AST.
        let arithmetic_builtins = ARITHMETIC.into_iter().map(|(sym, ty)| {
            builder::function(
                sym,
                vec![
                    // FIXME: Use something else than `ty("int")` here. but what?
                    builder::argument("lhs", builder::ty_arg(*ty)),
                    builder::argument("rhs", builder::ty_arg(*ty)),
                ],
                Some(builder::ty_arg(*ty)),
            )
        });

        let cmp_builtins = COMPARISON.into_iter().map(|(sym, ty)| {
            builder::function(
                sym,
                vec![
                    // FIXME: Use something else than `ty("int")` here. but what?
                    builder::argument("lhs", builder::ty_arg(*ty)),
                    builder::argument("rhs", builder::ty_arg(*ty)),
                ],
                Some(builder::ty_arg(BuiltinType::Bool)),
            )
        });

        // unary operators, like !bool or !int
        let unary_builtins = UNARY.into_iter().map(|(sym, ty)| {
            builder::function(
                sym,
                vec![builder::argument("value", builder::ty_arg(*ty))],
                Some(builder::ty_arg(*ty)),
            )
        });

        // actually, we don't want extend - we want *prepend*. otherwise this causes the
        // potential last expression of the `stmts` to not be the last expression anymore
        new_stmts.extend(builtin_types);
        new_stmts.extend(arithmetic_builtins);
        new_stmts.extend(cmp_builtins);
        new_stmts.extend(unary_builtins);

        // and finally
        new_stmts.extend(stmts.into_iter());

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
