//! A [`TypeId`] refers to a type's identifier. For example, the [`TypeId`] of `int` is "int".
//! The [`TypeId`] of `type Custom(a: int, b: OtherCustom)` is "Custom".
//! These identifiers are not simple strings, as they help in identifying which
//! type is being used. This means that the identifier must also keep a list of
//! generics that might be associated with the type's name, or, in the case of
//! function-like types, a list of arguments and maybe a return type.
//! These identifiers should be constructed gradually, in order to improve parsing
//! performance. For example, when parsing `f` in the following function:
//! ```ignore
//! func takes_functor[T](f: func[T](T, string) -> T, s: string) { ... }
//! ```
//! we may first assume that `f`'s type identifier is the type "func", with one
//! generic argument. Then, as we start parsing the argument types, we add them
//! to our existing [`TypeId`] using a builder pattern.
//! ```
//! use jinko::{typechecker::TypeId, symbol::Symbol};
//!
//! let t = TypeId::new(Symbol::from(String::from("func")));                  // Type
//! let t = t.with_generic(TypeId::new(Symbol::from(String::from("T"))));     // Type
//! let t = t.with_arg(TypeId::new(Symbol::from(String::from("T"))));         // Functor
//! let t = t.with_arg(TypeId::new(Symbol::from(String::from("string"))));    // Functor
//! let t = t.with_return_type(TypeId::new(Symbol::from(String::from("T")))); // Functor
//! ```

use std::fmt::{Display, Formatter, Result as FmtResult};
use std::hash::Hash;

use colored::Colorize;

use crate::instruction::TypeDec;
use crate::symbol::Symbol;

pub const PRIMITIVE_TYPES: [&str; 5] = ["bool", "int", "float", "char", "string"];

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
// FIXME: They should probably have location info
pub enum TypeId {
    Type {
        id: Symbol,
        generics: Vec<TypeId>,
    },
    Functor {
        generics: Vec<TypeId>,
        arg_types: Vec<TypeId>,
        return_type: Option<Box<TypeId>>,
    },
}

impl TypeId {
    // Create a new, empty [`TypeId`] simply from a symbol. The type id will
    // always represent a [`TypeId::Type`] at first
    pub fn new(id: Symbol) -> TypeId {
        TypeId::Type {
            id,
            generics: vec![],
        }
    }

    // Create a new empty function-like [`TypeId`] from a symbol
    pub fn functor() -> TypeId {
        TypeId::Functor {
            generics: vec![],
            arg_types: vec![],
            return_type: None,
        }
    }

    /// Add a generic type to a consumed [`TypeId`]'s generic list
    pub fn with_generic(self, generic: TypeId) -> TypeId {
        match self {
            TypeId::Type { id, generics } => {
                let mut new_generics = generics;
                new_generics.push(generic);

                TypeId::Type {
                    id,
                    generics: new_generics,
                }
            }
            TypeId::Functor {
                generics,
                arg_types,
                return_type,
            } => {
                let mut new_generics = generics;
                new_generics.push(generic);

                TypeId::Functor {
                    generics: new_generics,
                    arg_types,
                    return_type,
                }
            }
        }
    }

    /// Add an argument type to a consumed [`TypeId`]. If the type id represented
    /// a [`TypeId::Type`], then it becomes a [`TypeId::Functor`]. Otherwise,
    /// the argument type is added to the functor's argument types
    pub fn with_arg(self, arg: TypeId) -> TypeId {
        match self {
            TypeId::Type { generics, .. } => TypeId::Functor {
                generics,
                arg_types: vec![],
                return_type: None,
            },
            TypeId::Functor {
                generics,
                arg_types,
                return_type,
            } => {
                let mut new_args = arg_types;
                new_args.push(arg);

                TypeId::Functor {
                    generics,
                    arg_types: new_args,
                    return_type,
                }
            }
        }
    }

    /// Add a return type to a consumed [`TypeId`]. If the type id represented
    /// a [`TypeId::Type`], then it becomes a [`TypeId::Functor`]. Otherwise,
    /// the functor's return type is replaced
    pub fn with_return_type(self, ret: TypeId) -> TypeId {
        match self {
            TypeId::Type { generics, .. } => TypeId::Functor {
                generics,
                arg_types: vec![],
                return_type: Some(Box::new(ret)),
            },
            TypeId::Functor {
                generics,
                arg_types,
                ..
            } => TypeId::Functor {
                generics,
                arg_types,
                return_type: Some(Box::new(ret)),
            },
        }
    }

    pub fn id(&self) -> &str {
        match self {
            TypeId::Type { id, .. } => id.access(),
            // FIXME: Should we store "func" in a symbol?
            _ => "func",
        }
    }

    pub fn void() -> TypeId {
        TypeId::new(Symbol::from(String::from("void")))
    }

    #[deprecated]
    pub fn is_primitive(&self) -> bool {
        PRIMITIVE_TYPES.contains(&self.id())
    }
}

// FIXME: We should remove all of these once we actually use a proper
// symbol framework

impl From<&str> for TypeId {
    fn from(s: &str) -> Self {
        TypeId::new(Symbol::from(s.to_string()))
    }
}

impl From<&TypeDec> for TypeId {
    fn from(td: &TypeDec) -> Self {
        TypeId::from(td.name())
    }
}

impl From<TypeDec> for TypeId {
    fn from(td: TypeDec) -> Self {
        TypeId::from(&td)
    }
}

impl Display for TypeId {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "{}", self.id().purple())?;

        let generics = match self {
            TypeId::Type { generics, .. } | TypeId::Functor { generics, .. } => generics,
        };

        if !generics.is_empty() {
            write!(f, "[")?;
            write!(f, "{}", generics[0])?;

            generics
                .iter()
                .skip(1)
                .try_for_each(|generic| write!(f, ", {}", generic))?;

            write!(f, "]")?;
        }

        Ok(())
    }
}
