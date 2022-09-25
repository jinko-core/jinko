use crate::context::Context;
use crate::error::Error;
use crate::instance::{FromObjectInstance, ObjectInstance, ToObjectInstance};
use crate::instruction::{InstrKind, Instruction, Operator};
use crate::location::SpanTuple;
use crate::typechecker::{CheckedType, TypeCheck, TypeCtx, TypeId};
use crate::value::{JkString, Value};

use std::convert::TryFrom;

#[derive(Clone)]
/// A JkConstant represents a primitive type in Jinko. It is used in order to
/// implement integers, floating point numbers, characters, booleans and strings, as
/// well as raw byte values later for custom types.
pub struct JkConstant<T: Clone>(pub(crate) T, CheckedType, Option<SpanTuple>);

impl<T: Clone> JkConstant<T> {
    pub fn rust_value(&self) -> T {
        self.0.clone()
    }

    pub fn set_location(&mut self, location: SpanTuple) {
        self.2 = Some(location)
    }
}

// We can do a generic implementation instead of copy pasting it 5 times.
// However, this part of the rust compiler is still not ready
// (https://github.com/rust-lang/rust/issues/43408)
//
// Here is the generic implementation:
//
// ```
// impl<T: Sized> ToObjectInstance for JkConstant<T> {
//     fn to_instance(&self) -> ObjectInstance {
//         use std::mem::{size_of, transmute};
//
//         unsafe {
//             ObjectInstance::from_bytes(
//                 None,
//                 size_of::<T>(),
//                 &transmute::<T, [u8; size_of::<T>()]>(self.0),
//             )
//         }
//     }
// }
//
// /* (Plus a specific implementation for JkConstant<String>) */
// ```
//
// which fails with the following error message:
//
// ```
// error[E0277]: the size for values of type `T` cannot be known at compilation time
// --> src/value/jink_constant.rs:34:48
//     |
// 25  | impl <T: Sized> ToObjectInstance for JkConstant<T> {
//     |       - this type parameter needs to be `Sized`
//     ...
// 34  |         &transmute::<T, [u8; size_of::<T>()]>(self.0),
//     |                                        ^ doesn't have a size known at compile-time
// ```

/// Circumvents the need for a generic implementation (see comment).
/// Call it with the type contained in the JkConstant and the &str representation
///
/// ```ignore
/// // Implements a JkConstant<i64> with type displayed as "int"
/// jk_primitive!(i64, "int");
/// ```
macro_rules! jk_primitive {
    // Special implementation for JkBool, in order to have as_bool()
    (bool) => {
        impl ToObjectInstance for JkConstant<bool> {
            fn to_instance(&self) -> ObjectInstance {
                use std::mem::{size_of, transmute};

                unsafe {
                    ObjectInstance::from_bytes(
                        CheckedType::Resolved(TypeId::from("bool")),
                        size_of::<bool>(),
                        &transmute::<bool, [u8; size_of::<bool>()]>(self.0),
                        None,
                    )
                }
            }
        }

        impl FromObjectInstance for JkConstant<bool> {
            fn from_instance(i: &ObjectInstance) -> Self {
                use std::mem::{size_of, transmute};

                unsafe {
                    Self::from(transmute::<[u8; size_of::<bool>()], bool>(
                        TryFrom::try_from(i.data()).unwrap(),
                    ))
                }
            }
        }

        impl From<bool> for JkConstant<bool> {
            fn from(value: bool) -> JkConstant<bool> {
                JkConstant(value, CheckedType::Resolved(TypeId::from("bool")), None)
            }
        }

        impl Instruction for JkConstant<bool> {
            fn kind(&self) -> InstrKind {
                InstrKind::Expression(None)
            }

            fn print(&self) -> String {
                self.0.to_string()
            }

            fn execute(&self, _ctx: &mut Context) -> Option<ObjectInstance> {
                // Since we cannot use the generic ToObjectInstance implementation, we also have to
                // copy paste our four basic implementations for jinko's primitive types...
                Some(self.to_instance())
            }

            fn location(&self) -> Option<&SpanTuple> {
                self.2.as_ref()
            }
        }

        impl TypeCheck for JkConstant<bool> {
            fn resolve_type(&mut self, _: &mut TypeCtx) -> Result<CheckedType, Error> {
                Ok(CheckedType::Resolved(TypeId::from("bool")))
            }

            fn set_cached_type(&mut self, _: CheckedType) {}

            fn cached_type(&self) -> Option<&CheckedType> {
                Some(&self.1)
            }
        }
    };
    (char) => {
        impl ToObjectInstance for JkConstant<char> {
            fn to_instance(&self) -> ObjectInstance {
                use std::mem::{size_of, transmute};

                unsafe {
                    ObjectInstance::from_bytes(
                        CheckedType::Resolved(TypeId::from("char")),
                        size_of::<char>(),
                        &transmute::<char, [u8; size_of::<char>()]>(self.0),
                        None,
                    )
                }
            }
        }

        impl FromObjectInstance for JkConstant<char> {
            fn from_instance(i: &ObjectInstance) -> Self {
                use std::mem::{size_of, transmute};

                unsafe {
                    Self::from(transmute::<[u8; size_of::<char>()], char>(
                        TryFrom::try_from(i.data()).unwrap(),
                    ))
                }
            }
        }

        impl From<char> for JkConstant<char> {
            fn from(value: char) -> JkConstant<char> {
                JkConstant(value, CheckedType::Resolved(TypeId::from("char")), None)
            }
        }

        impl Instruction for JkConstant<char> {
            fn kind(&self) -> InstrKind {
                InstrKind::Expression(None)
            }

            fn print(&self) -> String {
                self.0.to_string()
            }

            fn execute(&self, ctx: &mut Context) -> Option<ObjectInstance> {
                ctx.debug("CONSTANT", &self.0.to_string());

                // Since we cannot use the generic ToObjectInstance implementation, we also have to
                // copy paste our four basic implementations for jinko's primitive types...
                Some(self.to_instance())
            }

            fn location(&self) -> Option<&SpanTuple> {
                self.2.as_ref()
            }
        }

        impl TypeCheck for JkConstant<char> {
            fn resolve_type(&mut self, _: &mut TypeCtx) -> Result<CheckedType, Error> {
                Ok(CheckedType::Resolved(TypeId::from("char")))
            }

            fn set_cached_type(&mut self, _: CheckedType) {}

            fn cached_type(&self) -> Option<&CheckedType> {
                Some(&self.1)
            }
        }
    };
    ($t:ty, $s:expr) => {
        impl ToObjectInstance for JkConstant<$t> {
            fn to_instance(&self) -> ObjectInstance {
                use std::mem::size_of;

                ObjectInstance::from_bytes(
                    CheckedType::Resolved(TypeId::from($s)),
                    size_of::<$t>(),
                    &self.0.to_ne_bytes(),
                    None,
                )
            }
        }

        impl FromObjectInstance for JkConstant<$t> {
            fn from_instance(i: &ObjectInstance) -> Self {
                use std::mem::{size_of, transmute};

                unsafe {
                    Self::from(transmute::<[u8; size_of::<$t>()], $t>(
                        TryFrom::try_from(i.data()).unwrap(),
                    ))
                }
            }
        }

        impl Instruction for JkConstant<$t> {
            fn kind(&self) -> InstrKind {
                InstrKind::Expression(None)
            }

            fn print(&self) -> String {
                self.0.to_string()
            }

            fn execute(&self, _ctx: &mut Context) -> Option<ObjectInstance> {
                // Since we cannot use the generic ToObjectInstance implementation, we also have to
                // copy paste our four basic implementations for jinko's primitive types...
                Some(self.to_instance())
            }

            fn location(&self) -> Option<&SpanTuple> {
                self.2.as_ref()
            }
        }

        impl TypeCheck for JkConstant<$t> {
            fn resolve_type(&mut self, _: &mut TypeCtx) -> Result<CheckedType, Error> {
                Ok(CheckedType::Resolved(TypeId::from($s)))
            }

            fn set_cached_type(&mut self, _: CheckedType) {}

            fn cached_type(&self) -> Option<&CheckedType> {
                Some(&self.1)
            }
        }

        impl From<$t> for JkConstant<$t> {
            fn from(rust_value: $t) -> Self {
                JkConstant(rust_value, CheckedType::Resolved(TypeId::from($s)), None)
            }
        }
    };
}

jk_primitive!(i64, "int");
jk_primitive!(f64, "float");
jk_primitive!(char);
jk_primitive!(bool);

impl Value for JkConstant<i64> {
    fn do_op(&self, other: &Self, op: Operator) -> Result<ObjectInstance, Error> {
        match op {
            Operator::Add => Ok(JkConstant::from(self.0 + other.0).to_instance()),
            Operator::Sub => Ok(JkConstant::from(self.0 - other.0).to_instance()),
            Operator::Mul => Ok(JkConstant::from(self.0 * other.0).to_instance()),
            Operator::Div => Ok(JkConstant::from(self.0 / other.0).to_instance()),
            Operator::Lt => Ok(JkConstant::from(self.0 < other.0).to_instance()),
            Operator::Gt => Ok(JkConstant::from(self.0 > other.0).to_instance()),
            Operator::LtEq => Ok(JkConstant::from(self.0 <= other.0).to_instance()),
            Operator::GtEq => Ok(JkConstant::from(self.0 >= other.0).to_instance()),
            Operator::Equals => Ok(JkConstant::from(self.0 == other.0).to_instance()),
            Operator::NotEquals => Ok(JkConstant::from(self.0 != other.0).to_instance()),
            _ => self.no_op(other, op),
        }
    }
}

impl Value for JkConstant<f64> {
    fn do_op(&self, other: &Self, op: Operator) -> Result<ObjectInstance, Error> {
        match op {
            Operator::Add => Ok(JkConstant::from(self.0 + other.0).to_instance()),
            Operator::Sub => Ok(JkConstant::from(self.0 - other.0).to_instance()),
            Operator::Mul => Ok(JkConstant::from(self.0 * other.0).to_instance()),
            Operator::Div => Ok(JkConstant::from(self.0 / other.0).to_instance()),
            Operator::Lt => Ok(JkConstant::from(self.0 < other.0).to_instance()),
            Operator::Gt => Ok(JkConstant::from(self.0 > other.0).to_instance()),
            Operator::LtEq => Ok(JkConstant::from(self.0 <= other.0).to_instance()),
            Operator::GtEq => Ok(JkConstant::from(self.0 >= other.0).to_instance()),
            // TODO: Do we want to allow float equality comparison?
            _ => self.no_op(other, op),
        }
    }
}

impl ToObjectInstance for JkString {
    fn to_instance(&self) -> ObjectInstance {
        ObjectInstance::from_bytes(
            CheckedType::Resolved(TypeId::from("string")),
            self.0.as_bytes().len(),
            self.0.as_bytes(),
            None,
        )
    }
}

impl FromObjectInstance for JkString {
    fn from_instance(i: &ObjectInstance) -> Self {
        // unchecked is safe because this instance came from a utf8 string in ToObjectInstance
        unsafe { JkString::from(String::from_utf8_unchecked(i.data().to_vec())) }
    }
}

impl Instruction for JkString {
    fn kind(&self) -> InstrKind {
        InstrKind::Expression(None)
    }

    fn print(&self) -> String {
        format!("\"{}\"", self.0.clone())
    }

    fn execute(&self, _ctx: &mut Context) -> Option<ObjectInstance> {
        Some(self.to_instance())
    }

    fn location(&self) -> Option<&SpanTuple> {
        self.2.as_ref()
    }
}

impl TypeCheck for JkString {
    fn resolve_type(&mut self, _ctx: &mut TypeCtx) -> Result<CheckedType, Error> {
        Ok(CheckedType::Resolved(TypeId::from("string")))
    }

    fn set_cached_type(&mut self, _: CheckedType) {}

    fn cached_type(&self) -> Option<&CheckedType> {
        Some(&self.1)
    }
}

impl From<&str> for JkConstant<String> {
    fn from(s: &str) -> Self {
        JkConstant(
            s.to_string(),
            CheckedType::Resolved(TypeId::from("string")),
            None,
        )
    }
}

impl From<String> for JkConstant<String> {
    fn from(s: String) -> Self {
        JkConstant(s, CheckedType::Resolved(TypeId::from("string")), None)
    }
}

#[cfg(test)]
mod tests {
    use crate::jinko;
    use crate::value::{JkBool, JkFloat, JkInt};

    use super::*;

    #[test]
    fn tc_string_type() {
        let mut ctx = Context::new(Box::new(crate::io_trait::JkStdReader));
        let mut s = JkString::from("that's a jk string");

        assert_eq!(
            ctx.type_check(&mut s).unwrap(),
            CheckedType::Resolved(TypeId::from("string"))
        );
    }

    #[test]
    fn tc_bool_type() {
        let mut ctx = Context::new(Box::new(crate::io_trait::JkStdReader));
        let mut s = JkBool::from(false);

        assert_eq!(
            ctx.type_check(&mut s).unwrap(),
            CheckedType::Resolved(TypeId::from("bool"))
        );
    }

    #[test]
    fn tc_i_type() {
        let mut ctx = Context::new(Box::new(crate::io_trait::JkStdReader));
        let mut s = JkInt::from(0);

        assert_eq!(
            ctx.type_check(&mut s).unwrap(),
            CheckedType::Resolved(TypeId::from("int"))
        );
    }

    #[test]
    fn tc_f_type() {
        let mut ctx = Context::new(Box::new(crate::io_trait::JkStdReader));
        let mut s = JkFloat::from(15.4);

        assert_eq!(
            ctx.type_check(&mut s).unwrap(),
            CheckedType::Resolved(TypeId::from("float"))
        );
    }

    #[test]
    fn tc_primitives_available() {
        jinko! {
            b = true;
            i = 15;
            f = 4.5;
            c = 'c';
            s = "jinko";
        };
    }
}
