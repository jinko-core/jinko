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

/// Call it with the type contained in the JkConstant and the &str representation
///
/// ```ignore
/// // Implements a JkConstant<i64> with type displayed as "int"
/// jk_primitive!(i64, "int");
/// ```
macro_rules! jk_primitive {
    (@toinstance $self:expr, $t:ty => "int" $size:expr) => {
        ObjectInstance::from_bytes(
            CheckedType::Resolved(TypeId::from("int")),
            $size,
            &$self.0.to_ne_bytes(),
            None,
        )
    };
    (@toinstance $self:expr, $t:ty => "float" $size:expr) => {
        ObjectInstance::from_bytes(
            CheckedType::Resolved(TypeId::from("float")),
            $size,
            &$self.0.to_ne_bytes(),
            None,
        )
    };
    (@toinstance $self:expr, $t:ty => $ty_name:literal $size:expr) => {
        unsafe {
            ObjectInstance::from_bytes(
                CheckedType::Resolved(TypeId::from($ty_name)),
                $size,
                &std::mem::transmute::<$t, [u8; $size]>($self.0),
                None,
            )
        }
    };
    (@instantiate $t:ty => $ty_name:tt $size:expr) => {
        impl ToObjectInstance for JkConstant<$t> {
            fn to_instance(&self) -> ObjectInstance {
                jk_primitive!(@toinstance self, $t => $ty_name $size)
            }
        }

        impl FromObjectInstance for JkConstant<$t> {
            fn from_instance(i: &ObjectInstance) -> Self {
                unsafe {
                    Self::from(std::mem::transmute::<[u8; $size], $t>(
                        TryFrom::try_from(i.data()).unwrap(),
                    ))
                }
            }
        }
    };

    ($rust_type:ty, $jk_type_name:tt) => {
        jk_primitive!(@instantiate $rust_type => $jk_type_name std::mem::size_of::<$rust_type>());

        impl Instruction for JkConstant<$rust_type> {
            fn kind(&self) -> InstrKind {
                InstrKind::Expression(None)
            }

            fn print(&self) -> String {
                self.0.to_string()
            }

            fn execute(&self, _ctx: &mut Context) -> Option<ObjectInstance> {
                Some(self.to_instance())
            }

            fn location(&self) -> Option<&SpanTuple> {
                self.2.as_ref()
            }
        }

        impl TypeCheck for JkConstant<$rust_type> {
            fn resolve_type(&mut self, _: &mut TypeCtx) -> Result<CheckedType, Error> {
                Ok(CheckedType::Resolved(TypeId::from($jk_type_name)))
            }

            fn set_cached_type(&mut self, _: CheckedType) {}

            fn cached_type(&self) -> Option<&CheckedType> {
                Some(&self.1)
            }
        }

        impl From<$rust_type> for JkConstant<$rust_type> {
            fn from(rust_value: $rust_type) -> Self {
                JkConstant(rust_value, CheckedType::Resolved(TypeId::from($jk_type_name)), None)
            }
        }
    };
}

jk_primitive!(i64, "int");
jk_primitive!(f64, "float");
jk_primitive!(char, "char");
jk_primitive!(bool, "bool");

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
