// FIXME: This is invalid
// what's a type? at runtime?
// just the hash of the type? -> that's good enough
//     what's the hash of a type?
// for a string -> the hash of this string
// for a char/int/bool -> the actual value (an i64)
// for a float -> eeeeeeeh? typecheck error?
//     introduce a safe-float type in the stdlib?
// for other types -> needs to be a unique hash -> based on source location and FirId?
type Type = &'static str;

// an instance needs to be unique
// needs to be hashable
// we need the type of the value - it needs to be known at all times
// FIXME: We need to be very careful about what a "Clone" means here
/// An [`Instance`] represents an allocated value in `jinko`, more specifically an instantiation of a type. For example, you can consider `15` to be an instance of `int`.
#[derive(PartialEq, Debug, Clone)]
pub enum Instance {
    /// This variant is used to represent empty types
    Empty,
    Int(i64),
    Float(f64),
    Bool(bool),
    Char(char),
    String(String),
    Other {
        ty: Type,
        data: Vec<u8>,
    },
}

impl Instance {
    pub fn empty() -> Instance {
        Instance::Empty
    }
}

impl From<i64> for Instance {
    fn from(value: i64) -> Instance {
        Instance::Int(value)
    }
}

impl From<f64> for Instance {
    fn from(value: f64) -> Instance {
        Instance::Float(value)
    }
}

impl From<char> for Instance {
    fn from(value: char) -> Instance {
        Instance::Char(value)
    }
}

impl From<bool> for Instance {
    fn from(value: bool) -> Instance {
        Instance::Bool(value)
    }
}

impl From<&str> for Instance {
    fn from(value: &str) -> Instance {
        Instance::String(value.to_owned())
    }
}

impl From<&String> for Instance {
    fn from(value: &String) -> Instance {
        Instance::from(value.as_str())
    }
}
