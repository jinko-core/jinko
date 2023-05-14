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
// FIXME: We can probably improve this type by specializing it more - turning it into a sum type differentiating between
// FIXME: We need to be very careful about what a "Clone" means here
#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Instance {
    ty: Type,
    data: Vec<u8>,
}

impl Instance {
    pub fn empty() -> Instance {
        Instance {
            ty: "empty type",
            data: Vec::new(),
        }
    }

    pub fn data(&self) -> &[u8] {
        &self.data
    }
}

impl From<i64> for Instance {
    fn from(value: i64) -> Instance {
        Instance {
            ty: "int",
            // origin: node.origin,
            data: value.to_le_bytes().to_vec(),
        }
    }
}

impl From<&str> for Instance {
    fn from(value: &str) -> Instance {
        Instance {
            ty: "string",
            // orgin: node.origin,
            data: value.as_bytes().to_owned(),
        }
    }
}

impl From<&String> for Instance {
    fn from(value: &String) -> Instance {
        Instance::from(value.as_str())
    }
}
