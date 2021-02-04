//! In Jinko, an Instance represents an actual value in memory.
//! An Instance has a type, a size, and owns a memory region. In the case of an integer,
//! it would for example be of the primitive type `int`, be of size 8 and contain 8 bytes
//! of "raw" data. Because Jinko is strongly typed, this isn't an issue. An integer will
//! always be an integer.
//! For example, a variable contains an Instance. Since a variable cannot be uninitialized,
//! the instance is always there. The type of the Instance might be resolved later, after
//! different passes of the typechecker.

use super::value::JinkConstant;
use crate::Instruction;

use std::collections::HashMap;

// FIXME: Use CustomType once @Skallwar's PR is merged
type Ty = String;
pub type Name = String;
type Offset = usize;
pub type Size = usize;
type FieldsMap = HashMap<Name, (Offset, Size)>;

/// The type is optional. At first, the type might not be known, and will only be
/// revealed during the typechecking phase. `size` is the size of the instance in bytes.
/// It's the same as `data.len()`. `data` is the raw byte value of the instance.
#[derive(Debug, PartialEq, Clone)]
pub struct Instance {
    ty: Option<Ty>,
    size: Size,
    data: Vec<u8>,
    fields: Option<FieldsMap>,
}

impl Instance {
    /// Create a new, empty instance without a type or a size
    pub fn empty() -> Instance {
        Instance::new(None, 0, vec![], None)
    }

    /// Create a new instance
    pub fn new(
        ty: Option<Ty>,
        size: usize,
        data: Vec<u8>,
        fields: Option<Vec<(Name, Size)>>,
    ) -> Instance {
        let fields = fields.map(|vec| Instance::fields_vec_to_hash_map(vec));

        Instance {
            ty,
            size,
            data,
            fields,
        }
    }

    /// Create a new instance from raw bytes instead of a vector
    pub fn from_bytes(
        ty: Option<Ty>,
        size: usize,
        data: &[u8],
        fields: Option<Vec<(Name, Size)>>,
    ) -> Instance {
        Instance::new(ty, size, data.to_vec(), fields)
    }

    /// Get a reference to the type of the instance
    pub fn ty(&self) -> Option<&Ty> {
        self.ty.as_ref()
    }

    /// Set the type of the instance
    pub fn set_ty(&mut self, ty: Option<Ty>) {
        self.ty = ty;
    }

    /// Get a reference to the raw data bytes of the Instance
    pub fn data(&self) -> &[u8] {
        &self.data
    }

    pub fn size(&self) -> Size {
        self.size
    }

    fn fields_vec_to_hash_map(vec: Vec<(String, Size)>) -> FieldsMap {
        let mut current_offset: usize = 0;
        let mut hashmap = FieldsMap::new();
        for (name, size) in vec {
            hashmap.insert(name, (current_offset, size));
            current_offset += size;
        }

        hashmap
    }
}

/// Convert a Jinko type to an instance. This is handled by jinko's primitive types
/// as well as user defined ones
pub trait ToInstance {
    fn to_instance(&self) -> Instance;
}

/// Convert an instance to a jinko type. This is handled by jinko's primitive types
/// as well as user defined ones
pub trait FromInstance {
    fn from_instance(i: &Instance) -> Self;
}

// FIXME:
// - Is Display really how we want to go about it?
// - Should the implementation reside here and not in the repl crate?
// - Cleanup the code
impl std::fmt::Display for Instance {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self.ty() {
                Some(ty) => match ty.as_ref() {
                    "int" => JinkConstant::<i64>::from_instance(self).print(),
                    "float" => JinkConstant::<f64>::from_instance(self).print(),
                    "char" => JinkConstant::<char>::from_instance(self).print(),
                    "string" => JinkConstant::<String>::from_instance(self).print(),
                    "bool" => JinkConstant::<bool>::from_instance(self).print(),
                    _ => format!("{:?}", self),
                },
                None => format!(""),
            }
        )
    }
}
