//! In Jinko, an ObjectInstance represents an actual value in memory.
//! An ObjectInstance has a type, a size, and owns a memory region. In the case of an integer,
//! it would for example be of the primitive type `int`, be of size 8 and contain 8 bytes
//! of "raw" data. Because Jinko is strongly typed, this isn't an issue. An integer will
//! always be an integer.
//! For example, a variable contains an ObjectInstance. Since a variable cannot be uninitialized,
//! the instance is always there. The type of the ObjectInstance might be resolved later, after
//! different passes of the typechecker.

// FIXME: Use CustomType once @Skallwar's PR is merged
type Ty = String;

/// The type is optional. At first, the type might not be known, and will only be
/// revealed during the typechecking phase. `size` is the size of the instance in bytes.
/// It's the same as `data.len()`. `data` is the raw byte value of the instance.
#[derive(Debug, PartialEq, Clone)]
pub struct ObjectInstance {
    ty: Option<Ty>,
    size: usize,
    data: Vec<u8>,
}

impl ObjectInstance {
    /// Create a new, empty instance without a type or a size
    pub fn empty() -> ObjectInstance {
        ObjectInstance::new(None, 0, vec![])
    }

    /// Create a new instance
    pub fn new(ty: Option<Ty>, size: usize, data: Vec<u8>) -> ObjectInstance {
        ObjectInstance { ty, size, data }
    }

    /// Create a new instance from raw bytes instead of a vector
    pub fn from_bytes(ty: Option<Ty>, size: usize, data: &[u8]) -> ObjectInstance {
        ObjectInstance::new(ty, size, data.to_vec())
    }

    /// Get a reference to the type of the instance
    pub fn ty(&self) -> Option<&Ty> {
        self.ty.as_ref()
    }

    /// Set the type of the instance
    pub fn set_ty(&mut self, ty: Option<Ty>) {
        self.ty = ty;
    }

    /// Get a reference to the raw data bytes of the ObjectInstance
    pub fn data(&self) -> &[u8] {
        &self.data
    }
}

/// Convert a Jinko type to an instance. This is handled by jinko's primitive types
/// as well as user defined ones
pub trait ToObjectInstance {
    fn to_instance(&self) -> ObjectInstance;
}

/// Convert an instance to a jinko type. This is handled by jinko's primitive types
/// as well as user defined ones
pub trait FromObjectInstance {
    fn from_instance(i: &ObjectInstance) -> Self;
}
