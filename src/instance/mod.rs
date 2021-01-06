//! In Jinko, an Instance represents an actual value in memory.
//! An Instance has a type, a size, and owns a memory region. In the case of an integer,
//! it would for example be of the primitive type `int`, be of size 8 and contain 8 bytes
//! of "raw" data. Because Jinko is strongly typed, this isn't an issue. An integer will
//! always be an integer.
//! For example, a variable contains an Instance. Since a variable cannot be uninitialized,
//! the instance is always there. The type of the Instance might be resolved later, after
//! different passes of the typechecker.

// FIXME: Use CustomType once @Skallwar's PR is merged
type Ty = String;

/// The type is optional. At first, the type might not be known, and will only be
/// revealed during the typechecking phase. `size` is the size of the instance in bytes.
/// It's the same as `data.len()`. `data` is the raw byte value of the instance.
#[derive(Debug, PartialEq)]
pub struct Instance {
    ty: Option<Ty>,
    size: usize,
    data: Vec<u8>,
}

impl Instance {
    /// Get a reference to the type of the instance
    pub fn ty(&self) -> Option<&Ty> {
        self.ty.as_ref()
    }

    /// Set the type of the instance
    pub fn set_ty(&mut self, ty: Option<Ty>) {
        self.ty = ty;
    }
}
