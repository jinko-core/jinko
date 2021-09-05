//! In Jinko, an ObjectInstance represents an actual value in memory.
//! An ObjectInstance has a type, a size, and owns a memory region. In the case of an integer,
//! it would for example be of the primitive type `int`, be of size 8 and contain 8 bytes
//! of "raw" data. Because Jinko is strongly typed, this isn't an issue. An integer will
//! always be an integer.
//! For example, a variable contains an ObjectInstance. Since a variable cannot be uninitialized,
//! the instance is always there. The type of the ObjectInstance might be resolved later, after
//! different passes of the typechecker.

use std::collections::HashMap;

use crate::instruction::TypeDec;
use crate::{ErrKind, Error};

pub type Name = String;
type Ty = Option<TypeDec>;
type Size = usize;
type Offset = usize;

#[derive(Debug, PartialEq, Clone)]
pub struct FieldInstance(Offset, ObjectInstance);

type FieldsMap = HashMap<Name, FieldInstance>;

/// The type is optional. At first, the type might not be known, and will only be
/// revealed during the typechecking phase. `size` is the size of the instance in bytes.
/// It's the same as `data.len()`. `data` is the raw byte value of the instance.
#[derive(Debug, PartialEq, Clone)]
pub struct ObjectInstance {
    ty: Ty,
    size: Size,
    data: Vec<u8>,
    fields: Option<FieldsMap>,
}

impl ObjectInstance {
    /// Create a new, empty instance without a type or a size
    pub fn empty() -> ObjectInstance {
        ObjectInstance::new(None, 0, vec![], None)
    }

    /// Create a new instance
    pub fn new(
        ty: Ty,
        size: usize,
        data: Vec<u8>,
        fields: Option<Vec<(Name, ObjectInstance)>>,
    ) -> ObjectInstance {
        let fields = fields.map(ObjectInstance::fields_vec_to_hash_map);

        ObjectInstance {
            ty,
            size,
            data,
            fields,
        }
    }

    /// Create a new instance from raw bytes instead of a vector
    pub fn from_bytes(
        ty: Ty,
        size: usize,
        data: &[u8],
        fields: Option<Vec<(Name, ObjectInstance)>>,
    ) -> ObjectInstance {
        ObjectInstance::new(ty, size, data.to_vec(), fields)
    }

    /// Get a reference to the type of the instance
    // FIXME: Remove clone
    pub fn ty(&self) -> Option<TypeDec> {
        self.ty.clone()
    }

    /// Set the type of the instance
    pub fn set_ty(&mut self, ty: Ty) {
        self.ty = ty;
    }

    /// Get a reference to the raw data bytes of the ObjectInstance
    pub fn data(&self) -> &[u8] {
        &self.data
    }

    pub fn size(&self) -> Size {
        self.size
    }

    pub fn get_field(&self, field_name: &str) -> Result<ObjectInstance, Error> {
        match self.fields.as_ref() {
            None => {
                Err(Error::new(ErrKind::Context).with_msg(String::from("no fields on instance")))
            }
            Some(fields) => fields.get(field_name).map_or(
                Err(Error::new(ErrKind::Context)
                    .with_msg(format!("field `{}` does not exist on instance", field_name))),
                |FieldInstance(_, instance)| {
                    Ok(instance.clone())
                },
            ),
        }
    }

    pub fn fields(&self) -> &Option<FieldsMap> {
        &self.fields
    }

    // FIXME: Isn't this disgusting
    fn fields_vec_to_hash_map(vec: Vec<(Name, ObjectInstance)>) -> FieldsMap {
        let mut current_offset: usize = 0;
        let mut hashmap = FieldsMap::new();
        for (name, instance) in vec {
            current_offset += instance.size();
            hashmap.insert(name, FieldInstance(current_offset, instance));
        }

        hashmap
    }

    // FIXME: Remove this
    pub fn as_string(&self) -> String {
        let mut base = String::new();

        // FIXME: This *really* needs to be somewhere else
        match &self.ty {
            Some(ty) => base = format!("{}type: {}\n", base, ty.name()),
            None => base = format!("{}type: `no type`\n", base),
        }

        base = format!("{}size: {}\n", base, self.size);
        base.push_str("fields:");

        if let Some(fields) = &self.fields {
            for (name, FieldInstance(_, instance)) in fields {
                base = format!("{}    {}:{}", base, name,
                    instance
                    .as_string()
                );
            }
        }

        base
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
