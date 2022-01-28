//! In Jinko, an ObjectInstance represents an actual value in memory.
//! An ObjectInstance has a type, a size, and owns a memory region. In the case of an integer,
//! it would for example be of the primitive type `int`, be of size 8 and contain 8 bytes
//! of "raw" data. Because Jinko is strongly typed, this isn't an issue. An integer will
//! always be an integer.
//! For example, a variable contains an ObjectInstance. Since a variable cannot be uninitialized,
//! the instance is always there. The type of the ObjectInstance might be resolved later, after
//! different passes of the typechecker.

use std::collections::HashMap;

use crate::typechecker::CheckedType;
use crate::{ErrKind, Error, Indent};

pub type Name = String;
type Offset = usize;

#[derive(Debug, PartialEq, Clone)]
pub struct FieldInstance(Offset, ObjectInstance);

impl FieldInstance {
    pub fn offset(&self) -> &Offset {
        &self.0
    }

    pub fn instance(&self) -> &ObjectInstance {
        &self.1
    }
}

type FieldsMap = HashMap<Name, FieldInstance>;

/// The type is optional. At first, the type might not be known, and will only be
/// revealed during the typechecking phase. `size` is the size of the instance in bytes.
/// It's the same as `data.len()`. `data` is the raw byte value of the instance.
#[derive(Debug, PartialEq, Clone)]
pub struct ObjectInstance {
    ty: CheckedType,
    size: usize,
    data: Vec<u8>,
    fields: Option<FieldsMap>,
}

impl ObjectInstance {
    /// Create a new, empty instance without a type or a size
    pub fn empty() -> ObjectInstance {
        ObjectInstance::new(CheckedType::Error, 0, vec![], None)
    }

    /// Create a new instance
    pub fn new(
        ty: CheckedType,
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
        ty: CheckedType,
        size: usize,
        data: &[u8],
        fields: Option<Vec<(Name, ObjectInstance)>>,
    ) -> ObjectInstance {
        ObjectInstance::new(ty, size, data.to_vec(), fields)
    }

    /// Get a reference to the type of the instance
    pub fn ty(&self) -> &CheckedType {
        &self.ty
    }

    /// Set the type of the instance
    pub fn set_ty(&mut self, ty: CheckedType) {
        self.ty = ty;
    }

    /// Get a reference to the raw data bytes of the ObjectInstance
    pub fn data(&self) -> &[u8] {
        &self.data
    }

    pub fn size(&self) -> usize {
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
                |FieldInstance(_, instance)| Ok(instance.clone()),
            ),
        }
    }

    pub fn fields(&self) -> &Option<FieldsMap> {
        &self.fields
    }

    fn fields_vec_to_hash_map(vec: Vec<(Name, ObjectInstance)>) -> FieldsMap {
        let mut current_offset: usize = 0;
        let mut hashmap = FieldsMap::new();
        for (name, instance) in vec {
            let inst_size = instance.size();
            hashmap.insert(name, FieldInstance(current_offset, instance));
            current_offset += inst_size;
        }

        hashmap
    }

    fn as_string_inner(instance: &ObjectInstance, indent: Indent) -> String {
        let mut base = String::new();

        match &instance.ty {
            CheckedType::Resolved(ty) => base = format!("{}{}type: {}\n", base, indent, ty.id()),
            _ => base = format!("{}{}type: `no type`\n", base, indent),
        }

        base = format!("{}{}size: {}\n", base, indent, instance.size);

        if let Some(fields) = &instance.fields {
            base = format!("{}{}fields:\n", base, indent);

            for (name, FieldInstance(_, instance)) in fields {
                base = format!(
                    "{}{}{}:\n{}",
                    base,
                    indent,
                    name,
                    ObjectInstance::as_string_inner(instance, indent.increment()),
                );
            }
        }

        base
    }

    pub fn as_string(&self) -> String {
        ObjectInstance::as_string_inner(self, Indent::default())
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{parser::constructs, span, Context, JkInt};

    fn setup() -> Context {
        let mut ctx = Context::new();

        let inst = constructs::expr(span!("type Point(x: int, y: int);"))
            .unwrap()
            .1;
        inst.execute(&mut ctx);

        let inst = constructs::expr(span!("type Vec2(f: Point, s: Point);"))
            .unwrap()
            .1;
        inst.execute(&mut ctx);

        let inst = constructs::expr(span!("p = Point(x: 1, y: 2)")).unwrap().1;
        inst.execute(&mut ctx);

        let inst = constructs::expr(span!("v = Vec2(f: p, s: p)")).unwrap().1;
        inst.execute(&mut ctx);

        ctx
    }

    #[test]
    fn t_one_deep_access() {
        let mut ctx = setup();

        let inst = constructs::expr(span!("p")).unwrap().1;
        let p = inst.execute(&mut ctx).unwrap();

        let inst = constructs::expr(span!("v")).unwrap().1;
        let v = inst.execute(&mut ctx).unwrap();
        let v_f = v.get_field("f").unwrap();
        let v_s = v.get_field("s").unwrap();

        assert_eq!(v_f, p);
        assert_eq!(v_s, p);
    }

    #[test]
    fn t_two_deep_access() {
        let mut ctx = setup();

        let inst = constructs::expr(span!("v")).unwrap().1;
        let v = inst.execute(&mut ctx).unwrap();
        let v_f = v.get_field("f").unwrap();
        let v_s = v.get_field("s").unwrap();
        let v_f_x = v_f.get_field("x").unwrap();
        let v_f_y = v_s.get_field("y").unwrap();

        assert_eq!(v_f_x, JkInt::from(1).to_instance());
        assert_eq!(v_f_y, JkInt::from(2).to_instance());
    }
}
