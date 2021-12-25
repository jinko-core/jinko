//! The [`Generic`] trait is required to enable the generic type expansion of
//! variables, type and function declarations, type instantiations and function
//! calls. Its basic goal is to replace all instances of `T` with an actual [`TypeId`].

use std::collections::HashMap;

use crate::instruction::{Instruction, TypeId};
use crate::Context;
use crate::{log, ErrKind, Error};

pub type GenericMap = HashMap<TypeId, TypeId>;

/// Mangle a name to resolve it to its proper expanded name.
/// The format used is the following:
/// <name> '+' <T0> '+' <T1>...
pub fn _mangle(name: &str, types: &[TypeId]) -> String {
    let mut mangled = String::from(name);
    for type_id in types.iter() {
        mangled.push('+');
        mangled.push_str(type_id.id());
    }

    log!("mangled: {}", &mangled);

    mangled
}

/// Create a generic map from two sets of [`TypeId`]s: The set of generics to resolve,
/// and the set of resolved generics.
pub fn create_map(
    generics: &[TypeId],
    resolved: &[TypeId],
    ctx: &mut Context,
) -> Result<GenericMap, Error> {
    if generics.len() != resolved.len() {
        // FIXME: Add better error message here printing both sets of generics
        let err_msg = String::from("missing types in generic expansion");
        return Err(Error::new(ErrKind::Generics).with_msg(err_msg));
    }

    let mut map = GenericMap::new();

    generics.iter().zip(resolved).for_each(|(l_ty, r_ty)| {
        log!("mapping generic: {} <- {}", l_ty, r_ty);
        match map.insert(l_ty.clone(), r_ty.clone()) {
            None => {}
            Some(existing_ty) => ctx.error(Error::new(ErrKind::Generics).with_msg(format!(
                "mapping type to already mapped generic type: {} <- {} with {}",
                l_ty, existing_ty, r_ty
            ))),
        }
    });

    Ok(map)
}

/// Since most of the instructions cannot do generic expansion, we can implement
/// default methods which do nothing. This avoid more boilerplate code for instructions
/// such as constants or variables which cannot be generic.
pub trait Generic {
    /// Expand all generics contained in the current instruction, or its sub-instructions.
    /// For example, a [`Block`] is responsible for expanding the generics of all the
    /// functions defined within itself.
    fn expand(&self, _ctx: &mut Context) {}

    /// Mutate an instruction in order to resolve to the proper, expanded generic instruction.
    /// For example, a call to the function `f[T]` should now be replaced by a call to
    /// the function `generics::mangle("f", GenericMap { TypeId "T" })`
    fn resolve_self(&mut self, _type_map: GenericMap) {}

    /// Generate a new instruction from itself, according to a given type map. The generated
    /// instruction should not contain any generic types. In the case of the following
    /// function declaration:
    ///
    /// ```ignore
    /// func generic_f[T](a: T);
    /// ```
    ///
    /// With a typemap linking the type `T` to the type `float`, this call would generate
    /// the following function declaration:
    ///
    /// ```ignore
    /// func generic_f+float(a: float); // according to the mangling rules
    /// ```
    fn generate_new(&self, _type_map: GenericMap) -> Option<Box<dyn Instruction>> {
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::instruction::TypeId;

    macro_rules! ty {
        ($str:literal) => {
            TypeId::new(String::from($str))
        };
    }

    #[test]
    fn mangle_no_generics() {
        assert_eq!(_mangle("mangled", &[]), "mangled");
    }

    #[test]
    fn mangle_one_generic() {
        assert_eq!(_mangle("mangled", &[ty!("bool")]), "mangled+bool");
    }

    #[test]
    fn mangle_multi_generic() {
        assert_eq!(
            _mangle("mangled", &[ty!("float"), ty!("ComplexType")]),
            "mangled+float+ComplexType"
        );
    }

    #[test]
    fn create_map_different_size() {
        let mut ctx = Context::new();

        assert!(create_map(&[ty!("int"), ty!("float")], &[ty!("T")], &mut ctx).is_err());
    }
}
