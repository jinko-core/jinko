//! The [`Generic`] trait is required to enable the generic type expansion of
//! variables, type and function declarations, type instantiations and function
//! calls. Its basic goal is to replace all instances of `T` with an actual [`TypeId`].

use std::collections::HashMap;

use crate::instruction::TypeId;
use crate::{log, ErrKind, Error};
use crate::{Context, TypeCtx};

#[derive(Default)]
pub struct GenericMap {
    map: HashMap<TypeId, TypeId>,
}

impl GenericMap {
    /// Create a generic map from two sets of [`TypeId`]s: The set of generics to resolve,
    /// and the set of resolved generics.
    pub fn create(
        generics: &[TypeId],
        resolved: &[TypeId],
        ctx: &mut TypeCtx,
    ) -> Result<GenericMap, Error> {
        if generics.len() != resolved.len() {
            // FIXME: Add better error message here printing both sets of generics
            let err_msg = String::from("missing types in generic expansion");
            return Err(Error::new(ErrKind::Generics).with_msg(err_msg));
        }

        let mut map = GenericMap::default();

        let mut is_err = false;

        generics.iter().zip(resolved).for_each(|(l_ty, r_ty)| {
            log!("mapping generic: {} <- {}", l_ty, r_ty);
            if let Err(e) = map.declare(l_ty.clone(), r_ty.clone()) {
                ctx.error(e);
                is_err = true;
            }
        });

        match is_err {
            true => Err(Error::new(ErrKind::Generics)),
            false => Ok(map),
        }
    }

    /// Declare a new type "match" in the map. This function associates a generic type
    /// with its resolved counterpart, and errors out otherwise
    pub fn declare(&mut self, lty: TypeId, rty: TypeId) -> Result<(), Error> {
        match self.map.insert(lty.clone(), rty.clone()) {
            None => Ok(()),
            Some(existing_ty) => Err(Error::new(ErrKind::Generics).with_msg(format!(
                "mapping type to already mapped generic type: {} <- {} with {}",
                lty, existing_ty, rty
            ))),
        }
    }

    /// Get the type associated with a generic type in a previous call to `declare()`
    pub fn get_match(&self, to_resolve: &TypeId) -> Result<TypeId, Error> {
        self.map.get(to_resolve).cloned().ok_or_else(|| {
            Error::new(ErrKind::Generics)
                .with_msg(format!("undeclared generic type: {}", to_resolve))
        })
    }
}

/// Mangle a name to resolve it to its proper expanded name.
/// The format used is the following:
/// <name> '+' <T0> '+' <T1>...
pub fn mangle(name: &str, types: &[TypeId]) -> String {
    let mut mangled = String::from(name);
    for type_id in types.iter() {
        mangled.push('+');
        mangled.push_str(type_id.id());
    }

    log!("mangled: {}", &mangled);

    mangled
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
    /// the function `generics::mangle("f", GenericMap { TypeId "T" })` // FIXME: Fix doc
    fn resolve_self(&mut self, _ctx: &mut TypeCtx) {}
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
        assert_eq!(mangle("mangled", &[]), "mangled");
    }

    #[test]
    fn mangle_one_generic() {
        assert_eq!(mangle("mangled", &[ty!("bool")]), "mangled+bool");
    }

    #[test]
    fn mangle_multi_generic() {
        assert_eq!(
            mangle("mangled", &[ty!("float"), ty!("ComplexType")]),
            "mangled+float+ComplexType"
        );
    }

    #[test]
    fn create_map_different_size() {
        let mut ctx = TypeCtx::new();

        assert!(GenericMap::create(&[ty!("int"), ty!("float")], &[ty!("T")], &mut ctx).is_err());
    }
}
