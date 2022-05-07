//! The [`Generic`] trait is required to enable the generic type expansion of
//! variables, type and function declarations, type instantiations and function
//! calls. Its basic goal is to replace all instances of `T` with an actual [`TypeId`].

use std::collections::HashMap;
use std::vec::IntoIter;

use crate::error::{ErrKind, Error};
use crate::typechecker::{TypeCtx, TypeId};

#[derive(Default, Debug)]
pub struct GenericMap {
    map: HashMap<TypeId, TypeId>,
}

impl GenericMap {
    /// Create a generic map from two sets of [`TypeId`]s: The set of generics to resolve,
    /// and the set of resolved generics.
    pub fn create(
        generics: &GenericList,
        resolved: &GenericList,
        ctx: &mut TypeCtx,
    ) -> Result<GenericMap, Error> {
        let generics = generics.data();
        let resolved = resolved.data();

        if generics.len() != resolved.len() {
            // FIXME: Add better error message here printing both sets of generics
            let err_msg = String::from("missing types in generic expansion");
            let mut err_msg = format!("{}\ngeneric types: ", err_msg);
            for generic in generics {
                err_msg.push_str(&format!("{}", generic));
            }

            let mut err_msg = format!("{}\nresolved types: ", err_msg);
            for resolved in resolved {
                err_msg.push_str(&format!("{}", resolved));
            }

            return Err(Error::new(ErrKind::Generics).with_msg(err_msg));
        }

        let mut map = GenericMap::default();

        let mut is_err = false;

        generics.iter().zip(resolved).for_each(|(l_ty, r_ty)| {
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
    pub fn declare(&mut self, generic: TypeId, specialized: TypeId) -> Result<(), Error> {
        match self.map.insert(generic.clone(), specialized.clone()) {
            None => Ok(()),
            Some(existing_ty) => Err(Error::new(ErrKind::Generics).with_msg(format!(
                "mapping type to already mapped generic type: {} <- {} with {}",
                generic, existing_ty, specialized
            ))),
        }
    }

    /// Get the type associated with a generic type in a previous call to `declare()`
    pub fn get_specialized(&self, generic: &TypeId) -> Result<TypeId, Error> {
        self.map.get(generic).cloned().ok_or_else(|| {
            Error::new(ErrKind::Generics).with_msg(format!("undeclared generic type: {}", generic))
        })
    }

    /// Get the types associated with a list of generic types previously declared
    pub fn specialized_types(&self, generics: &GenericList) -> Result<GenericList, Error> {
        generics
            .data()
            .iter()
            .map(|g| self.get_specialized(g))
            .collect()
    }
}

/// A list of generic types. This adds a lot of methods over a regular `Vec<TypeId>` which are
/// useful for multiple instructions and types in the interpreter.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct GenericList(Vec<TypeId>);

// FIXME: Implement `Display` for `GenericList`

impl GenericList {
    /// Create an empty list of generic types
    pub fn empty() -> GenericList {
        GenericList(vec![])
    }

    /// Is a [`GenericList`] empty
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    /// Get the types contained in the [`GenericList`]
    pub fn data(&self) -> &Vec<TypeId> {
        self.0.as_ref()
    }

    /// Get a mutable reference on the types contained in the [`GenericList`]
    pub fn data_mut(&mut self) -> &mut Vec<TypeId> {
        self.0.as_mut()
    }

    /// Consume the vector to append a new type to it
    pub fn with(mut self, ty: TypeId) -> GenericList {
        self.0.push(ty);

        GenericList(self.0)
    }
}

impl FromIterator<TypeId> for GenericList {
    fn from_iter<T: IntoIterator<Item = TypeId>>(iter: T) -> Self {
        iter.into_iter()
            .fold(GenericList::empty(), |list, ty| list.with(ty))
    }
}

impl IntoIterator for GenericList {
    type Item = TypeId;

    type IntoIter = IntoIter<TypeId>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

/// Mangle a name to resolve it to its proper expanded name.
/// The format used is the following:
/// <name> '+' <T0> '+' <T1>...
pub fn mangle(name: &str, types: &GenericList) -> String {
    let mut mangled = String::from(name);
    for type_id in types.data().iter() {
        // FIXME: Add const for delimiter
        mangled.push('+');
        mangled.push_str(type_id.id());
    }

    mangled
}

/// Performs the opposite conversion, turning a mangled name into a valid
/// jinko function name with generics.
pub fn demangle(mangled_name: &str) -> &str {
    if let Some(idx) = mangled_name.find('+') {
        &mangled_name[..idx]
    } else {
        mangled_name
    }
}

/// Fetch the original name contained in a mangled name. This is useful for
/// generic builtins, which only have one implementation despite being able
/// to handle multiple types.
///
/// ```rust
/// use jinko::generics::original_name;
///
/// let mangled = "type_of+int";
/// let original_builtin_name = original_name(mangled);
///
/// assert_eq!(original_builtin_name, "type_of");
/// ```
pub fn original_name(mangled_name: &str) -> &str {
    match mangled_name.find('+') {
        None => mangled_name,
        Some(first_separator) => &mangled_name[..first_separator],
    }
}

/// Since most of the instructions cannot do generic expansion, we can implement
/// default methods which do nothing. This avoid more boilerplate code for instructions
/// such as constants or variables which cannot be generic.
pub trait GenericUser {
    /// Mutate an instruction in order to resolve to the proper, expanded generic instruction.
    /// This function is also responsible for calling `resolve_usages` on all its sub-items:
    /// A block is responsible for resolving each of its statements, as well as its final
    /// expression if there is one.
    ///
    /// With a type map like the following: `{ T: int, U: string }`, a type call like
    /// `f[T, U]()` should be resolved to `f+int+string()`
    fn resolve_usages(&mut self, _type_map: &GenericMap, _ctx: &mut TypeCtx) {}
}

/// Only a certain set of instructions are generic expansion sites - They can generate a "new
/// version of themselves" with a given typemap. Once a new version is generated, they must
/// be typechecked by the generator.
pub trait GenericExpander: Sized {
    /// Generate a new version of the instruction with the given typemap. The new version should
    /// not contain any generics.
    // FIXME: We need to remove the `new_name` parameter for types?
    fn generate(&self, new_name: String, type_map: &GenericMap, ctx: &mut TypeCtx) -> Self;
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::symbol::Symbol;
    use crate::typechecker::TypeId;

    macro_rules! ty {
        ($str:literal) => {
            TypeId::new(Symbol::from(String::from($str)))
        };
    }

    #[test]
    fn mangle_no_generics() {
        let generics = GenericList::empty();
        assert_eq!(mangle("mangled", &generics), "mangled");
    }

    #[test]
    fn mangle_one_generic() {
        let generics = GenericList::empty().with(ty!("bool"));
        assert_eq!(mangle("mangled", &generics), "mangled+bool");
    }

    #[test]
    fn mangle_multi_generic() {
        let generics = GenericList::empty()
            .with(ty!("float"))
            .with(ty!("ComplexType"));
        assert_eq!(mangle("mangled", &generics), "mangled+float+ComplexType");
    }

    #[test]
    fn get_original_name_back() {
        let generics = GenericList::empty()
            .with(ty!("float"))
            .with(ty!("ComplexType"));
        assert_eq!(original_name(&mangle("og_fn", &generics)), "og_fn");
    }

    #[test]
    fn create_map_different_size() {
        let mut ctx = TypeCtx::new();
        let resolved = GenericList::empty().with(ty!("int")).with(ty!("float"));
        let generics = GenericList::empty().with(ty!("T"));

        assert!(GenericMap::create(&resolved, &generics, &mut ctx).is_err());
    }
}
