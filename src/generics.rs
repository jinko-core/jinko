//! The [`Generic`] trait is required to enable the generic type expansion of
//! variables, type and function declarations, type instantiations and function
//! calls. Its basic goal is to replace all instances of `T` with an actual [`TypeId`].

use std::collections::HashMap;

use crate::instruction::TypeId;
use crate::typechecker::TypeCtx;
use crate::log;

pub type GenericMap = HashMap<String, TypeId>;

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
    fn expand(&self, _type_ctx: &mut TypeCtx) {}

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
    fn generate_new(&self, _type_map: GenericMap) -> Option<Box<Self>> {
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::instruction::TypeId;

    macro_rules! s {
        ($str:literal) => ( String::from($str) )
    }

    #[test]
    fn mangle_no_generics() {
        assert_eq!(mangle("mangled", &[]), "mangled");
    }

    #[test]
    fn mangle_one_generic() {
        assert_eq!(mangle("mangled", &[TypeId::new(s!("bool"))]), "mangled+bool");
    }

    #[test]
    fn mangle_multi_generic() {
        assert_eq!(mangle("mangled", &[TypeId::new(s!("float")), TypeId::new(s!("ComplexType"))]), "mangled+float+ComplexType");
    }
}
