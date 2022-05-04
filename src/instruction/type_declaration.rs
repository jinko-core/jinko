use super::{DecArg, InstrKind, Instruction};

use crate::context::Context;
use crate::generics::{GenericExpander, GenericMap, GenericUser};
use crate::instance::ObjectInstance;
use crate::location::SpanTuple;
use crate::log;
use crate::symbol::Symbol;
use crate::typechecker::{CheckedType, TypeCheck, TypeCtx, TypeId};

#[derive(Clone, Debug, PartialEq)]
pub struct TypeDec {
    id: TypeId,
    fields: Vec<DecArg>,
    typechecked: bool,
    location: Option<SpanTuple>,
}

impl TypeDec {
    /// Create a new type
    pub fn new(id: TypeId, fields: Vec<DecArg>) -> TypeDec {
        TypeDec {
            id,
            fields,
            typechecked: false,
            location: None,
        }
    }

    /// Get a reference to the name of the type
    pub fn id(&self) -> &TypeId {
        &self.id
    }

    /// Get a reference to the type's fields
    pub fn fields(&self) -> &Vec<DecArg> {
        &self.fields
    }

    /// Get a reference to the type's generics
    pub fn generics(&self) -> &Vec<TypeId> {
        &self.id.generics()
    }

    pub fn set_location(&mut self, location: SpanTuple) {
        self.location = Some(location)
    }
}

impl Instruction for TypeDec {
    fn kind(&self) -> InstrKind {
        InstrKind::Statement
    }

    fn execute(&self, ctx: &mut Context) -> Option<ObjectInstance> {
        log!("custom type enter: {}", self.id);

        if let Err(e) = ctx.add_type(self.clone()) {
            ctx.error(e);
            return None;
        }

        log!("custom type exit: {}", self.id);

        // Declaring a type is always a statement (for now)
        None
    }

    // FIXME: Really unefficient
    fn print(&self) -> String {
        let mut base = format!("type {}", self.id);

        if !self.generics().is_empty() {
            base.push('[');
            base.push_str(self.generics().first().unwrap().id());
            let generic_str = self
                .generics()
                .iter()
                .skip(1)
                .fold(String::new(), |acc, ty_id| {
                    format!("{}, {}", acc, ty_id.id())
                });
            base.push_str(&generic_str);
            base.push(']');
        }

        if !self.fields.is_empty() {
            base.push('(');
            base.push_str(&format!("{}", self.fields.first().unwrap()));
            let arg_str = self
                .fields
                .iter()
                .skip(1)
                .fold(String::new(), |acc, field| format!("{}, {}", acc, field));
            base.push_str(&arg_str);
            base.push(')');
        }

        base
    }

    fn location(&self) -> Option<&SpanTuple> {
        self.location.as_ref()
    }
}

impl TypeCheck for TypeDec {
    fn resolve_type(&mut self, ctx: &mut TypeCtx) -> CheckedType {
        if let Err(e) = ctx.declare_custom_type(self.id.clone(), self.clone()) {
            ctx.error(e);
        }

        CheckedType::Void
    }

    fn set_cached_type(&mut self, _ty: CheckedType) {
        self.typechecked = true
    }

    fn cached_type(&self) -> Option<&CheckedType> {
        match self.typechecked {
            true => Some(&CheckedType::Void),
            false => None,
        }
    }
}

impl GenericUser for TypeDec {
    fn resolve_usages(&mut self, _type_map: &GenericMap, ctx: &mut TypeCtx) {
        // FIXME: Can we do without that?
        if let Err(e) = ctx.declare_custom_type(self.id().clone(), self.clone()) {
            ctx.error(e);
        };
    }
}

impl GenericExpander for TypeDec {
    fn generate(&self, mangled_name: String, type_map: &GenericMap, ctx: &mut TypeCtx) -> TypeDec {
        let mut new_type = self.clone();
        // FIXME: Can we remove this?
        new_type.id = TypeId::new(Symbol::from(mangled_name));
        new_type.typechecked = false;

        new_type
            .fields
            .iter_mut()
            .for_each(|arg| arg.resolve_usages(type_map, ctx));

        new_type
    }
}

impl From<&str> for TypeDec {
    fn from(type_name: &str) -> TypeDec {
        TypeDec::from(type_name.to_string())
    }
}

impl From<&String> for TypeDec {
    fn from(type_name: &String) -> TypeDec {
        TypeDec::from(type_name.clone())
    }
}

impl From<String> for TypeDec {
    fn from(type_name: String) -> TypeDec {
        TypeDec {
            id: TypeId::new(Symbol::from(type_name)),
            fields: vec![],
            typechecked: false,
            location: None,
        }
    }
}

// FIXME: Can we improve that, and is it really used?
impl std::fmt::Display for TypeDec {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.id)
    }
}

#[cfg(test)]
mod tests {
    use crate::{jinko, jinko_fail};

    #[test]
    fn tc_valid_easy() {
        jinko! {
            type Complex(real: int, imaginary: int);
            c = Complex(real: 15, imaginary: 14);
        };
    }

    #[test]
    fn tc_valid_hard() {
        jinko! {
            type Point(x: int, y: int);
            type Vector2(v0: Point, v1: Point);

            func zero() -> Point {
                Point(x: 0, y: 0)
            }

            v = Vector2(v0: zero(), v1: zero());
        };
    }

    #[test]
    fn tc_invalid_hard() {
        jinko_fail! {
            type Point(x: int, y: int);
            type NotPoint(x: int, y: int);
            type Vector2(v0: Point, v1: Point);

            func zero() -> NotPoint {
                NotPoint(x: 0, y: 0)
            }

            v = Vector2(v0: 15, v1: zero());
        };
    }
}
