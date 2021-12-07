use super::{DecArg, InstrKind, Instruction, TypeId};

use crate::{
    typechecker::{CheckedType, TypeCtx},
    Context, ObjectInstance, TypeCheck,
};

#[derive(Clone, Debug, PartialEq)]
pub struct TypeDec {
    name: String,
    generics: Vec<TypeId>,
    fields: Vec<DecArg>,
}

impl TypeDec {
    /// Create a new type
    pub fn new(name: String, generics: Vec<TypeId>, fields: Vec<DecArg>) -> TypeDec {
        TypeDec {
            name,
            generics,
            fields,
        }
    }

    /// Get a reference to the name of the type
    pub fn name(&self) -> &str {
        &self.name
    }

    /// Get a reference to the type's fields
    pub fn fields(&self) -> &Vec<DecArg> {
        &self.fields
    }
}

impl Instruction for TypeDec {
    fn kind(&self) -> InstrKind {
        InstrKind::Statement
    }

    fn execute(&self, ctx: &mut Context) -> Option<ObjectInstance> {
        ctx.debug_step(&format!("CUSTOM TYPE {} ENTER", self.name));

        if let Err(e) = ctx.add_type(self.clone()) {
            ctx.error(e);
            return None;
        }

        ctx.debug_step(&format!("CUSTOM TYPE {} EXIT", self.name));

        // Declaring a type is always a statement (for now)
        None
    }

    // FIXME: Really unefficient
    fn print(&self) -> String {
        let mut base = format!("type {}", self.name);

        if !self.generics.is_empty() {
            base.push('[');
            base.push_str(self.generics.first().unwrap().id());
            let generic_str = self
                .generics
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
}

impl TypeCheck for TypeDec {
    fn resolve_type(&self, ctx: &mut TypeCtx) -> CheckedType {
        // TODO: FunctionDecs and TypeDec are very similar. Should we factor them together?
        let fields_ty = self
            .fields
            .iter()
            .map(|dec_arg| {
                (
                    dec_arg.name().to_string(),
                    CheckedType::Resolved(dec_arg.get_type().clone()),
                )
            })
            .collect();
        if let Err(e) = ctx.declare_custom_type(
            self.name.clone(),
            CheckedType::Resolved(TypeId::from(self.name())),
            fields_ty,
        ) {
            ctx.error(e);
        }

        CheckedType::Void
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
            name: type_name,
            generics: vec![],
            fields: vec![],
        }
    }
}

impl std::fmt::Display for TypeDec {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
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
