use super::{DecArg, InstrKind, Instruction, TypeId};

use crate::{
    typechecker::{CheckedType, TypeCtx},
    Context, ObjectInstance, TypeCheck,
};

#[derive(Clone, Debug, PartialEq)]
pub struct TypeDec {
    name: String,
    fields: Vec<DecArg>,
}

impl TypeDec {
    /// Create a new type
    pub fn new(name: String, fields: Vec<DecArg>) -> TypeDec {
        TypeDec { name, fields }
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
        let mut base = format!("type {} (", self.name);

        base.push_str(
            self.fields
                .get(0)
                .map_or(String::new(), |f| format!("{}", f))
                .as_str(),
        );

        self.fields()
            .iter()
            .skip(1)
            .for_each(|field| base.push_str(format!(", {}", field).as_str()));
        format!("{});", base)
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
            c = Complex { real = 15, imaginary = 14 };
        };
    }

    #[test]
    fn tc_valid_hard() {
        jinko! {
            type Point(x: int, y: int);
            type Vector2(v0: Point, v1: Point);

            func zero() -> Point {
                Point { x = 0, y = 0 }
            }

            v = Vector2 { v0 = zero(), v1 = zero() };
        };
    }

    #[test]
    fn tc_invalid_hard() {
        jinko_fail! {
            type Point(x: int, y: int);
            type NotPoint(x: int, y: int);
            type Vector2(v0: Point, v1: Point);

            func zero() -> NotPoint {
                NotPoint { x = 0, y = 0 }
            }

            v = Vector2 { v0 = 15, v1 = zero() };
        };
    }
}
