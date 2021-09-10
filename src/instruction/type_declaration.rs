use super::{DecArg, InstrKind, Instruction};

use crate::{typechecker::CheckedType, Context, ObjectInstance, TypeCheck};

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
    fn resolve_type(&self, _ctx: &mut Context) -> CheckedType {
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
