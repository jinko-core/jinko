//! Function Declarations are used when adding a new function to the source. They contain
//! a name, a list of required arguments as well as an associated code block

use super::{InstrKind, Instruction};
use crate::block::Block;

// FIXME: Shouldn't be a String
type Ty = String;

/// What "kind" of function is defined. There are four types of functions in broccoli,
/// the normal ones, the external ones, the unit tests and the mocks
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum FunctionKind {
    Unknown,
    Func,
    Ext,
    Test,
    Mock,
}

pub struct FunctionDecArg {
    name: String,
    // FIXME: Shouldn't be a string
    ty: Ty,
}

pub struct FunctionDec {
    name: String,
    ty: Option<Ty>,
    kind: FunctionKind,
    args: Vec<FunctionDecArg>,
    block: Option<Block>,
}

impl FunctionDecArg {
    /// Create a new function declaration argument with a name and a type
    pub fn new(name: String, ty: String) -> FunctionDecArg {
        FunctionDecArg { name, ty }
    }

    /// Return a reference to the argument's name
    pub fn name(&self) -> &String {
        &self.name
    }

    /// Return a reference to the argument's type
    pub fn ty(&self) -> &String {
        &self.ty
    }
}

impl FunctionDec {
    /// Create a new function declaration with a given name, no args and no code block
    pub fn new(name: String, ty: Option<String>) -> FunctionDec {
        FunctionDec {
            name,
            ty,
            kind: FunctionKind::Unknown,
            args: Vec::new(),
            block: None,
        }
    }

    /// Set the block of a given function declaration. This is useful since parsing a
    /// function's block comes after the function signature.
    pub fn set_block(&mut self, block: Block) {
        self.block = Some(block)
    }

    /// Return a reference to the function's name
    pub fn name(&self) -> &str {
        &self.name
    }

    /// Return a reference to the function's return type
    pub fn ty(&self) -> Option<&str> {
        match &self.ty {
            Some(ty) => Some(&ty),
            None => None,
        }
    }

    /// Return the kind of a function
    pub fn kind(&self) -> FunctionKind {
        self.kind
    }

    /// Set the kind of a function. This cannot be done at initialization due to the
    /// parser's nature
    pub fn set_kind(&mut self, kind: FunctionKind) {
        self.kind = kind
    }

    /// Return a reference to the function's arguments
    pub fn args(&self) -> &Vec<FunctionDecArg> {
        &self.args
    }

    /// Set the vector of arguments that the function should handle
    pub fn set_args(&mut self, args: Vec<FunctionDecArg>) {
        self.args = args
    }

    /// Return a reference to the function's block
    pub fn block(&self) -> Option<&Block> {
        match &self.block {
            Some(b) => Some(&b),
            None => None,
        }
    }
}

impl Instruction for FunctionDec {
    fn kind(&self) -> InstrKind {
        InstrKind::Statement
    }

    fn print(&self) -> String {
        let mut base = String::from(match self.kind {
            FunctionKind::Func => "func",
            FunctionKind::Ext => "ext func",
            FunctionKind::Test => "test",
            FunctionKind::Mock => "mock",
            FunctionKind::Unknown => "UNKNOWN",
        });

        base = format!("{} {}(", base, self.name);

        let mut first_arg = true;
        for arg in self.args {
            base.push_str(&format!("{}: {}", arg.name(), arg.ty()));

            if !first_arg {
                base.push_str(", ");
            }

            first_arg = false;
        }

        base = match self.ty {
            Some(ty) => format!("{}) -> {}", base, ty),
            None => format!("{}) ", base),
        };

        match self.block {
            Some(block) => format!("{} {}", base, block.print()),
            None => format!("{} {{}}", base),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn simple_no_arg() {
        let function = FunctionDec::new("fn".to_owned(), None);

        assert_eq!(function.print(), "func fn() {}");
    }

    #[test]
    fn simple_args() {
        let function = FunctionDec::new("fn".to_owned(), Some("int".to_owned()));
        let args = vec![
            FunctionDecArg::new("arg0".to_owned(), "int".to_owned()),
            FunctionDecArg::new("arg1".to_owned(), "int".to_owned()),
        ];

        function.set_args(args);

        assert_eq!(function.print(), "func fn(arg0: int, arg1: int) -> int {}");
    }
}
