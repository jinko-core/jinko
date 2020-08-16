//! Function Declarations are used when adding a new function to the source. They contain
//! a name, a list of required arguments as well as an associated code block

use super::{InstrKind, Instruction};
use crate::block::Block;

pub struct FunctionDecArg {
    name: String,
    // FIXME: Shouldn't be a string
    ty: String,
}

pub struct FunctionDec {
    name: String,
    ty: Option<String>,
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
    pub fn ty(&self) -> &Option<String> {
        &self.ty
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
}
