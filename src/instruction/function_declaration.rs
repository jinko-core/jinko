//! Function Declarations are used when adding a new function to the source. They contain
//! a name, a list of required arguments as well as an associated code block

use crate::block::Block;
use super::{Instruction, InstrKind};

pub struct FunctionDecArg {
    name: String,
    // FIXME: Shouldn't be a string
    ty: String,
}

pub struct FunctionDec {
    name: String,
    args: Vec<FunctionDecArg>,
    block: Option<Block>,
}

impl FunctionDecArg {
    /// Create a new function declaration argument with a name and a type
    pub fn new(name: String, ty: String) -> FunctionDecArg {
        FunctionDecArg {
            name,
            ty,
        }
    }
}

impl FunctionDec {
    /// Create a new function declaration with a given name, no args and no code block
    pub fn new(name: String) -> FunctionDec {
        FunctionDec {
            name,
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

    /// Add an argument to the function's signature
    pub fn add_arg(&mut self, arg: FunctionDecArg) {
        self.args.push(arg)
    }

    /// Return a reference to the function's block
    pub fn block(&self) -> Option<&Block> {
        match self.block {
            Some(b) => Some(&b),
            None => None
        }
    }
}

impl Instruction for FunctionDec {
    fn kind(&self) -> InstrKind {
        InstrKind::Statement
    }
}
