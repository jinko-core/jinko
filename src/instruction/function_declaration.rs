//! Function Declarations are used when adding a new function to the source. They contain
//! a name, a list of required arguments as well as an associated code block

use crate::instruction::{Block, DecArg, InstrKind, Instruction, TypeId};
use crate::{Interpreter, JkErrKind, JkError, Rename};

/// What "kind" of function is defined. There are four types of functions in jinko,
/// the normal ones, the external ones, the unit tests and the mocks
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum FunctionKind {
    Unknown,
    Func,
    Ext,
    Test,
    Mock,
}

#[derive(Clone)]
pub struct FunctionDec {
    name: String,
    ty: Option<TypeId>,
    kind: FunctionKind,
    args: Vec<DecArg>,
    block: Option<Block>,
}

impl FunctionDec {
    /// Create a new function declaration with a given name, no args and no code block
    pub fn new(name: String, ty: Option<TypeId>) -> FunctionDec {
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

    /// Add an instruction to the function declaration, in order. This is mostly useful
    /// when adding instructions to the entry point of the interpreter, since parsing
    /// directly gives a block to the function
    pub fn add_instruction(&mut self, instruction: Box<dyn Instruction>) -> Result<(), JkError> {
        match &mut self.block {
            Some(b) => Ok(b.add_instruction(instruction)),
            None => Err(JkError::new(
                JkErrKind::Interpreter,
                format!(
                    "function {} has no instruction block. It might be an extern function or an error",
                    self.name
                ),
                None,
                self.name.clone(),
            )),
        }
    }

    /// Return a reference to the function's name
    pub fn name(&self) -> &str {
        &self.name
    }

    /// Return a reference to the function's return type
    pub fn ty(&self) -> Option<&TypeId> {
        self.ty.as_ref()
    }

    /// Set the type of the function
    pub fn set_ty(&mut self, ty: Option<TypeId>) {
        self.ty = ty
    }

    /// Return the kind of a function
    pub fn fn_kind(&self) -> FunctionKind {
        self.kind
    }

    /// Set the kind of a function. This cannot be done at initialization due to the
    /// parser's nature
    pub fn set_kind(&mut self, kind: FunctionKind) {
        self.kind = kind
    }

    /// Return a reference to the function's arguments
    pub fn args(&self) -> &Vec<DecArg> {
        &self.args
    }

    /// Set the vector of arguments that the function should handle
    pub fn set_args(&mut self, args: Vec<DecArg>) {
        self.args = args
    }

    /// Return a reference to the function's block
    pub fn block(&self) -> Option<&Block> {
        self.block.as_ref()
    }

    /// Return a mutable reference to the function's block
    pub fn block_mut(&mut self) -> Option<&mut Block> {
        self.block.as_mut()
    }

    /// Run through the function as if it was called. This is useful for setting
    /// an entry point into the interpreter and executing it
    pub fn run(&self, interpreter: &mut Interpreter) -> Result<InstrKind, JkError> {
        let block = match self.block() {
            Some(b) => b,
            // FIXME: Fix Location and input
            None => {
                return Err(JkError::new(
                    JkErrKind::Interpreter,
                    format!(
                        "cannot execute function {} as it is marked `ext`",
                        self.name()
                    ),
                    None,
                    self.name().to_owned(),
                ))
            }
        };

        block.execute(interpreter)
    }
}

impl Instruction for FunctionDec {
    fn kind(&self) -> InstrKind {
        InstrKind::Statement
    }

    fn execute(&self, interpreter: &mut Interpreter) -> Result<InstrKind, JkError> {
        interpreter.debug_step("FUNCDEC ENTER");

        match self.fn_kind() {
            FunctionKind::Func | FunctionKind::Ext => interpreter.add_function(self.clone())?,
            FunctionKind::Test => interpreter.add_test(self.clone())?,
            FunctionKind::Mock | FunctionKind::Unknown => {
                return Err(JkError::new(
                    JkErrKind::Interpreter,
                    format!("unknown type for function {}", self.name()),
                    None,
                    self.name().to_owned(),
                ))
            }
        }

        interpreter.debug_step("FUNCDEC EXIT");

        Ok(InstrKind::Statement)
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
        for arg in &self.args {
            if !first_arg {
                base.push_str(", ");
            }

            base.push_str(&format!("{}: {}", arg.name(), arg.get_type().id()));

            first_arg = false;
        }

        base = match &self.ty {
            Some(ty) => format!("{}) -> {}", base, ty.id()),
            None => format!("{})", base),
        };

        match &self.block {
            Some(block) => format!("{} {}", base, block.print()),
            None => format!("{} {{}}", base),
        }
    }
}

impl Rename for FunctionDec {
    fn prefix(&mut self, prefix: &str) {
        self.name = format!("{}{}", prefix, self.name);
        self.set_ty(self.ty().map_or(None, |ty| {
            Some(TypeId::from(format!("{}{}", prefix, ty.id()).as_str()))
        }));

        // FIXME: No need to prefix the scope inside a function, right?
        // match &mut self.block {
        //     Some(b) => b.prefix(prefix),
        //     None => {}
        // };

        self.args.iter_mut().for_each(|arg| arg.prefix(prefix));
    }
}

impl Default for FunctionDec {
    fn default() -> Self {
        FunctionDec::new(String::new(), None)
    }
}

impl std::fmt::Debug for FunctionDec {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.print())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::instruction::TypeId;

    #[test]
    fn simple_no_arg() {
        let mut function = FunctionDec::new("fn".to_owned(), None);
        function.set_kind(FunctionKind::Func);

        assert_eq!(function.print(), "func fn() {}");
    }

    #[test]
    fn simple_args() {
        let mut function = FunctionDec::new("fn".to_owned(), Some(TypeId::from("int")));
        function.set_kind(FunctionKind::Func);
        let args = vec![
            DecArg::new("arg0".to_owned(), TypeId::from("int")),
            DecArg::new("arg1".to_owned(), TypeId::from("int")),
        ];

        function.set_args(args);

        assert_eq!(function.print(), "func fn(arg0: int, arg1: int) -> int {}");
    }
}
