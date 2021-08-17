//! `JkInst`s are special directives given to the interpreter. There is only a limited
//! amount of them, and they are mostly useful for debugging or testing. They aren't
//! really an `Instruction`, and therefore their implementation lives in the parser
//! module. They are executed at "compile" time, when running through the code first.

use crate::instruction::{FunctionCall, InstrKind, Instruction};
use crate::{ErrKind, Error, Interpreter, Rename};

/// The potential interpreter instructions
#[derive(Clone, Debug, PartialEq)]
pub enum JkInstKind {
    Dump,
    Quit,
    Ir,
}

#[derive(Clone)]
pub struct JkInst {
    kind: JkInstKind,
    args: Vec<Box<dyn Instruction>>,
}

impl JkInst {
    /// Construct a `JkInst` from a `FunctionCall`
    pub fn from_function_call(fc: FunctionCall) -> Result<Self, Error> {
        let func_name = fc.name();

        let kind = match func_name {
            "dump" => JkInstKind::Dump,
            "quit" => JkInstKind::Quit,
            "ir" => JkInstKind::Ir,
            // FIXME: Fix location
            _ => {
                return Err(Error::new(
                    ErrKind::Parsing).with_msg(
                    format!("unknown interpreter directive @{}", func_name),
                ))
            }
        };

        Ok(Self {
            kind,
            args: fc.args().clone(),
        })
    }

    #[cfg(test)]
    pub fn jk_inst_kind(&self) -> &JkInstKind {
        &self.kind
    }
}

impl Instruction for JkInst {
    fn kind(&self) -> InstrKind {
        InstrKind::Statement
    }

    fn print(&self) -> String {
        match self.kind {
            JkInstKind::Dump => "@dump",
            JkInstKind::Quit => "@quit",
            JkInstKind::Ir => "@ir",
        }
        .to_string()
    }

    fn execute(&self, interpreter: &mut Interpreter) -> Result<InstrKind, Error> {
        interpreter.debug("JINKO_INST", &self.print());

        match self.kind {
            JkInstKind::Dump => println!("{}", interpreter.print()),
            JkInstKind::Quit => std::process::exit(0),
            JkInstKind::Ir => eprintln!("usage: {:?} <statement|expr>", JkInstKind::Ir),
        };

        // JinkInsts cannot return anything. They simply act directly from the interpreter,
        // on the interpreter.
        Ok(InstrKind::Statement)
    }
}

impl Rename for JkInst {
    fn prefix(&mut self, _: &str) {}
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::Construct;

    #[test]
    fn t_invalid_jkinst() {
        let (_, fc) = Construct::function_call("tamer()").unwrap();
        let inst = JkInst::from_function_call(fc);

        assert!(inst.is_err(), "tamer is not a valid interpreter directive")
    }

    #[test]
    fn t_valid_inst_no_args() {
        let (_, fc) = Construct::function_call("dump()").unwrap();
        let inst = JkInst::from_function_call(fc);

        assert!(inst.is_ok(), "dump is a valid interpreter directive")
    }

    #[test]
    fn t_valid_inst_with_args() {
        let (_, fc) = Construct::function_call("ir(fn)").unwrap();
        let inst = JkInst::from_function_call(fc);

        assert!(
            inst.is_ok(),
            "ir(func) is a valid use of the ir interpreter directive"
        )
    }
}
