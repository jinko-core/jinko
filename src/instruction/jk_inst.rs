//! `JkInst`s are special directives given to the interpreter. There is only a limited
//! amount of them, and they are mostly useful for debugging or testing. They aren't
//! really an `Instruction`, and therefore their implementation lives in the parser
//! module. They are executed at "compile" time, when running through the code first.

use crate::instruction::{InstrKind, Instruction, FunctionCall};
use crate::{Interpreter, JkErrKind, JkError};

/// The potential interpreter instructions
#[derive(Clone, Debug, PartialEq)]
pub enum JkInst {
    Dump,
    Quit,
    Ir,
}

impl JkInst {
    /// Construct a `JkInst` from a `FunctionCall`
    pub fn from_function_call(fc: FunctionCall) -> Result<Self, JkError> {
        let func_name = fc.name();

        match func_name {
            "dump" => Ok(JkInst::Dump),
            "quit" => Ok(JkInst::Quit),
            "ir" => Ok(JkInst::Ir),
            // FIXME: Fix location
            _ => Err(JkError::new(
                JkErrKind::Parsing,
                format!("unknown interpreter directive @{}", func_name),
                None,
                func_name.to_owned(),
            )),
        }
    }
}

impl Instruction for JkInst {
    fn kind(&self) -> InstrKind {
        InstrKind::Statement
    }

    fn print(&self) -> String {
        match self {
            JkInst::Dump => "@dump",
            JkInst::Quit => "@quit",
            JkInst::Ir => "@ir",
        }
        .to_string()
    }

    fn execute(&self, interpreter: &mut Interpreter) -> Result<InstrKind, JkError> {
        interpreter.debug("JINKO_INST", &self.print());

        match self {
            JkInst::Dump => println!("{}", interpreter.print()),
            JkInst::Quit => std::process::exit(0),
            JkInst::Ir => eprintln!("usage: {:?} <statement|expr>", JkInst::Ir),
        };

        // JinkInsts cannot return anything. They simply act directly from the interpreter,
        // on the interpreter.
        Ok(InstrKind::Statement)
    }
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
        let (_, fc) = Construct::function_call("ir(func)").unwrap();
        let inst = JkInst::from_function_call(fc);

        assert!(inst.is_ok(), "ir(func) is a valid use of the ir interpreter directive")
    }


    #[test]
    fn t_invalid_inst_with_args() {
        let (_, fc) = Construct::function_call("dump(arg)").unwrap();
        let inst = JkInst::from_function_call(fc);

        assert!(inst.is_err(), "dump does not take any parameters")
    }
}
