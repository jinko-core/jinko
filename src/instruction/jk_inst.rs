//! `JkInst`s are special directives given to the context. There is only a limited
//! amount of them, and they are mostly useful for debugging or testing. They aren't
//! really an `Instruction`, and therefore their implementation lives in the parser
//! module. They are executed at "compile" time, when running through the code first.

use crate::instruction::{FunctionCall, InstrKind, Instruction};
use crate::{Context, ErrKind, Error, ObjectInstance};

/// The potential ctx instructions
#[derive(Clone, Debug, PartialEq)]
pub enum JkInstKind {
    Dump,
    Quit,
    Ir,
}

#[derive(Clone)]
pub struct JkInst {
    kind: JkInstKind,
    _args: Vec<Box<dyn Instruction>>,
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
                return Err(Error::new(ErrKind::Parsing)
                    .with_msg(format!("unknown ctx directive @{}", func_name)))
            }
        };

        Ok(Self {
            kind,
            _args: fc.args().clone(),
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

    fn execute(&self, ctx: &mut Context) -> Option<ObjectInstance> {
        ctx.debug("JINKO_INST", &self.print());

        match self.kind {
            JkInstKind::Dump => println!("{}", ctx.print()),
            JkInstKind::Quit => std::process::exit(0),
            JkInstKind::Ir => eprintln!("usage: {:?} <statement|expr>", JkInstKind::Ir),
        };

        // FIXME: Is that true?
        // JinkInsts cannot return anything. They simply act directly from the context,
        // on the context.
        None
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

        assert!(inst.is_err(), "tamer is not a valid ctx directive")
    }

    #[test]
    fn t_valid_inst_no_args() {
        let (_, fc) = Construct::function_call("dump()").unwrap();
        let inst = JkInst::from_function_call(fc);

        assert!(inst.is_ok(), "dump is a valid ctx directive")
    }

    #[test]
    fn t_valid_inst_with_args() {
        let (_, fc) = Construct::function_call("ir(fn)").unwrap();
        let inst = JkInst::from_function_call(fc);

        assert!(
            inst.is_ok(),
            "ir(func) is a valid use of the ir ctx directive"
        )
    }
}
