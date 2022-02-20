//! `JkInst`s are special directives given to the context. There is only a limited
//! amount of them, and they are mostly useful for debugging or testing. They aren't
//! really an `Instruction`, and therefore their implementation lives in the parser
//! module. They are executed at "compile" time, when running through the code first.

use crate::instruction::{FunctionCall, InstrKind, Instruction};
use crate::typechecker::{CheckedType, TypeCtx};
use crate::{log, Context, ErrKind, Error, ObjectInstance, TypeCheck};
use crate::{Generic, SpanTuple};

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
    location: Option<SpanTuple>,
}

impl JkInst {
    /// Construct a `JkInst` from a `FunctionCall`
    pub fn from_function_call(fc: &FunctionCall) -> Result<Self, Error> {
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

        Ok(JkInst {
            kind,
            _args: fc.args().clone(),
            location: fc.location().cloned(),
        })
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
        log!("jinko_inst: {}", &self.print());

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

    fn location(&self) -> Option<&SpanTuple> {
        self.location.as_ref()
    }
}

impl TypeCheck for JkInst {
    fn resolve_type(&mut self, _ctx: &mut TypeCtx) -> CheckedType {
        CheckedType::Void
    }

    fn set_cached_type(&mut self, _ty: CheckedType) {}

    fn cached_type(&self) -> Option<&CheckedType> {
        Some(&CheckedType::Void)
    }
}

impl Generic for JkInst {}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::constructs;
    use crate::{jinko, span};

    #[test]
    fn t_invalid_jkinst() {
        let expr = constructs::expr(span!("tamer()")).unwrap().1;
        let inst = JkInst::from_function_call(expr.downcast_ref().unwrap());

        assert!(inst.is_err(), "tamer is not a valid ctx directive")
    }

    #[test]
    fn t_valid_inst_no_args() {
        let expr = constructs::expr(span!("dump()")).unwrap().1;
        let inst = JkInst::from_function_call(expr.downcast_ref().unwrap());

        assert!(inst.is_ok(), "dump is a valid ctx directive")
    }

    #[test]
    fn t_valid_inst_with_args() {
        let expr = constructs::expr(span!("ir(fn)")).unwrap().1;
        let inst = JkInst::from_function_call(expr.downcast_ref().unwrap());

        assert!(
            inst.is_ok(),
            "ir(func) is a valid use of the ir ctx directive"
        )
    }

    #[test]
    fn tc_valid_jk_inst() {
        jinko! {
            @dump();
        };
    }
}
