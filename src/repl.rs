//! The REPL module implements an interactive mode for the jinko ctx. You can
//! use it as is, or run a file and then enter the interactive mode.

mod prompt;
use prompt::Prompt;
use std::path::PathBuf;

use linefeed::{DefaultTerminal, Interface, ReadResult};

use crate::args::Args;
use crate::{
    parser::Construct, Context, Error, FromObjectInstance, Instruction, InteractResult, JkConstant,
    ObjectInstance,
};

// FIXME:
// - Is Display really how we want to go about it?
// - Cleanup the code
// - This should not be here
impl std::fmt::Display for ObjectInstance {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self.ty() {
                Some(ty) => match ty.name() {
                    "int" => JkConstant::<i64>::from_instance(self).print(),
                    "float" => JkConstant::<f64>::from_instance(self).print(),
                    "char" => JkConstant::<char>::from_instance(self).print(),
                    "string" => JkConstant::<String>::from_instance(self).print(),
                    "bool" => JkConstant::<bool>::from_instance(self).print(),
                    _ => self.as_string(),
                },
                None => format!(""),
            }
        )
    }
}

pub struct Repl<'args> {
    args: &'args Args,
    ctx: Option<Context>,
    reader: Interface<DefaultTerminal>,
}

impl<'args> Repl<'args> {
    /// Parse a new instruction from the user's input. This function uses the parser's
    /// `instruction` method, and can therefore parse any valid Jinko instruction
    fn parse_instruction(input: &str) -> Result<Option<Box<dyn Instruction>>, Error> {
        match input.is_empty() {
            true => Ok(None),
            false => match Construct::instruction(input) {
                Ok((_, value)) => Ok(Some(value)),
                Err(e) => Err(Error::from(e)),
            },
        }
    }

    pub fn new(args: &Args) -> std::io::Result<Repl> {
        Ok(Repl {
            args,
            ctx: None,
            reader: Interface::new("jinko")?,
        })
    }

    pub fn with_context(self, ctx: Context) -> Repl<'args> {
        Repl {
            ctx: Some(ctx),
            ..self
        }
    }

    fn setup_context(args: &Args, ctx: &mut Context) {
        ctx.set_debug(args.debug());
        ctx.set_path(Some(PathBuf::from("repl")));

        let ep = ctx.entry_point.block().unwrap().clone();
        ep.instructions().iter().for_each(|inst| {
            inst.execute(ctx);
        });
        if let Some(last) = ep.last() {
            last.execute(ctx);
        }
    }

    /// Launch the REPL
    pub fn launch(self) -> InteractResult {
        let mut ctx = match self.ctx {
            Some(ctx) => ctx,
            None => Context::new(),
        };

        Repl::setup_context(self.args, &mut ctx);

        self.reader.set_prompt(&Prompt::get(&ctx))?;

        while let ReadResult::Input(input) = self.reader.read_line()? {
            let inst = match Repl::parse_instruction(&input) {
                Ok(i) => i,
                Err(e) => {
                    e.emit(PathBuf::from("repl").as_path());
                    continue;
                }
            };

            let inst = match inst {
                Some(i) => i,
                None => continue,
            };

            if let Some(result) = inst.execute(&mut ctx) {
                println!("{}", result);
            };

            ctx.emit_errors();
            ctx.clear_errors();

            self.reader.set_prompt(&Prompt::get(&ctx))?;
        }

        Ok((None, ctx))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn t_valid_parse_instruction() {
        let inst = "a = 2";
        let res = Repl::parse_instruction(inst);

        assert!(res.is_ok());
    }

    // FIXME: Add test to parse multiple instructions in one line
}
