//! The REPL module implements an interactive mode for the jinko ctx. You can
//! use it as is, or run a file and then enter the interactive mode.

mod prompt;
use prompt::Prompt;
use std::path::PathBuf;

use jinko::{
    constructs, log, Context, Error, FromObjectInstance, Instruction, JkConstant, ObjectInstance, CheckedType
};

use crate::InteractResult;

use linefeed::{DefaultTerminal, Interface, ReadResult};

struct ReplInstance(ObjectInstance);

impl std::fmt::Display for ReplInstance {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self.0.ty() {
                CheckedType::Resolved(ty) => match ty.id() {
                    "int" => JkConstant::<i64>::from_instance(&self.0).print(),
                    "float" => JkConstant::<f64>::from_instance(&self.0).print(),
                    "char" => JkConstant::<char>::from_instance(&self.0).print(),
                    "string" => JkConstant::<String>::from_instance(&self.0).print(),
                    "bool" => JkConstant::<bool>::from_instance(&self.0).print(),
                    _ => self.0.as_string(),
                },
                _ => String::new(),
            }
        )
    }
}

pub struct Repl {
    ctx: Option<Context>,
    reader: Interface<DefaultTerminal>,
}

impl Repl {
    /// Parse a new instruction from the user's input. This function uses the parser's
    /// `instruction` method, and can therefore parse any valid Jinko instruction
    fn parse_instruction(input: &str) -> Result<Option<Box<dyn Instruction>>, Error> {
        match input.is_empty() {
            true => Ok(None),
            false => match constructs::expr(input) {
                Ok((_, value)) => Ok(Some(value)),
                Err(e) => Err(Error::from(e)),
            },
        }
    }

    pub fn new() -> std::io::Result<Repl> {
        Ok(Repl {
            ctx: None,
            reader: Interface::new("jinko")?,
        })
    }

    pub fn with_context(self, ctx: Context) -> Repl {
        Repl {
            ctx: Some(ctx),
            ..self
        }
    }

    fn setup_context(ctx: &mut Context) {
        ctx.set_path(Some(PathBuf::from("repl")));

        ctx.execute().unwrap();

        ctx.emit_errors();
    }

    /// Launch the REPL
    pub fn launch(self) -> InteractResult {
        log!("starting REPL");

        let mut ctx = match self.ctx {
            Some(ctx) => ctx,
            None => Context::new(),
        };

        Repl::setup_context(&mut ctx);

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

            if ctx.type_check(&*inst).is_err() {
                ctx.emit_errors();
                ctx.clear_errors();
                continue;
            }

            if let Some(result) = inst.execute(&mut ctx) {
                println!("{}", ReplInstance(result));
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
