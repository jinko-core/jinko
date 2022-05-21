//! The REPL module implements an interactive mode for the jinko ctx. You can
//! use it as is, or run a file and then enter the interactive mode.

mod prompt;
use prompt::Prompt;
use std::path::PathBuf;

use jinko::{
    context::Context,
    instance::{FromObjectInstance, ObjectInstance},
    instruction::Instruction,
    typechecker::CheckedType,
    value::JkConstant,
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

        ctx.init_stdlib().unwrap();
        ctx.execute().unwrap();

        ctx.emit_errors();
    }

    /// Launch the REPL
    pub fn launch(self) -> InteractResult {
        let mut ctx = match self.ctx {
            Some(ctx) => ctx,
            None => Context::new(Box::new(jinko::io_trait::JkStdReader)),
        };

        Repl::setup_context(&mut ctx);

        self.reader.set_prompt(&Prompt::get(&ctx))?;

        while let ReadResult::Input(input) = self.reader.read_line()? {
            if let Ok(Some(res)) = ctx.eval(&input) {
                println!("{}", ReplInstance(res));
            }

            ctx.clear_errors();

            self.reader.set_prompt(&Prompt::get(&ctx))?;
        }

        Ok((None, ctx))
    }
}
