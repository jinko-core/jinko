use crate::Indent;

/// Printing context containing various information such as the current
/// indentation level
#[derive(Default)]
pub struct PrintCtx {
    /// Current indentation level
    indent: Indent,
}

impl PrintCtx {
    /// Write to the printing context
    pub fn write<S: Into<String>>(&self, content: S) {
        print!("{}", content.into())
    }

    /// Write to the printing context with a newline
    pub fn writeln<S: Into<String>>(&self, content: S) {
        print!("{}\n{}", content.into(), self.indent)
    }

    /// Increment the current indentation
    pub fn increment_indent(&mut self) {
        self.indent = self.indent.increment();
    }

    /// Decrement the current indentation
    pub fn decrement_indent(&mut self) {
        self.indent = self.indent.decrement();
    }
}

pub trait PrettyPrint {
    /// Print an instruction as valid jinko code. This may be annotated with
    /// comments
    fn code(&self, _ctx: &mut PrintCtx) {}

    /// Print an instruction as its AST representation. This is used for
    /// debugging purposes
    fn ast(&self, _ctx: &mut PrintCtx) {}
}
