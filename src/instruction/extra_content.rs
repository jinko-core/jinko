//! The ExtraContent instruction is not a real instruction. It contains extra content
//! useful to the user, such as newlines and comments. As a consequence, the execution
//! of such an instruction produces no results. But they are useful when it comes to
//! pretty printing and code formatting.

use crate::typechecker::{CheckedType, TypeCtx};
use crate::Generic;
use crate::{Context, InstrKind, Instruction, ObjectInstance, TypeCheck};

#[derive(Clone)]
pub enum CommentKind {
    Shebang,
    SingleLine,
    MultiLine,
}

#[derive(Clone)]
pub enum ExtraKind {
    Comment(CommentKind),
    WhiteSpace,
}

#[derive(Clone)]
pub struct ExtraContent {
    kind: ExtraKind,
    content: String,
}

impl ExtraContent {
    /// Create a new ExtraContent from a kind and its content
    pub fn new(kind: ExtraKind, content: String) -> ExtraContent {
        ExtraContent { kind, content }
    }

    /// New extra content from a `shebang` comment
    pub fn new_shebang(content: String) -> ExtraContent {
        ExtraContent::new(ExtraKind::Comment(CommentKind::Shebang), content)
    }

    /// New extra content from a `single_line` comment
    pub fn new_single_line(content: String) -> ExtraContent {
        ExtraContent::new(ExtraKind::Comment(CommentKind::SingleLine), content)
    }

    /// New extra content from a `multi_line` comment
    pub fn new_multi_line(content: String) -> ExtraContent {
        ExtraContent::new(ExtraKind::Comment(CommentKind::MultiLine), content)
    }

    /// New whitespaces content
    pub fn new_whitespaces(content: String) -> ExtraContent {
        ExtraContent::new(ExtraKind::WhiteSpace, content)
    }
}

impl Instruction for ExtraContent {
    fn execute(&self, ctx: &mut Context) -> Option<ObjectInstance> {
        log!("comment: {}", self.print().as_str());

        None
    }

    fn kind(&self) -> InstrKind {
        InstrKind::Statement
    }

    fn print(&self) -> String {
        let result = self.content.clone();

        match self.kind {
            ExtraKind::WhiteSpace => result,
            ExtraKind::Comment(CommentKind::Shebang) => format!("#{}", result),
            ExtraKind::Comment(CommentKind::SingleLine) => format!("//{}", result),
            ExtraKind::Comment(CommentKind::MultiLine) => format!("/*{}*/", result),
        }
    }
}

impl TypeCheck for ExtraContent {
    fn resolve_type(&mut self, _ctx: &mut TypeCtx) -> CheckedType {
        // FIXME: This should probably be removed, as well as the ExtraContent struct
        CheckedType::Void
    }
}

impl Generic for ExtraContent {}
