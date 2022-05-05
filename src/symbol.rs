//! The symbole module, ideally, internalizes strings to reduce memory footprint
//! and allow reusability. For now, it is a simple wrapper on [`String`] used
//! in various places of the framework

use std::convert::From;

// FIXME: We probably want to implement Clone ourselves or simply keep a Rc<String>
// instead
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Symbol(String);

impl Symbol {
    pub fn access(&self) -> &str {
        self.0.as_str()
    }
}

impl From<String> for Symbol {
    fn from(inner: String) -> Self {
        Symbol(inner)
    }
}

impl From<&str> for Symbol {
    fn from(inner: &str) -> Self {
        Symbol(inner.to_string())
    }
}
