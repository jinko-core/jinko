//! The constant module symbolizes a constant in the source code. There are 4 different
//! types of constants: Integer, Float, String and Character

// use super::Value;

use crate::instruction::{InstrKind, Instruction};

/// The 4 different types of constants
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum ConstKind {
    Char,
    Str,
    Int,
    Float,
}

/// The Constant contains a kind, and the associated value
#[derive(Debug)]
pub struct Constant {
    kind: ConstKind,

    pub char_value: Option<char>,
    pub str_value: Option<String>,
    pub int_value: Option<i64>,
    pub float_value: Option<f64>,
}

impl Constant {
    pub fn new(kind: ConstKind) -> Constant {
        Constant {
            kind,
            char_value: None,
            str_value: None,
            int_value: None,
            float_value: None,
        }
    }

    pub fn with_cv(mut self, cv: char) -> Constant {
        self.char_value = Some(cv);
        self
    }

    pub fn with_sv(mut self, sv: String) -> Constant {
        self.str_value = Some(sv);
        self
    }

    pub fn with_iv(mut self, iv: i64) -> Constant {
        self.int_value = Some(iv);
        self
    }

    pub fn with_fv(mut self, fv: f64) -> Constant {
        self.float_value = Some(fv);
        self
    }

    pub fn kind(&self) -> ConstKind {
        self.kind
    }
}

impl Instruction for Constant {
    fn kind(&self) -> InstrKind {
        // FIXME: Add logic
        InstrKind::Expression
    }
}
