//! Represents a resizeable string in Jinko

use super::Value;

pub struct JinkString(String);

impl From<&str> for JinkString {
    fn from(s: &str) -> Self {
        JinkString(s.to_owned())
    }
}

impl Value for JinkString {}
