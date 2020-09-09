//! Represents a single character in Jinko

use super::Value;

pub struct JinkChar(char);

impl From<char> for JinkChar {
    fn from(c: char) -> Self {
        JinkChar(c)
    }
}

impl Value for JinkChar {}
