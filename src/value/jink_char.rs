//! Represents a single character in Jinko

pub struct JinkChar(char);

impl From<char> for JinkChar {
    fn from(c: char) -> Self {
        JinkChar(c)
    }
}
