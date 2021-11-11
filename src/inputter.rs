//! The Inputter trait allows applications using the jinko crate to specify various
//! input methods and not rely on system-dependant IO operations. For example, this
//! allows you to write a very simple REPL on embedded systems by providing input
//! operations on your board.

pub trait Inputter {
    /// Read the content of an entire file
    fn read_file(&self, path: &str) -> &str;

    /// Return the next line available from the inputter. This is only used in
    /// interactive contexts such as the REPL
    fn read_line(&self) -> &str;
}
