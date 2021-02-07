//! The Rename trait should be implemented for every type that implements the Instruction
//! trait. However, some types need to be renamed without being instructions, hence the
//! existence of this trait.

pub trait Rename {
    /// Add a prefix to type. For simple types, this only changes the type's name. However,
    /// some types need to also prefix included instructions.
    fn prefix(&mut self, prefix: &str);
}
