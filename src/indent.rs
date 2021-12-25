//! Helps when formatting indentation and keeping track of it

use std::fmt::Display;

/// Keep and print an indentation level
#[derive(Default, Copy, Clone)]
pub struct Indent(usize);

impl Indent {
    /// Step to increment the indent by
    const INDENT_STEP: usize = 4;

    /// Increment an indent to the next step, returning a new one
    pub fn increment(self) -> Indent {
        Indent(self.0 + Indent::INDENT_STEP)
    }

    /// Decrement an indent to the next step, stopping at zero spaces
    pub fn decrement(self) -> Indent {
        Indent(self.0.saturating_sub(Indent::INDENT_STEP))
    }
}

impl Display for Indent {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{: <1$}", "", self.0)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn t_indent_nothing() {
        assert_eq!(format!("{}", Indent::default()), String::new());
    }

    #[test]
    fn t_indent_inc() {
        assert_eq!(
            format!("{}", Indent::default().increment()),
            String::from("    ")
        );
    }

    #[test]
    fn t_indent_inc_twice() {
        assert_eq!(
            format!("{}", Indent::default().increment().increment()),
            String::from("        ")
        );
    }

    #[test]
    fn t_indent_inc_dec() {
        assert_eq!(
            format!("{}", Indent::default().increment().decrement()),
            String::new()
        );
    }

    #[test]
    fn t_indent_inc_dec_dec_stops_at_0() {
        assert_eq!(
            format!("{}", Indent::default().increment().decrement().decrement()),
            String::new()
        );
    }
}
