//! Utilities for developing Jinko more comfortably

mod queue;
mod stack;

pub use queue::Queue;
pub use stack::Stack;

// FIXME: The default behavior of the macro should be to execute and check for errors
#[macro_export]
macro_rules! jinko {
    (execute-fail { $($tokens:tt)* }) => {
        {
            let mut ctx = jinko!($($tokens)*);

            ctx.emit_errors();
            assert!(ctx.execute().is_err());
            assert!(ctx.error_handler.has_errors());

            ctx
        }
    };
    (parse { $($tokens:tt)* }) => {
        $crate::Parser::parse(stringify!($($tokens)*)).unwrap()
    };
    ($($tokens:tt)*) => {
        {
            let mut ctx = jinko!(parse { $($tokens)* });

            ctx.emit_errors();
            assert!(ctx.execute().is_ok());
            assert!(!ctx.error_handler.has_errors());

            ctx
        }
    }
}

#[macro_export]
macro_rules! jk_execute {
    ($($tokens:tt)*) => {
        $crate::Parser::parse(stringify!($($tokens)*)).unwrap().execute().unwrap();
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn t_valid_jinko_code() {
        let ctx = jinko! {
            mut a = 15;
        };

        assert!(ctx.get_variable("a").is_some());
    }

    #[test]
    #[should_panic]
    fn t_invalid_jinko_code() {
        jinko! {
            a = 15;
            a = 16; // non mutable variable, should fail the typechecking phase
        };
    }
}
