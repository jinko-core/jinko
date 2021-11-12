//! Utilities for developing Jinko more comfortably

mod queue;
mod stack;

pub use queue::Queue;
pub use stack::Stack;

#[macro_export]
macro_rules! jk_parse {
    ($ctx:expr, $($tokens:tt)*) => {
        $crate::Parser::parse($ctx, stringify!($($tokens)*)).unwrap()
    }
}

#[macro_export]
macro_rules! jinko {
    ($($tokens:tt)*) => {
        {
            let mut ctx = $crate::Context::new();

            $crate::jk_parse!(&mut ctx, $($tokens)*);

            ctx.emit_errors();
            assert!(ctx.execute().is_ok());
            assert!(!ctx.error_handler.has_errors());

            ctx
        }
    }
}

#[macro_export]
macro_rules! jinko_fail {
    ($($tokens:tt)*) => {
        {
            let mut ctx = $crate::Context::new();

            $crate::jk_parse! (&mut ctx, $($tokens)*);

            ctx.emit_errors();
            assert!(ctx.execute().is_err());
            assert!(ctx.error_handler.has_errors());

            ctx
        }
    }
}

#[macro_export]
macro_rules! jk_execute {
    ($($tokens:tt)*) => {
        {
            let mut ctx = $crate::Context::new();
            $crate::Parser::parse(&mut ctx, stringify!($($tokens)*)).unwrap();

            ctx.execute().unwrap()
        }
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
