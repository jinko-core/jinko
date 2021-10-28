//! Utilities for developing Jinko more comfortably

#[macro_export]
macro_rules! jk_parse {
    ($($tokens:tt)*) => {
        $crate::Parser::parse(stringify!($($tokens)*)).unwrap()
    }
}

#[macro_export]
macro_rules! jinko {
    ($($tokens:tt)*) => {
        {
            let mut ctx = $crate::jk_parse! { $($tokens)* };

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
            let mut ctx = $crate::jk_parse! { $($tokens)* };

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
