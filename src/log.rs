use lazy_static::lazy_static;
use std::sync::atomic::{AtomicBool, Ordering};

lazy_static! {
    static ref ENABLED: AtomicBool = AtomicBool::new(false);
}

pub fn enable() {
    ENABLED.store(true, Ordering::Relaxed);
}

pub fn disable() {
    ENABLED.store(false, Ordering::Relaxed);
}

pub fn is_enabled() -> bool {
    ENABLED.load(Ordering::Relaxed)
}

#[macro_export]
macro_rules! log {
    (typectx, $($token:tt)*) => (
        if $crate::log::is_enabled() {
            use colored::Colorize;

            eprintln!("<{}> [{}] {}", "LOG".black().on_purple(), "type context".black().on_green(), format_args!($($token)*));
        }
    );
    (generics, $($token:tt)*) => (
        if $crate::log::is_enabled() {
            use colored::Colorize;

            eprintln!("<{}> [{}] {}", "LOG".black().on_purple(), "generics".black().on_blue(), format_args!($($token)*));
        }
    );
    (rare, $($token:tt)*) => (
        if $crate::log::is_enabled() {
            use colored::Colorize;

            eprintln!("<{}> [{}] {}", "LOG".black().on_purple(), "RARE".black().on_red(), format_args!($($token)*));
        }
    );
    ($($token:tt)*) => (
        if $crate::log::is_enabled() {
            use colored::Colorize;

            eprintln!("<{}> {}", "LOG".black().on_purple(), format_args!($($token)*));
        }
    );
}
