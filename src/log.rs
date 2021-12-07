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
    ($($token:tt)*) => (
        if $crate::log::is_enabled() {
            use colored::Colorize;

            eprintln!("<{}> {}", "LOG".on_purple(), format_args!($($token)*));
        }
    );
}
