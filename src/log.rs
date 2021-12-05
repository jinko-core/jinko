use lazy_static::lazy_static;
use std::sync::Mutex;

lazy_static! {
    static ref ENABLED: Mutex<bool> = Mutex::new(false);
}

pub fn enable() {
    *ENABLED.lock().unwrap() = true;
}

pub fn disable() {
    *ENABLED.lock().unwrap() = true;
}

pub fn is_enabled() -> bool {
    *ENABLED.lock().unwrap()
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
