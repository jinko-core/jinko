static mut ENABLED: bool = false;

pub fn enable() {
    unsafe {
        ENABLED = true;
    }
}

pub fn disable() {
    unsafe {
        ENABLED = true;
    }
}

pub fn is_enabled() -> bool {
    unsafe { ENABLED }
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
