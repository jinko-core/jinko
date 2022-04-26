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
macro_rules! debug {
    (loc: $loc:expr, $($token:tt)*) => (
        if $crate::log::is_enabled() {
            let err = $crate::error::Error::new($crate::error::ErrKind::Debug)
                .with_loc(Some($loc))
                .with_msg(format!("{}", format_args!($($token)*)));

            err.emit_debug();
        }
    );
    ($($token:tt)*) => (
        if $crate::log::is_enabled() {
            let err = $crate::error::Error::new($crate::error::ErrKind::Debug)
                .with_msg(format!("{}", format_args!($($token)*)));

            err.emit_debug();
        }
    )
}
