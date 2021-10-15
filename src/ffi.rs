//! The FFI module allows the jinko context to call into native code.
//! Primitive types are converted to their C counterparts.
//! FIXME

use std::path::{Path, PathBuf};

use crate::instruction::{FunctionCall, FunctionDec};
use crate::{Context, ErrKind, Error, JkInt, ObjectInstance, ToObjectInstance};

/// Fetch libraries from the path according to dlopen's (simplified) rules.
/// If the LD_LIBRARY_PATH variable is set and contains a colon, then assume it
/// contains a list of colon-separated directories and search through them.
/// Then, search through /lib/, and then finally through /usr/lib/
fn lib_from_path(lib_path: PathBuf) -> Result<libloading::Library, Error> {
    if let Ok(lib) = lib_from_ld_library_path(&lib_path) {
        return Ok(lib);
    }

    let root_lib_path = PathBuf::from("/lib").join(&lib_path);
    if root_lib_path.exists() {
        if let Ok(lib) = unsafe { libloading::Library::new(root_lib_path) } {
            return Ok(lib);
        }
    }

    let usr_root_lib_path = PathBuf::from("/usr/lib").join(&lib_path);
    if usr_root_lib_path.exists() {
        return unsafe { Ok(libloading::Library::new(usr_root_lib_path)?) };
    }

    Err(Error::new(ErrKind::ExternFunc))
}

/// Look through the paths specified in the LD_LIBRARY_PATH environment variable (if any)
fn lib_from_ld_library_path(lib_path: &Path) -> Result<libloading::Library, Error> {
    let paths = std::env::var("LD_LIBRARY_PATH")?;

    for dir in paths.split(':') {
        let path = PathBuf::from(dir).join(&lib_path);
        dbg!(&path);
        if path.exists() {
            return unsafe { Ok(libloading::Library::new(path)?) };
        }
    }

    Err(Error::new(ErrKind::ExternFunc))
}

pub fn link_with(ctx: &mut Context, lib_path: PathBuf) -> Result<(), Error> {
    let lib = if std::path::Path::new(&lib_path).exists() {
        unsafe { libloading::Library::new(lib_path)? }
    } else {
        lib_from_path(lib_path)?
    };

    ctx.add_lib(lib);

    Ok(())
}

pub fn execute(
    dec: &FunctionDec,
    call: &FunctionCall,
    ctx: &mut Context,
) -> Option<ObjectInstance> {
    ctx.debug("EXT CALL", call.name());
    let sym = call.name().as_bytes();

    for lib in ctx.libs().iter() {
        unsafe {
            if lib.get::<libloading::Symbol<()>>(sym).is_ok() {
                match call.args().len() {
                    0 => {
                        match dec.ty() {
                            None => {
                                if let Ok(f) = lib.get::<libloading::Symbol<fn()>>(sym) {
                                    f();
                                    return None;
                                }
                            }
                            Some(ty) => match ty.id() {
                                "int" => {
                                    if let Ok(f) = lib.get::<libloading::Symbol<fn() -> i64>>(sym) {
                                        let res = f();
                                        return Some(JkInt::from(res).to_instance());
                                    }
                                }
                                _ => unreachable!(),
                            },
                        };
                    }
                    _ => unreachable!(),
                }
            }
        }
    }

    ctx.error(
        Error::new(ErrKind::ExternFunc)
            .with_msg(format!("cannot call external function {}", call.name())),
    );

    None
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::jinko;
    use crate::parser::Construct;
    use crate::{JkInt, ToObjectInstance};

    fn init_ctx() -> Context {
        jinko! {
            link_with("./tests/fixtures/clib/lib.so");

            ext func no_arg() -> int;
            ext func square(v: int) -> int;
            ext func add(lhs: int, rhs: int) -> int;
        }
    }

    #[test]
    fn t_load() {
        let _ = init_ctx();
    }

    #[test]
    fn t_void_int() {
        let mut i = init_ctx();

        let dec = Construct::instruction("ext func no_arg() -> int;")
            .unwrap()
            .1;
        let dec = dec.downcast_ref::<FunctionDec>().unwrap();
        let call = Construct::instruction("no_arg()").unwrap().1;
        let call = call.downcast_ref::<FunctionCall>().unwrap();

        assert_eq!(
            execute(&dec, &call, &mut i),
            Some(JkInt::from(15).to_instance())
        );
    }

    #[test]
    fn t_void_void() {
        let mut i = init_ctx();

        let dec = Construct::instruction("ext func print_something();")
            .unwrap()
            .1;
        let dec = dec.downcast_ref::<FunctionDec>().unwrap();
        let call = Construct::instruction("print_something()").unwrap().1;
        let call = call.downcast_ref::<FunctionCall>().unwrap();

        assert_eq!(execute(&dec, &call, &mut i), None);
    }

    #[test]
    fn load_libs_stress() {
        let ld_library_path = std::env::var("LD_LIBRARY_PATH").unwrap();
        let pwd = std::env::var("PWD").unwrap();
        std::env::set_var(
            "LD_LIBRARY_PATH",
            format!("{}/{}:{}", pwd, "tests/fixtures/clib/", ld_library_path),
        );

        jinko! {
            link_with("lib.so");
        };

        // Load libc, probably from LD_LIBRARY_PATH
        jinko! {
            link_with("libc.so.6");
        };

        std::env::remove_var("LD_LIBRARY_PATH");

        // Load libc without LD_LIBRARY_PATH
        jinko! {
            link_with("libc.so.6");
        };
    }
}
