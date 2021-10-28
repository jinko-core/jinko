//! The FFI module allows the jinko context to call into native code.
//! Primitive types are converted to their C counterparts.
//! FIXME

use crate::instruction::{FunctionCall, FunctionDec};
use crate::{
    Context, ErrKind, Error, FromObjectInstance, JkInt, JkString, ObjectInstance, ToObjectInstance,
};

use libloading::{Library, Symbol};
use std::ffi::CString;
use std::mem::transmute;
use std::os::raw::c_char;
use std::path::{Path, PathBuf};

fn find_lib(paths: &[PathBuf], lib_path: &Path) -> Result<Library, Error> {
    for dir_base in paths.iter() {
        let lib_path = PathBuf::from(dir_base).join(lib_path);
        if lib_path.exists() {
            if let Ok(lib) = unsafe { Library::new(lib_path) } {
                return Ok(lib);
            }
        }
    }

    Err(Error::new(ErrKind::ExternFunc))
}

fn lib_from_root(base: &Path, lib_path: &Path) -> Result<Library, Error> {
    // We search through all entries in the `base` directory as well as all the entries
    // in the directories contained in `base`.
    let mut dirs = vec![PathBuf::from(base)];
    for entry in base.read_dir()? {
        let entry = entry?;
        if entry.file_type()?.is_dir() {
            dirs.push(entry.path())
        }
    }

    find_lib(&dirs, lib_path)
}

/// Fetch libraries from the path according to dlopen's (simplified) rules.
/// If the LD_LIBRARY_PATH variable is set and contains a colon, then assume it
/// contains a list of colon-separated directories and search through them.
/// Then, search through /lib/, and then finally through /usr/lib/
fn lib_from_path(lib_path: PathBuf) -> Result<Library, Error> {
    if let Ok(lib) = lib_from_ld_library_path(&lib_path) {
        return Ok(lib);
    }

    if let Ok(lib) = lib_from_root(&PathBuf::from("/lib"), &lib_path) {
        return Ok(lib);
    }

    if let Ok(lib) = lib_from_root(&PathBuf::from("/usr/lib"), &lib_path) {
        return Ok(lib);
    }

    Err(Error::new(ErrKind::ExternFunc))
}

/// Look through the paths specified in the LD_LIBRARY_PATH environment variable (if any)
fn lib_from_ld_library_path(lib_path: &Path) -> Result<Library, Error> {
    let paths = std::env::var("LD_LIBRARY_PATH")?;

    for dir in paths.split(':') {
        let path = PathBuf::from(dir).join(&lib_path);
        if path.exists() {
            return unsafe { Ok(Library::new(path)?) };
        }
    }

    Err(Error::new(ErrKind::ExternFunc))
}

pub fn link_with(ctx: &mut Context, lib_path: PathBuf) -> Result<(), Error> {
    let lib = if std::path::Path::new(&lib_path).exists() {
        unsafe { Library::new(lib_path)? }
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
) -> Result<Option<ObjectInstance>, Error> {
    ctx.debug("EXT CALL", call.name());
    let sym = call.name().as_bytes();

    // FIXME: Don't unwrap
    let args: Vec<ObjectInstance> = call
        .args()
        .iter()
        .map(|arg| arg.execute(ctx).unwrap())
        .collect();

    for lib in ctx.libs().iter() {
        let func = unsafe { lib.get::<Symbol<fn()>>(sym) };
        let func = match func {
            Ok(func) => func,
            Err(_) => continue,
        };
        match call.args().len() {
            0 => match dec.ty() {
                None => {
                    func();
                    return Ok(None);
                }
                Some(ty) => match ty.id() {
                    "int" => {
                        let func = unsafe { transmute::<fn(), fn() -> i64>(**func) };
                        let res = func();
                        return Ok(Some(JkInt::from(res).to_instance()));
                    }
                    _ => unreachable!(),
                },
            },
            1 => match dec.args()[0].get_type().id() {
                "string" => match dec.ty() {
                    None => {
                        let func = unsafe { transmute::<fn(), fn(*const c_char)>(**func) };
                        let arg = JkString::from_instance(&args[0]).0;
                        let arg = CString::new(arg.as_str()).unwrap();
                        let arg = arg.as_ptr();
                        func(arg);
                        return Ok(None);
                    }
                    Some(ty) => match ty.id() {
                        "int" => {
                            let func =
                                unsafe { transmute::<fn(), fn(*const c_char) -> i64>(**func) };
                            let arg = JkString::from_instance(&args[0]).0;
                            let arg = CString::new(arg.as_str()).unwrap();
                            let arg = arg.as_ptr();
                            let res = func(arg);
                            return Ok(Some(JkInt::from(res).to_instance()));
                        }
                        _ => unreachable!(),
                    },
                },
                _ => unreachable!(),
            },
            _ => unreachable!(),
        }
    }

    Err(Error::new(ErrKind::ExternFunc)
        .with_msg(format!("cannot call external function `{}`", call.name())))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::jinko;
    use crate::parser::constructs;
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

        let dec = constructs::expr("ext func no_arg() -> int;").unwrap().1;
        let dec = dec.downcast_ref::<FunctionDec>().unwrap();
        let call = constructs::expr("no_arg()").unwrap().1;
        let call = call.downcast_ref::<FunctionCall>().unwrap();

        assert_eq!(
            execute(&dec, &call, &mut i),
            Ok(Some(JkInt::from(15).to_instance()))
        );
    }

    #[test]
    fn t_void_void() {
        let mut i = init_ctx();

        let dec = constructs::expr("ext func print_something();").unwrap().1;
        let dec = dec.downcast_ref::<FunctionDec>().unwrap();
        let call = constructs::expr("print_something()").unwrap().1;
        let call = call.downcast_ref::<FunctionCall>().unwrap();

        assert_eq!(execute(&dec, &call, &mut i), Ok(None));
    }

    #[test]
    fn load_libs_stress() {
        let ld_library_path = std::env::var("LD_LIBRARY_PATH").unwrap_or(String::new());
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
    }
}
