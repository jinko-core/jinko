//! The FFI module allows the jinko context to call into native code.
//! Primitive types are converted to their C counterparts.
//! FIXME

use crate::instruction::{FunctionCall, FunctionDec};
use crate::{
    log, Context, ErrKind, Error, FromObjectInstance, JkBool, JkFloat, JkInt, JkString,
    ObjectInstance, ToObjectInstance,
};

use libffi::high::{arg, call as ffi_call, Arg as FfiArg, CodePtr};
use libloading::{Library, Symbol};

use std::ffi::{CStr, CString};
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

enum FfiJkArg {
    Int(i64),
    Float(f64),
    Pointer(*const ()),
    Error,
}

pub fn execute(
    dec: &FunctionDec,
    call: &FunctionCall,
    ctx: &mut Context,
) -> Result<Option<ObjectInstance>, Error> {
    log!("ext call: {}", call.name());
    let sym = call.name().as_bytes();

    let mut errors = vec![];

    let jk_args: Vec<ObjectInstance> = call
        .args()
        .iter()
        .map(|arg| arg.execute(ctx).unwrap())
        .collect();

    // We need pointers for strings to live long enough... So keep them here
    // for the duration of the function call
    let mut strings = Vec::new();

    // FIXME: Don't unwrap
    let ffi_jk_args: Vec<FfiJkArg> = jk_args
        .iter()
        .zip(
            dec.args()
                .iter()
                .map(|dec_arg| dec_arg.get_type().id().to_owned()),
        )
        .map(|(arg_value, arg_ty)| match arg_ty.as_str() {
            "int" => FfiJkArg::Int(JkInt::from_instance(arg_value).0),
            "bool" => FfiJkArg::Int(JkBool::from_instance(arg_value).0 as i64),
            "float" => FfiJkArg::Float(JkFloat::from_instance(arg_value).0),
            "string" => {
                let arg = JkString::from_instance(arg_value).0;
                let arg = CString::new(arg).unwrap();
                let res = FfiJkArg::Pointer(arg.as_ptr() as *const ());
                strings.push(arg);
                res
            }
            _ => {
                errors.push(Error::new(ErrKind::ExternFunc).with_msg(format!(
                    "ffi module does not support calls with arguments of type {} yet",
                    arg_ty
                )));
                FfiJkArg::Error
            }
        })
        .collect();

    if !errors.is_empty() {
        errors.into_iter().for_each(|e| ctx.error(e));
        return Err(Error::new(ErrKind::ExternFunc));
    }

    // At this point, we cannot have errors anymore and will have returned from
    // the function instead of making the call
    let args: Vec<FfiArg> = ffi_jk_args
        .iter()
        .map(|ffi_jk_arg| match ffi_jk_arg {
            FfiJkArg::Int(i) => arg(i),
            FfiJkArg::Float(f) => arg(f),
            FfiJkArg::Pointer(p) => arg(p),
            _ => unreachable!(),
        })
        .collect();

    for lib in ctx.libs().iter() {
        // FIXME: Rework this
        let func = unsafe { lib.get::<Symbol<fn()>>(sym) };
        let func = match func {
            Ok(func) => CodePtr::from_ptr(unsafe { func.into_raw().into_raw() }),
            Err(_) => continue,
        };

        unsafe {
            match dec.ty() {
                None => {
                    ffi_call::<()>(func, &args);
                    return Ok(None);
                }
                Some(ty) => match ty.id() {
                    "int" => {
                        return Ok(Some(
                            JkInt::from(ffi_call::<i64>(func, &args)).to_instance(),
                        ))
                    }
                    "bool" => {
                        return Ok(Some(
                            JkBool::from(ffi_call::<i32>(func, &args) != 0).to_instance(),
                        ))
                    }
                    "string" => {
                        let raw_ptr = ffi_call::<*mut i8>(func, &args);
                        log!("Pointer: {:p}", raw_ptr);
                        // FIXME: Do we really want to return an empty string if the ffi
                        // function returns NULL?
                        return if raw_ptr.is_null() {
                            Ok(Some(JkString::from(String::new()).to_instance()))
                        } else {
                            Ok(Some(
                                JkString::from(CStr::from_ptr(raw_ptr).to_str().unwrap())
                                    .to_instance(),
                            ))
                        };
                    }
                    _ => {
                        ctx.error(Error::new(ErrKind::ExternFunc).with_msg(format!(
                            "ffi module does not support {} as a return type",
                            ty
                        )));
                        return Err(Error::new(ErrKind::ExternFunc));
                    }
                },
            }
        }
    }

    Err(Error::new(ErrKind::ExternFunc).with_msg(format!(
        "could not find external function `{}`",
        call.name()
    )))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::constructs;
    use crate::{jinko, span};
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

        let dec = constructs::expr(span!("ext func no_arg() -> int;"))
            .unwrap()
            .1;
        let dec = dec.downcast_ref::<FunctionDec>().unwrap();
        let call = constructs::expr(span!("no_arg()")).unwrap().1;
        let call = call.downcast_ref::<FunctionCall>().unwrap();

        assert_eq!(
            execute(&dec, &call, &mut i),
            Ok(Some(JkInt::from(15).to_instance()))
        );
    }

    #[test]
    fn t_void_void() {
        let mut i = init_ctx();

        let dec = constructs::expr(span!("ext func print_something();"))
            .unwrap()
            .1;
        let dec = dec.downcast_ref::<FunctionDec>().unwrap();
        let call = constructs::expr(span!("print_something()")).unwrap().1;
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
