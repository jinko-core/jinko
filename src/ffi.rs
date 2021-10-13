//! The FFI module allows the jinko context to call into native code.
//! Primitive types are converted to their C counterparts.
//! FIXME

use crate::instruction::{FunctionCall, FunctionDec};
use crate::{Context, ErrKind, Error, JkInt, ObjectInstance, ToObjectInstance};

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
        let mut i = jinko! {
            ext func no_arg() -> int;
            ext func square(v: int) -> int;
            ext func add(lhs: int, rhs: int) -> int;
        };

        i.add_lib(unsafe { libloading::Library::new("./tests/fixtures/clib/lib.so").unwrap() });

        i
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
}
