//! The FFI module allows the jinko interpreter to call into native code.
//! Primitive types are converted to their C counterparts.
//! FIXME

use crate::instruction::FunctionCall;
use crate::{InstrKind, Instruction, Interpreter, JkErrKind, JkError, ObjectInstance};

pub struct JkFfi;

impl JkFfi {
    pub fn execute(call: &FunctionCall, interpreter: &Interpreter) -> Result<InstrKind, JkError> {
        interpreter.debug("EXT CALL", call.name());

        for lib in interpreter.libs().iter() {
            let sym = call.name().as_bytes();
            unsafe {
                if lib.get::<libloading::Symbol<()>>(&sym).is_ok() {
                    match call.args().len() {
                        0 => {
                            let f = lib.get::<libloading::Symbol<fn() -> ObjectInstance>>(&sym)?;
                            return Ok(InstrKind::Expression(Some(f())));
                        }
                        _ => {}
                    }
                }
            }
        }

        Err(JkError::new(
            JkErrKind::ExternFunc,
            format!("cannot call external function {}", call.name()),
            None,
            call.print(),
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::Construct;
    use crate::{JkInt, ToObjectInstance};

    fn declare_ext_funcs(i: &mut Interpreter) {
        let dec0 = Construct::instruction("ext func no_arg() -> int;").unwrap().1;
        let dec1 = Construct::instruction("ext func square(v: int) -> int;").unwrap().1;
        let dec2 = Construct::instruction("ext func add(lhs: int, rhs: int) -> int;").unwrap().1;

        dec0.execute(i).unwrap();
        dec1.execute(i).unwrap();
        dec2.execute(i).unwrap();
    }

    fn init_interpreter() -> Interpreter {
        let mut i = Interpreter::new();

        i.add_lib(unsafe { libloading::Library::new("./tests/fixtures/clib/lib.so").unwrap() });

        declare_ext_funcs(&mut i);

        i
    }

    #[test]
    fn t_load() {
        let _ = init_interpreter();
    }

    #[test]
    #[ignore]
    fn t_no_arg() {
        let i = init_interpreter();

        let call = Construct::function_call("no_arg()").unwrap().1;

        assert_eq!(
            JkFfi::execute(&call, &i).unwrap(),
            InstrKind::Expression(Some(JkInt::from(15).to_instance()))
        );
    }

    #[test]
    #[ignore]
    fn t_one_arg() {
        let i = init_interpreter();

        let call = Construct::function_call("add(12, 15)").unwrap().1;

        assert_eq!(
            JkFfi::execute(&call, &i).unwrap(),
            InstrKind::Expression(Some(JkInt::from(27).to_instance()))
        );
    }
}
