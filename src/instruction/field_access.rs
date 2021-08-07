//! FieldAccesses represent an access onto a type instance's members.
//! FIXME: Add doc

use crate::{InstrKind, Instruction, Interpreter, JkErrKind, JkError, Rename};

#[derive(Clone)]
pub struct FieldAccess {
    instance: Box<dyn Instruction>,
    field_name: String,
}

impl FieldAccess {
    /// Create a new field access from the instance accessed and the field seeked
    pub fn new(instance: Box<dyn Instruction>, field_name: String) -> FieldAccess {
        FieldAccess {
            instance,
            field_name,
        }
    }
}

impl Instruction for FieldAccess {
    fn kind(&self) -> InstrKind {
        // A field access can only ever be an expression, since we cannot store statements
        // in a type
        InstrKind::Expression(None)
    }

    fn print(&self) -> String {
        format!("{}.{}", self.instance.print(), self.field_name)
    }

    fn execute(&self, interpreter: &mut Interpreter) -> Result<InstrKind, JkError> {
        interpreter.debug("FIELD ACCESS ENTER", &self.print());

        let calling_instance = match self.instance.execute(interpreter)? {
            InstrKind::Statement | InstrKind::Expression(None) => {
                return Err(JkError::new(
                    JkErrKind::Interpreter,
                    format!(
                        "instance `{}` is a statement and cannot be accessed",
                        self.instance.print()
                    ),
                    None,
                    self.print(),
                ))
            }
            InstrKind::Expression(Some(i)) => i,
        };
        let field_instance = calling_instance.get_field(&self.field_name)?;

        interpreter.debug("FIELD ACCESS EXIT", &self.print());

        Ok(InstrKind::Expression(Some(field_instance)))
    }
}

impl Rename for FieldAccess {
    fn prefix(&mut self, _: &str) {
        // FIXME: Do we want to do something?
        // todo!()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::instance::ToObjectInstance;
    use crate::parser::Construct;
    use crate::JkInt;

    fn setup() -> Interpreter {
        let mut interpreter = Interpreter::new();

        let inst = Construct::instruction("type Point(x: int, y: int); ")
            .unwrap()
            .1;
        inst.execute(&mut interpreter).unwrap();

        let inst = Construct::instruction("func basic() -> Point { Point { x = 15, y = 14 }}")
            .unwrap()
            .1;
        inst.execute(&mut interpreter).unwrap();

        let inst = Construct::instruction("b = basic();").unwrap().1;
        inst.execute(&mut interpreter).unwrap();

        interpreter
    }

    #[test]
    fn t_valid_field_access() {
        let mut interpreter = setup();

        let inst = Construct::instruction("b.x").unwrap().1;
        let res = match inst.execute(&mut interpreter).unwrap() {
            InstrKind::Expression(Some(i)) => i,
            _ => return assert!(false, "Error when accessing valid field"),
        };

        let mut exp = JkInt::from(15).to_instance();
        // FIXME: Remove once TypeInstantiations create typed instances
        exp.set_ty(None);

        assert_eq!(res, exp)
    }

    #[test]
    fn t_valid_field_access_from_type_instantiation() {
        let mut interpreter = setup();

        let inst = Construct::instruction("Point { x = 1, y = 2 }.x")
            .unwrap()
            .1;
        let res = match inst.execute(&mut interpreter).unwrap() {
            InstrKind::Expression(Some(i)) => i,
            _ => return assert!(false, "Error when accesing valid field"),
        };

        let mut exp = JkInt::from(1).to_instance();
        // FIXME: Remove once TypeInstantiations create typed instances
        exp.set_ty(None);

        assert_eq!(res, exp)
    }

    #[test]
    #[ignore] // FIXME: Do not ignore once we can type instance fields
    fn t_valid_multi_field_access() {
        let mut interpreter = Interpreter::new();

        let inst = Construct::instruction("type Pair1(x: int, y: int)")
            .unwrap()
            .1;
        inst.execute(&mut interpreter).unwrap();

        let inst = Construct::instruction("type Pair2(x: Pair1, y: int)")
            .unwrap()
            .1;
        inst.execute(&mut interpreter).unwrap();

        let inst = Construct::instruction("p = Pair2 { x = Pair1 { x = 1, y = 2}, y = 3}")
            .unwrap()
            .1;
        inst.execute(&mut interpreter).unwrap();

        let inst = Construct::instruction("p.x.y").unwrap().1;
        let res = match inst.execute(&mut interpreter).unwrap() {
            InstrKind::Expression(Some(i)) => i,
            _ => return assert!(false, "Error when accessing valid multi field"),
        };

        let mut expected = JkInt::from(2).to_instance();
        // FIXME: Remove once typechecking is implemented
        expected.set_ty(None);

        assert_eq!(res, expected)
    }

    #[test]
    fn t_field_access_on_void() {
        let mut interpreter = setup();

        let inst = Construct::instruction("func void() {}").unwrap().1;
        inst.execute(&mut interpreter).unwrap();

        let inst = Construct::instruction("void().field").unwrap().1;
        assert!(inst.execute(&mut interpreter).is_err())
    }

    #[test]
    fn t_field_access_unknown_field() {
        let mut interpreter = setup();

        let inst = Construct::instruction("b.not_a_field").unwrap().1;
        assert!(inst.execute(&mut interpreter).is_err())
    }

    #[test]
    fn t_field_access_field_on_primitive() {
        let mut interpreter = setup();

        let inst = Construct::instruction("i = 12").unwrap().1;
        inst.execute(&mut interpreter).unwrap();

        let inst = Construct::instruction("i.field_on_primitive").unwrap().1;
        assert!(inst.execute(&mut interpreter).is_err())
    }
}
