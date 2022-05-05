//! FieldAccesses represent an access onto a type instance's members.
//! FIXME: Add doc

use crate::context::Context;
use crate::error::{ErrKind, Error};
use crate::generics::{GenericMap, GenericUser};
use crate::instance::ObjectInstance;
use crate::instruction::{InstrKind, Instruction};
use crate::location::SpanTuple;
use crate::typechecker::{CheckedType, TypeCheck, TypeCtx};

#[derive(Clone)]
pub struct FieldAccess {
    instance: Box<dyn Instruction>,
    field_name: String,
    cached_type: Option<CheckedType>,
    location: Option<SpanTuple>,
}

impl FieldAccess {
    /// Create a new field access from the instance accessed and the field seeked
    pub fn new(instance: Box<dyn Instruction>, field_name: String) -> FieldAccess {
        FieldAccess {
            instance,
            field_name,
            cached_type: None,
            location: None,
        }
    }

    /// Get a reference to the accessed field's instance
    fn get_field_instance(&self, ctx: &mut Context) -> Option<ObjectInstance> {
        let calling_instance = match self.instance.execute(ctx) {
            None => {
                ctx.error(
                    Error::new(ErrKind::Context)
                        .with_msg(format!(
                            "instance `{}` is a statement and cannot be accessed",
                            self.instance.print()
                        ))
                        .with_loc(self.instance.location().cloned()),
                );
                return None;
            }
            Some(i) => i,
        };
        let field_instance = match calling_instance.get_field(&self.field_name) {
            Ok(field) => field,
            Err(e) => {
                ctx.error(e);
                return None;
            }
        };

        Some(field_instance)
    }

    pub fn set_location(&mut self, location: SpanTuple) {
        self.location = Some(location)
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

    fn execute(&self, ctx: &mut Context) -> Option<ObjectInstance> {
        self.get_field_instance(ctx)
    }

    fn location(&self) -> Option<&SpanTuple> {
        self.location.as_ref()
    }
}

impl TypeCheck for FieldAccess {
    fn resolve_type(&mut self, ctx: &mut TypeCtx) -> CheckedType {
        let instance_ty = self.instance.type_of(ctx);
        let instance_ty = match &instance_ty {
            CheckedType::Resolved(ty) => ty,
            CheckedType::Void => {
                ctx.error(
                    Error::new(ErrKind::TypeChecker)
                        .with_msg(format!(
                            "trying to access field `{}` on statement",
                            self.field_name
                        ))
                        .with_loc(self.instance.location().cloned()),
                );
                return CheckedType::Error;
            }
            _ => return CheckedType::Error,
        };

        // We can unwrap here since the type that was resolved from the instance HAS
        // to exist. If it does not, this is an interpreter error
        let dec = ctx.get_custom_type(instance_ty).unwrap();

        match dec
            .fields()
            .iter()
            .find(|dec_arg| dec_arg.name() == self.field_name)
        {
            Some(dec_arg) => CheckedType::Resolved(dec_arg.get_type().clone()),
            None => {
                ctx.error(
                    Error::new(ErrKind::TypeChecker)
                        .with_msg(format!(
                            "trying to access field `{}` on instance of type `{}`",
                            self.field_name, instance_ty
                        ))
                        .with_loc(self.location.clone()),
                );
                CheckedType::Error
            }
        }
    }

    fn set_cached_type(&mut self, ty: CheckedType) {
        self.cached_type = Some(ty)
    }

    fn cached_type(&self) -> Option<&CheckedType> {
        self.cached_type.as_ref()
    }
}

impl GenericUser for FieldAccess {
    fn resolve_usages(&mut self, type_map: &GenericMap, ctx: &mut TypeCtx) {
        self.instance.resolve_usages(type_map, ctx)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::instance::ToObjectInstance;
    use crate::parser::constructs;
    use crate::value::JkInt;
    use crate::{jinko, jinko_fail, span};

    fn setup() -> Context {
        let ctx = jinko! {
            type Point(x: int, y:int);
            func basic() -> Point { Point ( x : 15, y : 14 )}
            b = basic();
        };

        ctx
    }

    #[test]
    fn t_valid_field_access() {
        let mut ctx = setup();

        let inst = constructs::expr(span!("b.x")).unwrap().1;
        let res = match inst.execute(&mut ctx) {
            Some(i) => i,
            None => return assert!(false, "Error when accessing valid field"),
        };

        let exp = JkInt::from(15).to_instance();

        assert_eq!(res, exp)
    }

    #[test]
    fn t_valid_field_access_from_type_instantiation() {
        let mut ctx = setup();

        let inst = constructs::expr(span!("Point ( x : 1, y : 2 ).x"))
            .unwrap()
            .1;
        let res = match inst.execute(&mut ctx) {
            Some(i) => i,
            None => unreachable!("Error when accessing valid field"),
        };

        let exp = JkInt::from(1).to_instance();

        assert_eq!(res, exp)
    }

    #[test]
    #[ignore] // FIXME: Do not ignore once we can type instance fields
    fn t_valid_multi_field_access() {
        let mut ctx = Context::new();

        let inst = constructs::expr(span!("type Pair1(x: int, y: int)"))
            .unwrap()
            .1;
        inst.execute(&mut ctx).unwrap();

        let inst = constructs::expr(span!("type Pair2(x: Pair1, y: int)"))
            .unwrap()
            .1;
        inst.execute(&mut ctx).unwrap();

        let inst = constructs::expr(span!("p = Pair2 ( x : Pair1 ( x : 1, y : 2), y : 3)"))
            .unwrap()
            .1;
        inst.execute(&mut ctx).unwrap();

        let inst = constructs::expr(span!("p.x.y")).unwrap().1;
        let res = match inst.execute(&mut ctx) {
            Some(i) => i,
            None => unreachable!("Error when accessing valid multi field"),
        };

        let expected = JkInt::from(2).to_instance();

        assert_eq!(res, expected)
    }

    #[test]
    fn t_field_access_on_void() {
        let mut ctx = setup();

        let inst = constructs::expr(span!("func void() {}")).unwrap().1;
        inst.execute(&mut ctx);

        let inst = constructs::expr(span!("void().field")).unwrap().1;
        assert!(inst.execute(&mut ctx).is_none());
        assert!(ctx.error_handler.has_errors())
    }

    #[test]
    fn t_field_access_unknown_field() {
        let mut ctx = setup();

        let inst = constructs::expr(span!("b.not_a_field")).unwrap().1;
        assert!(inst.execute(&mut ctx).is_none());
        assert!(ctx.error_handler.has_errors());
    }

    #[test]
    fn t_field_access_field_on_primitive() {
        let mut ctx = setup();

        let inst = constructs::expr(span!("i = 12")).unwrap().1;
        inst.execute(&mut ctx);

        let inst = constructs::expr(span!("i.field_on_primitive")).unwrap().1;
        assert!(inst.execute(&mut ctx).is_none());
        assert!(ctx.error_handler.has_errors())
    }

    #[test]
    fn tc_missing_field() {
        jinko_fail! {
            type Point(x: int, y: int);
            p = Point(x: 14, y: 15);
            p.non_existent
        };
    }

    #[test]
    fn tc_field_on_primitive_type() {
        jinko_fail! {
            some_int = 14;
            some_int.field
        };
    }

    #[test]
    fn tc_valid_field_access() {
        jinko! {
            type Point(x: int, y: int);
            func normalize(p: Point) -> int { p.x + p.y }
        };
    }
}
