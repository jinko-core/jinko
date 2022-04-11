//! FieldAccesses represent an access onto a type instance's members.
//! FIXME: Add doc

use crate::context::Context;
use crate::error::{ErrKind, Error};
use crate::generics::{GenericMap, GenericUser};
use crate::instance::ObjectInstance;
use crate::instruction::{InstrKind, Instruction};
use crate::location::SpanTuple;
use crate::log;
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
        let calling_instance = self.instance.execute(ctx)?;
        let field_instance = calling_instance.get_field_unchecked(&self.field_name);

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
        log!("field access enter: {}", &self.print());

        let field_instance = self.get_field_instance(ctx);

        log!("field access exit: {}", &self.print());

        field_instance
    }

    fn location(&self) -> Option<&SpanTuple> {
        self.location.as_ref()
    }
}

impl TypeCheck for FieldAccess {
    fn resolve_type(&mut self, ctx: &mut TypeCtx) -> Result<CheckedType, Error> {
        let instance_ty = self.instance.type_of(ctx)?;
        let instance_ty_name = match &instance_ty {
            CheckedType::Resolved(ty) => ty.id(),
            CheckedType::Void => {
                return Err(Error::new(ErrKind::TypeChecker)
                    .with_msg(format!(
                        "trying to access field `{}` on statement",
                        self.field_name
                    ))
                    .with_loc(self.instance.location().cloned()));
            }
            // FIXME: Remove this once we don't have ::Later and ::Error variants
            // anymore
            _ => unreachable!(),
        };

        // We can unwrap here since the type that was resolved from the instance HAS
        // to exist. If it does not, this is an interpreter error
        let dec = ctx.get_custom_type(instance_ty_name).unwrap();

        match dec
            .fields()
            .iter()
            .find(|dec_arg| dec_arg.name() == self.field_name)
        {
            Some(dec_arg) => Ok(CheckedType::Resolved(dec_arg.get_type().clone())),
            None => Err(Error::new(ErrKind::TypeChecker)
                .with_msg(format!(
                    "trying to access field `{}` on instance of type `{}`",
                    self.field_name, instance_ty
                ))
                .with_loc(self.location.clone())),
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
    fn resolve_usages(&mut self, type_map: &GenericMap, ctx: &mut TypeCtx) -> Result<(), Error> {
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
        jinko! {
            type Point(x: int, y:int);
            func basic() -> Point { Point ( x : 15, y : 14 )}
            b = basic();
        }
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
        jinko_fail! {
            func void() {}

            void().field
        };
    }

    #[test]
    fn t_field_access_unknown_field() {
        jinko_fail! {
            type Point(x: int, y:int);
            func basic() -> Point { Point ( x : 15, y : 14 )}
            b = basic();

            b.not_a_field
        };
    }

    #[test]
    fn t_field_access_field_on_primitive() {
        jinko_fail! {
            i = 12;

            i.field_on_primitive()
        };
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
