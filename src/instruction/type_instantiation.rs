//! TypeInstantiations are used when instantiating a type. The argument list is given to the
//! type on execution.
//!
//! Instantiating a type is an aggregation site.

use crate::context::Context;
use crate::error::{ErrKind, Error};
use crate::generics::{self, GenericExpander, GenericMap, GenericUser};
use crate::instance::{Name, ObjectInstance};
use crate::instruction::{InstrKind, Instruction, TypeDec, VarAssign};
use crate::location::SpanTuple;
use crate::log;
use crate::symbol::Symbol;
use crate::typechecker::{CheckedType, SpecializedNode, TypeCheck, TypeCtx, TypeId};

use std::rc::Rc;

#[derive(Clone)]
pub struct TypeInstantiation {
    type_name: TypeId,
    generics: Vec<TypeId>,
    fields: Vec<VarAssign>,
    cached_type: Option<CheckedType>,
    location: Option<SpanTuple>,
}

impl TypeInstantiation {
    /// Create a new type instantiation and return it
    pub fn new(type_name: TypeId) -> TypeInstantiation {
        TypeInstantiation {
            type_name,
            generics: vec![],
            fields: vec![],
            cached_type: None,
            location: None,
        }
    }

    /// Add an argument to the given type instantiation
    pub fn add_field(&mut self, arg: VarAssign) {
        self.fields.push(arg)
    }

    /// Return a reference to the instantiated type's name
    pub fn name(&self) -> &TypeId {
        &self.type_name
    }

    /// Return a reference to the list of fields
    pub fn fields(&self) -> &Vec<VarAssign> {
        &self.fields
    }

    /// Get the corresponding type declaration from a context
    fn get_declaration(&self, ctx: &mut Context) -> Rc<TypeDec> {
        // get_type() return a Rc, so this clones the Rc, not the TypeId
        ctx.get_type(self.name()).cloned().unwrap()
    }

    /// Check if the fields received and the fields expected match
    fn check_fields_count(&self, type_dec: &TypeDec) -> Result<(), Error> {
        match self.fields().len() == type_dec.fields().len() {
            true => Ok(()),
            false => Err(Error::new(ErrKind::TypeChecker).with_msg(format!(
                "wrong number of arguments \
                    for type instantiation `{}`: expected {}, got {}",
                self.name().id(),
                type_dec.fields().len(),
                self.fields().len()
            ))),
            // FIXME: Add hint with typedec here
        }
    }

    pub fn set_generics(&mut self, generics: Vec<TypeId>) {
        self.generics = generics
    }

    pub fn set_location(&mut self, location: SpanTuple) {
        self.location = Some(location)
    }

    pub fn resolve_generic_instantiation(
        &mut self,
        dec: TypeDec,
        ctx: &mut TypeCtx,
    ) -> Result<CheckedType, Error> {
        log!(
            "creating specialized type. type generics: {}, instantiation generics {}",
            dec.generics().len(),
            self.generics.len()
        );
        let type_map = GenericMap::create(dec.generics(), &self.generics, ctx)?;

        let specialized_name = generics::mangle(dec.name(), &self.generics);
        log!("specialized name {}", specialized_name);
        if ctx.get_custom_type(&specialized_name).is_none() {
            // FIXME: Remove this clone once we have proper symbols
            let specialized_ty = dec.generate(specialized_name.clone(), &type_map, ctx)?;

            ctx.add_specialized_node(SpecializedNode::Type(specialized_ty))?;
        }

        self.type_name = TypeId::new(Symbol::from(specialized_name));
        self.generics = vec![];

        // Recursively resolve the type of self now that we changed the
        // function to call
        self.type_of(ctx)
    }
}

impl Instruction for TypeInstantiation {
    fn kind(&self) -> InstrKind {
        InstrKind::Expression(None)
    }

    fn print(&self) -> String {
        let mut base = format!("{}(", self.type_name.id());
        let mut first_arg = true;
        for arg in &self.fields {
            if !first_arg {
                base.push_str(", ");
            }

            base.push_str(&arg.print());

            first_arg = false;
        }

        format!("{})", base)
    }

    fn execute(&self, ctx: &mut Context) -> Option<ObjectInstance> {
        let type_dec = self.get_declaration(ctx);

        let mut size: usize = 0;
        let mut data: Vec<u8> = Vec::new();
        let mut fields: Vec<(Name, ObjectInstance)> = Vec::new();
        for (_, named_arg) in self.fields.iter().enumerate() {
            // FIXME: Need to assign the correct field to the field that corresponds
            // in the typedec
            let field_instr = named_arg.value();
            let field_name = named_arg.symbol();

            let instance = field_instr.execute_expression(ctx);
            size += instance.size();

            data.append(&mut instance.data().to_vec());
            fields.push((field_name.to_string(), instance));
        }

        Some(ObjectInstance::new(
            // FIXME: Disgusting, maybe do not use Rc for TypeId?
            CheckedType::Resolved((*type_dec).clone().into()),
            size,
            data,
            Some(fields),
        ))
    }
}

impl TypeCheck for TypeInstantiation {
    fn resolve_type(&mut self, ctx: &mut TypeCtx) -> Result<CheckedType, Error> {
        let dec = match ctx.get_custom_type(self.type_name.id()) {
            Some(ty) => ty.clone(),
            None => {
                return Err(Error::new(ErrKind::TypeChecker)
                    .with_msg(format!(
                        "use of undeclared type `{}`",
                        // FIXME: Remove this clone, not useful
                        CheckedType::Resolved(self.type_name.clone())
                    ))
                    .with_loc(self.location.clone()));
            }
        };

        self.check_fields_count(&dec)?;

        if !dec.generics().is_empty() || !self.generics.is_empty() {
            log!("resolving generic type instantiation");
            return self.resolve_generic_instantiation(dec, ctx);
        }

        let mut errors = vec![];
        for (field_dec, var_assign) in dec.fields().iter().zip(self.fields.iter_mut()) {
            let expected_ty = CheckedType::Resolved(field_dec.get_type().clone());
            let value_ty = var_assign.value_mut().type_of(ctx)?;
            if expected_ty != value_ty {
                errors.push(
                    Error::new(ErrKind::TypeChecker)
                        .with_msg(format!(
                            "trying to assign value of type `{}` to field of type `{}`",
                            value_ty,
                            field_dec.get_type()
                        ))
                        .with_loc(var_assign.location().cloned()),
                );
                // FIXME: Add hint for typedec argument later
            }
        }

        // Propagate all errors at once in the context
        errors.into_iter().for_each(|err| ctx.error(err));

        Ok(CheckedType::Resolved(self.type_name.clone()))
    }

    fn set_cached_type(&mut self, ty: CheckedType) {
        self.cached_type = Some(ty)
    }

    fn cached_type(&self) -> Option<&CheckedType> {
        self.cached_type.as_ref()
    }
}

impl GenericUser for TypeInstantiation {
    fn resolve_usages(&mut self, type_map: &GenericMap, ctx: &mut TypeCtx) -> Result<(), Error> {
        log!(generics, "type name: {}", self.type_name);
        let dec = match ctx.get_custom_type(self.type_name.id()) {
            Some(t) => t,
            None => {
                return Err(Error::new(ErrKind::Generics).with_msg(format!("trying to access undeclared type when resolving generic type instantiation: `{}`", self.type_name)).with_loc(self.location.clone()));
            }
        };

        let new_types = match type_map.specialized_types(dec.generics()) {
            Err(e) => {
                return Err(e.with_loc(self.location().cloned()));
            }
            Ok(new_t) => new_t,
        };

        let new_name = generics::mangle(self.type_name.id(), &new_types);
        let old_name = String::from(self.type_name.id());
        self.type_name = TypeId::new(Symbol::from(new_name.clone()));
        self.generics = vec![];

        // Aggregation site
        self.fields.iter_mut().for_each(|arg| {
            if let Err(e) = arg.resolve_usages(type_map, ctx) {
                ctx.error(e)
            }
        });

        // FIXME: This is ugly as sin
        if ctx.get_specialized_node(self.type_name.id()).is_none()
            && self.type_name.id() != old_name
        {
            let demangled = generics::demangle(self.type_name.id());

            // FIXME: Can we unwrap here? Probably not
            let generic_dec = ctx.get_custom_type(demangled).unwrap().clone();
            let specialized_ty = generic_dec.generate(new_name, type_map, ctx)?;
            ctx.add_specialized_node(SpecializedNode::Type(specialized_ty))
        } else {
            // FIXME: Is that the expected behavior?
            Ok(())
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{jinko_fail, symbol::Symbol};

    #[test]
    fn t_fields_number() {
        use super::super::DecArg;
        use crate::typechecker::TypeId;
        use crate::value::JkInt;

        let mut ctx = Context::new();

        // Create a new type with two integers fields
        let fields = vec![
            DecArg::new("a".to_owned(), TypeId::from("int")),
            DecArg::new("b".to_owned(), TypeId::from("int")),
        ];
        let t = TypeDec::new("Type_Test".to_owned(), vec![], fields);

        t.execute(&mut ctx);

        let mut t_inst = TypeInstantiation::new(TypeId::from("Type_Test"));

        assert!(t_inst.execute(&mut ctx).is_none());
        assert!(
            ctx.error_handler.has_errors(),
            "Given 0 field to 2 fields type"
        );
        ctx.clear_errors();

        t_inst.add_field(VarAssign::new(
            false,
            "a".to_string(),
            Box::new(JkInt::from(12)),
        ));

        assert!(t_inst.execute(&mut ctx).is_none());
        assert!(
            ctx.error_handler.has_errors(),
            "Given 1 field to 2 fields type"
        );
        ctx.clear_errors();

        t_inst.add_field(VarAssign::new(
            false,
            "b".to_string(),
            Box::new(JkInt::from(8)),
        ));

        assert!(
            t_inst.execute(&mut ctx).is_some(),
            "Type instantiation should have a correct number of fields now"
        );
        assert!(!ctx.error_handler.has_errors());
    }

    #[test]
    fn t_returned_instance() {
        use super::super::DecArg;
        use crate::typechecker::TypeId;
        use crate::value::{JkInt, JkString};

        const TYPE_NAME: &str = "Type_Name";

        let mut ctx = Context::new();

        // Create a new type with two integers fields
        let fields = vec![
            DecArg::new("a".to_owned(), TypeId::from("string")),
            DecArg::new("b".to_owned(), TypeId::from("int")),
        ];
        let t = TypeDec::new(TYPE_NAME.to_owned(), vec![], fields);

        t.execute(&mut ctx);

        let mut t_inst = TypeInstantiation::new(TypeId::new(Symbol::from(TYPE_NAME.to_string())));
        t_inst.add_field(VarAssign::new(
            false,
            "a".to_string(),
            Box::new(JkString::from("I am a loooooooong string")),
        ));
        t_inst.add_field(VarAssign::new(
            false,
            "b".to_string(),
            Box::new(JkInt::from(12)),
        ));

        let instance = match t_inst.execute(&mut ctx) {
            Some(instance) => instance,
            None => {
                unreachable!("Type instantiation should have returned an Expression")
            }
        };

        assert_eq!(
            instance.data()[..],
            [
                73, 32, 97, 109, 32, 97, 32, 108, 111, 111, 111, 111, 111, 111, 111, 111, 110, 103,
                32, 115, 116, 114, 105, 110, 103, 12, 0, 0, 0, 0, 0, 0, 0
            ]
        );
        assert_eq!(instance.size(), 33);

        // FIXME: Is this a valid test?
        assert_eq!(
            instance
                .fields()
                .as_ref()
                .unwrap()
                .get("a")
                .unwrap()
                .offset(),
            &0usize
        );
        assert_eq!(
            instance
                .fields()
                .as_ref()
                .unwrap()
                .get("b")
                .unwrap()
                .offset(),
            &25usize
        );
    }

    #[test]
    fn t_instantiate_primitive() {
        jinko_fail! {
            i = int(no_fields: 15);
        };
    }
}
