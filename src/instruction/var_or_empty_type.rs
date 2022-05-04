use crate::context::Context;
use crate::generics::GenericUser;
use crate::instance::ObjectInstance;
use crate::instruction::{InstrKind, Instruction, TypeInstantiation, Var};
use crate::location::SpanTuple;
use crate::symbol::Symbol;
use crate::typechecker::{CheckedType, TypeCheck, TypeCtx, TypeId};

#[derive(Clone, PartialEq)]
enum Kind {
    Unknown,
    EmptyTypeInst,
    VarAccess,
}

#[derive(Clone)]
pub struct VarOrEmptyType {
    kind: Kind,
    symbol: String,
    // FIXME: We can probably avoid keeping a `cached_type` and a `kind`. Only one
    // is enough. Refactor later
    cached_type: Option<CheckedType>,
    location: Option<SpanTuple>,
}

impl VarOrEmptyType {
    pub fn new(symbol: String) -> VarOrEmptyType {
        VarOrEmptyType {
            kind: Kind::Unknown,
            symbol,
            cached_type: None,
            location: None,
        }
    }

    fn resolve_kind(&self, ctx: &mut TypeCtx) -> Kind {
        let resolved = ctx.get_custom_type(&TypeId::new(Symbol::from(self.symbol.as_str())));
        if resolved.is_some() {
            return Kind::EmptyTypeInst;
        }

        let resolved = ctx.get_var(&self.symbol);
        if resolved.is_some() {
            return Kind::VarAccess;
        }

        Kind::Unknown
    }

    pub fn set_location(&mut self, location: SpanTuple) {
        self.location = Some(location)
    }
}

impl Instruction for VarOrEmptyType {
    fn kind(&self) -> InstrKind {
        InstrKind::Expression(None)
    }

    fn print(&self) -> String {
        self.symbol.clone()
    }

    fn execute(&self, ctx: &mut Context) -> Option<ObjectInstance> {
        let symbol_type_id = TypeId::new(Symbol::from(self.symbol.clone()));
        match ctx.get_type(&symbol_type_id) {
            Some(_) => {
                let ty_inst = TypeInstantiation::new(symbol_type_id);
                ty_inst.execute(ctx)
            }
            None => {
                let var_inst = Var::new(self.symbol.clone());
                var_inst.execute(ctx)
            }
        }
    }

    fn location(&self) -> Option<&SpanTuple> {
        self.location.as_ref()
    }
}

impl TypeCheck for VarOrEmptyType {
    fn resolve_type(&mut self, ctx: &mut TypeCtx) -> CheckedType {
        let kind = if self.kind == Kind::Unknown {
            self.resolve_kind(ctx)
        } else {
            self.kind.clone()
        };

        match kind {
            Kind::Unknown => CheckedType::Error,
            Kind::EmptyTypeInst => {
                CheckedType::Resolved(TypeId::new(Symbol::from(self.symbol.clone())))
            }
            Kind::VarAccess => ctx.get_var(&self.symbol).unwrap().to_owned(),
        }
    }

    fn set_cached_type(&mut self, ty: CheckedType) {
        match ty {
            CheckedType::Void => self.kind = Kind::VarAccess,
            CheckedType::Resolved(_) => self.kind = Kind::EmptyTypeInst,
            CheckedType::Error | CheckedType::Later => self.kind = Kind::Unknown,
        }
        self.cached_type = Some(ty);
    }

    fn cached_type(&self) -> Option<&CheckedType> {
        self.cached_type.as_ref()
    }
}

impl GenericUser for VarOrEmptyType {}
