use crate::{
    instruction::{TypeId, TypeInstantiation, Var},
    CheckedType, Context, Generic, InstrKind, Instruction, ObjectInstance, TypeCheck, TypeCtx,
};

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
}

impl VarOrEmptyType {
    pub fn new(symbol: String) -> VarOrEmptyType {
        VarOrEmptyType {
            kind: Kind::Unknown,
            symbol,
            cached_type: None,
        }
    }

    fn resolve_kind(&self, ctx: &mut TypeCtx) -> Kind {
        let resolved = ctx.get_custom_type(&self.symbol);
        if resolved.is_some() {
            return Kind::EmptyTypeInst;
        }

        let resolved = ctx.get_var(&self.symbol);
        if resolved.is_some() {
            return Kind::VarAccess;
        }

        Kind::Unknown
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
        let symbol_type_id = TypeId::new(self.symbol.clone());
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
            Kind::EmptyTypeInst => CheckedType::Resolved(TypeId::new(self.symbol.clone())),
            Kind::VarAccess => ctx.get_var(&self.symbol).unwrap().to_owned(),
        }
    }

    fn set_cached_type(&mut self, ty: CheckedType) {
        match ty {
            CheckedType::Void => self.kind = Kind::VarAccess,
            CheckedType::Resolved(_) => self.kind = Kind::EmptyTypeInst,
            // FIXME: Is Later truly an error here?
            CheckedType::Later | CheckedType::Error => self.kind = Kind::Unknown,
        }
        self.cached_type = Some(ty);
    }

    fn cached_type(&self) -> Option<&CheckedType> {
        self.cached_type.as_ref()
    }
}

impl Generic for VarOrEmptyType {}
