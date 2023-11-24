use std::collections::HashMap;

use fir::OriginIdx;

use crate::Type;

// FIXME: Fix documentation
// typer is going to create a linked list of types
// actual is going to flatten that linked list into a "multikey-map", TypeMap

// so the role of Actual is to take a Map<OriginIdx, TypeRef>
// and turn it into a TypeMap

// so, typer is Fir -> Map<OriginIdx, TypeRef>
// actual is Map<OriginIdx, TypeRef> -> TypeMap,
// checker is TypeMap -> Result<Fir, Error>

// do we do something like TypeCtx<LinkedTypeMap> ? and then TypeCtx<TypeMap>
// and Actual is TypeCtx<LinkedTypeMap> -> TypeCtx<TypeMap>?

/// A strongly typed reference to a type node - `Kind::UnionType` or `Kind::RecordType` only
#[derive(PartialEq, Eq, Hash, Clone, Copy)]
pub struct TypeRef(OriginIdx);

// FIXME: These two are not needed?
// FIXME: Should we use phantom types here?
pub struct UnionType(OriginIdx);
pub struct RecordType(OriginIdx);

impl TypeRef {
    pub fn union(UnionType(node): UnionType) -> TypeRef {
        TypeRef(node)
    }

    pub fn record(RecordType(node): RecordType) -> TypeRef {
        TypeRef(node)
    }
}

pub struct TypeMap {
    nodes: HashMap<OriginIdx, TypeRef>,
    types: HashMap<TypeRef, Type>,
}

impl TypeMap {
    pub fn new() -> TypeMap {
        TypeMap {
            nodes: HashMap::new(),
            types: HashMap::new(),
        }
    }

    pub fn type_of(&self, node: OriginIdx) -> Option<&Type> {
        self.nodes
            .get(&node)
            .and_then(|ty_ref| self.types.get(ty_ref))
    }

    /// Insert a new type into the typemap
    pub fn new_type(&mut self, origin: OriginIdx, ty: Type) -> TypeRef {
        let ty_ref = TypeRef(origin);
        // FIXME:
        self.types.insert(ty_ref, ty).unwrap();

        ty_ref
    }

    pub fn insert(&mut self, node: OriginIdx, tyref: TypeRef) {
        // FIXME:
        self.nodes.insert(node, tyref).unwrap();
    }
}
