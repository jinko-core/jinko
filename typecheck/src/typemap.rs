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
#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct TypeRef(pub(crate) OriginIdx); // FIXME: Remove vis?

#[derive(Debug)]
pub struct TypeMap {
    nodes: HashMap<OriginIdx, TypeRef>,
    // FIXME: Remove?
    pub(crate) types: HashMap<
        TypeRef,
        Type, /* FIXME: We should store the OriginIdx here as well probably */
    >,
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
            // in case that we are directly looking at the type's definition and not a reference.
            // this happens when dealing with inline type variables, such as union-types defined
            // in a function's return type.
            .or_else(|| self.types.get(&TypeRef(node)))
    }

    /// Insert a new type into the typemap
    pub fn new_type(&mut self, ty: Type) -> TypeRef {
        let ty_ref = TypeRef(ty.0);
        // FIXME: Is it actually okay to ignore if the type existed or not?
        self.types.insert(ty_ref, ty);

        ty_ref
    }

    pub fn insert(&mut self, node: OriginIdx, tyref: TypeRef) {
        // FIXME:
        self.nodes.insert(node, tyref);
    }
}
