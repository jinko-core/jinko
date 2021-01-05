/// In Jinko, an Instance represents an actual value in memory.
/// An Instance has a type, a size, and owns a memory region. In the case of an integer,
/// it would for example be of the primitive type `int`, be of size 8 and contain 8 bytes
/// of "raw" data. Because Jinko is strongly typed, this isn't an issue. An integer will
/// always be an integer.
/// For example, a variable contains an Instance. Since a variable cannot be uninitialized,
/// the instance is always there. The type of the Instance might be resolved later, after
/// different passes of the typechecker.

// FIXME: Use CustomType once @Skallwar's PR is merged
type Ty = String;

pub struct Instance {
    ty: Ty,
    size: usize,
    data: Vec<u8>,
}
