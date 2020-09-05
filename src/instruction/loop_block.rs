//! The Loop instruction is used for repeating instructions. They can be of three
//! different kinds, `for`, `while` or `loop`.

/// What kind of loop the loop block represents: Either a for Loop, with a lower and
/// upper bound, a while loop with just an upper bound, or a loop with no bound
/// at all
enum LoopKind {
    For,
    While,
    Loop,
}

/// The Loop block struct. Contains the block to execute, as well as the kind of loop
/// it represents.
pub struct Loop {
    kind: LoopKind,
    block: Block,
}
