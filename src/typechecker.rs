//! The [`TypeCheck`] trait enables a value to get resolved to a type. If the value will
//! need to get its type checked multiple times, then it can implement the [`CachedTypeCheck`]
//! trait on top of it.

mod type_id;
pub use type_id::{TypeId, PRIMITIVE_TYPES};

use crate::context::ScopeMap;
use crate::error::{ErrKind, Error, ErrorHandler};
use crate::instruction::{FunctionDec, Instruction, TypeDec};
use crate::io_trait::JkReader;

use colored::Colorize;

use std::{
    collections::HashSet,
    fmt::{Display, Formatter, Result as FmtResult},
    path::{Path, PathBuf},
};

/// The [`CheckedType`] enum contains three possible states about the type. Either the
/// type has been properly resolved to something, or it corresponds to a Void type. If the
/// type has not been resolved yet, it can be unknown.
#[derive(Clone, PartialEq, Debug)]
pub enum CheckedType {
    Resolved(TypeId),
    // Should we remove this for Resolved(TypeId::void())?
    Void,
    Error,
    Later,
}

impl Default for CheckedType {
    fn default() -> CheckedType {
        CheckedType::Error
    }
}

impl Display for CheckedType {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        match self {
            CheckedType::Resolved(ty) => write!(f, "{}", ty),
            CheckedType::Void => write!(f, "{}", "void".purple()),
            CheckedType::Error => write!(f, "{}", "!!unknown!!".red()),
            // This should never happen
            CheckedType::Later => write!(f, "{}", "!!unresolved!!".red().underline()),
        }
    }
}

/// Possible generic generated nodes. Since we can only expand generic functions or
/// generic types, there is no need to store any other instruction type.
pub enum SpecializedNode {
    Func(Box<FunctionDec>),
    Type(TypeDec),
}

// TODO: Should we factor this into a `Context` trait? All contexts will share some
// similarities, such as the ability to emit errors or enter and exit scopes
/// The [`TypeCtx`]'s role is to keep track of declared types based on the scope they
/// were created in. In doing so, it allows the [`Instruction`]s to perform a type lookup
/// in order to resolve to a concrete type.Each declaration (First [`VarAssign`],
/// [`FunctionDec`]s and [`TypeDec`]s) can also declare a new type and make it available
/// to all instructions in the avaialble scopes.
pub struct TypeCtx {
    /// Reference to the context's error handler
    pub(crate) error_handler: ErrorHandler,
    /// The Type Context stores [`CheckedType`]s for all three generics kept in the scope
    /// map: Variables, Functions and Types.
    /// For functions, we keep a vector of argument types as well as the return type.
    /// Custom types need to keep a type for themselves, as well as types for all their fields
    types: ScopeMap<CheckedType, FunctionDec, TypeDec>,
    /// When typechecking, monomorphization is performed, meaning that generic functions
    /// and types get expanded into a new [`Instruction`]. We need to store them
    /// as we go and then use them in the calling context
    // FIXME: Turn this into a hashset?
    generated: Vec<SpecializedNode>,
    // FIXME: Remove both of these fields...
    /// Path from which the typechecking context was instantiated
    path: Option<PathBuf>,
    included: HashSet<PathBuf>,

    reader: Box<dyn JkReader>,
}

impl TypeCtx {
    /// Create a new empty [`TypeCtx`]
    pub fn new(reader: Box<dyn JkReader>) -> TypeCtx {
        // FIXME: This doesn't contain builtins
        let mut ctx = TypeCtx {
            error_handler: ErrorHandler::default(),
            types: ScopeMap::new(),
            generated: vec![],
            path: None,
            included: HashSet::new(),
            reader,
        };

        macro_rules! declare_primitive {
            ($ty_name:ident) => {
                ctx.declare_custom_type(
                    String::from(stringify!($ty_name)),
                    TypeDec::new(stringify!($ty_name).to_string(), vec![], vec![]),
                )
                .unwrap();
            };
        }

        ctx.scope_enter();

        declare_primitive!(bool);
        declare_primitive!(int);
        declare_primitive!(float);
        declare_primitive!(char);
        declare_primitive!(string);

        ctx
    }

    // FIXME: Remove these three functions
    pub fn set_path(&mut self, path: Option<PathBuf>) {
        self.path = path
    }

    pub fn path(&self) -> Option<&PathBuf> {
        self.path.as_ref()
    }

    pub fn include(&mut self, path: PathBuf) {
        self.included.insert(path);
    }

    pub fn is_included(&self, path: &Path) -> bool {
        self.included.contains(path)
    }

    pub fn reader(&self) -> &dyn JkReader {
        self.reader.as_ref()
    }

    /// Enter a new scope. This is the same as lexical scopes
    pub fn scope_enter(&mut self) {
        self.types.scope_enter()
    }

    /// Exit a previously created scope. This is the same as lexical scopes
    pub fn scope_exit(&mut self) {
        self.types.scope_exit()
    }

    /// Declare a newly-created variable's type
    pub fn declare_var(&mut self, name: String, ty: CheckedType) -> Result<(), Error> {
        // FIXME: Add hint here too
        self.types.add_variable(name, ty)
    }

    /// Declare a newly-created function
    pub fn declare_function(&mut self, name: String, function: FunctionDec) -> Result<(), Error> {
        // FIXME: Remove clones here
        let loc = function.location().cloned();
        match self.types.add_function(name.clone(), function) {
            Ok(_) => Ok(()),
            Err(err) => {
                let previous_dec = self.types.get_function(&name).unwrap();
                Err(err.with_loc(loc).with_hint(
                    Error::new(ErrKind::Hint)
                        .with_msg(String::from("previous declaration here"))
                        .with_loc(previous_dec.loc()),
                ))
            }
        }
    }

    /// Declare a newly-created generic function
    pub fn declare_generic_function(
        &mut self,
        name: String,
        function: FunctionDec,
    ) -> Result<(), Error> {
        // FIXME: Remove clones here
        let loc = function.location().cloned();
        match self.types.add_function(name.clone(), function) {
            Ok(_) => Ok(()),
            Err(err) => {
                let previous_dec = self.types.get_generic_function(&name).unwrap();
                Err(err.with_loc(loc).with_hint(
                    Error::new(ErrKind::Hint)
                        .with_msg(String::from("previous generic declaration here"))
                        .with_loc(previous_dec.loc()),
                ))
            }
        }
    }

    /// Add a new generated node to the context
    pub fn add_specialized_node(&mut self, mut node: SpecializedNode) {
        match &mut node {
            SpecializedNode::Func(f) => f.type_of(self),
            SpecializedNode::Type(t) => t.type_of(self),
        };

        self.generated.push(node)
    }

    /// Get a reference to a newly generated node
    pub fn get_specialized_node(&mut self, name: &str) -> Option<&SpecializedNode> {
        self.generated.iter().find(|node| {
            let node_name = match node {
                SpecializedNode::Func(f) => f.name(),
                SpecializedNode::Type(t) => t.name(),
            };

            node_name == name
        })
    }

    /// Take ownership of the specialized nodes currently present in the
    /// type context. This replaces the vector of generated nodes with an
    /// empty vector
    pub fn take_specialized_nodes(&mut self) -> Vec<SpecializedNode> {
        std::mem::take(&mut self.generated)
    }

    /// Declare a newly-created custom type
    pub fn declare_custom_type(&mut self, name: String, dec: TypeDec) -> Result<(), Error> {
        self.types.add_type(name, dec)
        // FIXME: Add hint here too
    }

    /// Declare a newly-created generic custom type
    pub fn declare_generic_custom_type(&mut self, name: String, dec: TypeDec) -> Result<(), Error> {
        self.types.add_generic_type(name, dec)
        // FIXME: Add hint here too
    }

    /// Access a previously declared variable's type
    pub fn get_var(&mut self, name: &str) -> Option<&CheckedType> {
        self.types.get_variable(name)
    }

    /// Access a previously declared function
    pub fn get_function(&mut self, name: &str) -> Option<&FunctionDec> {
        self.types.get_function(name)
    }

    /// Access a previously declared generic function
    pub fn get_generic_function(&mut self, name: &str) -> Option<&FunctionDec> {
        self.types.get_generic_function(name)
    }

    /// Access a previously declared custom type
    pub fn get_custom_type(&mut self, name: &str) -> Option<&TypeDec> {
        self.types.get_type(name)
    }

    /// Access a previously declared generic custom type
    pub fn get_generic_custom_type(&mut self, name: &str) -> Option<&TypeDec> {
        self.types.get_generic_type(name)
    }

    /// Create a new error to propagate to the original context
    pub fn error(&mut self, err: Error) {
        self.error_handler.add(err)
    }
}

/// The [`TypeCheck`] trait allows an [`Instruction`] to see its type resolved statically.
/// There are three possible return values:
///     - Resolve(type): This means that the type of the [`Instruction`] was abled to
///     get resolved statically
///     - Void: The [`Instruction`] is not of any type. It corresponds to a statement.
///     - Later: The [`Instruction`] cannot be typechecked at the time. This can happen
///       due to a function/type declaration being past the call/instantiation site, or in
///       the case of generics when monomorphized functions might not have been generated
///       yet.
///     - Error: This means that even after the typechecking pass, the instruction's
///       type is still unclear.
/// Every [`Instruction`] should keep a cached copy of its own type. This is important
/// for later passes of the typechecker or generic expansion. To do so, a special field
/// of the type `Option<CheckedType>` should be kept, and originally initialized to `None`
pub trait TypeCheck {
    /// Go through the context in order to figure out the type of an instruction.
    /// This function should report errors using the context, and the [`ErrKind::TypeCheck`]
    /// error kind.
    fn resolve_type(&mut self, ctx: &mut TypeCtx) -> CheckedType;

    /// Cache the type of an instruction
    fn set_cached_type(&mut self, ty: CheckedType);

    /// Access the cached type of an instruction
    fn cached_type(&self) -> Option<&CheckedType>;

    /// Access the cached type of an instruction or perform the type resolution process.
    /// This avoid typechecking an entire instruction a second time and allows the
    /// context to just access it. This is useful for passes such as generic expansion.
    fn type_of(&mut self, ctx: &mut TypeCtx) -> CheckedType {
        // FIXME: Remove clones
        match self.cached_type() {
            None => match self.resolve_type(ctx) {
                CheckedType::Resolved(new_ty) => {
                    self.set_cached_type(CheckedType::Resolved(new_ty.clone()));
                    CheckedType::Resolved(new_ty)
                }
                CheckedType::Void => {
                    self.set_cached_type(CheckedType::Void);
                    CheckedType::Void
                }
                CheckedType::Error => CheckedType::Error,
                CheckedType::Later => CheckedType::Later,
            },
            Some(ty) => ty.clone(),
        }
    }
}
