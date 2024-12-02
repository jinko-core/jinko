//! The goal of the flatten crate is to produce a flat representation of a given syntax tree.
//! This means going from an [`Ast`] to an [`Fir`]. The goal of this crate is not to name-resolve:
//! Instead, this crate will produce an [`Fir`] containing enough information to perform name
//! resolution strictly on an instance of [`Fir`].
//!
//! Let's take the following, invalid jinko program:
//! ```text
//! func f() -> int {
//!     return 15;
//! }
//!
//! f();
//! f(15);
//! ```
//!
//! This is the corresponding simplified [`Ast`] representation:
//!
//! ```text
//! Ast {
//!     Block(
//!         [
//!             Function {
//!                 kind: Func,
//!                 decl: Declaration {
//!                     name: "f",
//!                     generics: [],
//!                     args: [],
//!                     return_type: Some(
//!                         TypeArgument {
//!                             generics: [],
//!                             kind: "int",
//!                         },
//!                     ),
//!                 },
//!                 block: Some(
//!                     Block( [ Return(Constant(Integer(15)) ] ),
//!                 ),
//!             },
//!             FunctionCall(
//!                 to: "f",
//!                 generics: [],
//!                 args: [],
//!             ),
//!             FunctionCall(
//!                 to: "f",
//!                 generics: [],
//!                 args: [ Constant(Integer(15)) ],
//!             ),
//!         ],
//!     ),
//! }
//! ```
//!
//! We want to turn this into an instance of [`Fir`], so a *flat* representation of the above
//! [`Ast`]. Let's go through it step by step:
//!
//! First, we visit a Block. We go through all of its statements (visit them) and then we will add the
//! block to the [`Fir`]. We do this since a [`Kind::Statements`] only contains *references* to the statements
//! it contains, meaning the statements have to be defined within the [`Fir`] before we define the block.
//!
//! Programmatically, this means something along the lines of `for stmt in block { fir = visit(fir, stmt) }; fir.append(block.to_node())`
//!
//! The first statement of that block is a function declaration. We have to
//! visit all of its generics, append them to the [`Fir`]. then visit all of the
//! arguments, append them to the [`Fir`], then the return type, and finally visit
//! the functions's block. Let's go through that. Note that at this point, our [`Fir`] is still empty!
//!
//! Our function has no generics, and no arguments. This is easy. However, it has a return type. This return type must be a reference to an
//! existing type (which we'll resolve in a later pass - here, we are not interested in whether or not this type is valid). Let's create that reference add it to the [`Fir`].
//!
//! ```text
//! Fir [
//!     (Origin::0, TypeUsage(RefIdx::Unresolved),
//! ]
//! ```
//!
//! If we want to create our function in the [`Fir`], we need references to the
//! arguments (there's zero of them), generics (likewise), return type (we have
//! that one, it's `0`), and block, which is not currently present.
//!
//! Let's visit it. Similarly to the original block, it will only contain
//! references to its statements. Let's add each of them. The first one is a return statement, which, similarly,
//! can only store a reference to an existing value, which we visit.
//!
//! We're now on a "leaf node": the constant value `15`. This node does not contain any references and simply... exists.
//! We create it and append it to the [`Fir`].
//!
//! ```text
//! Fir [
//!     (Origin::0, TypeUsage(RefIdx::Unresolved)),
//!     (Origin::1, Constant(RefIdx::Unresolved)), // we don't know the type of `15` yet
//! ]
//! ```
//!
//! We can now create the return statement.
//!
//! ```text
//! Fir [
//!     (Origin::0, TypeUsage(RefIdx::Unresolved)),
//!     (Origin::1, Constant(RefIdx::Unresolved)), // we don't know the type of `15` yet
//!     (Origin::2, Return(RefIdx::Ref(1))), // reference to Origin::1
//! ]
//! ```
//!
//! Which lets us create the block
//!
//! ```text
//! Fir [
//!     (Origin::0, TypeUsage(RefIdx::Unresolved)),
//!     (Origin::1, Constant(RefIdx::Unresolved)), // we don't know the type of `15` yet
//!     (Origin::2, Return(RefIdx::Ref(1))), // reference to Origin::1
//!     (Origin::3, Block { stmts: [RefIdx::Ref(2)] }), // only one statement, the `return` one
//! ]
//! ```
//!
//! Which means we now have enough to create our function and add it to the [`Fir`].
//!
//! ```text
//! Fir [
//!     (Origin::0, TypeUsage(RefIdx::Unresolved)),
//!     (Origin::1, Constant(RefIdx::Unresolved)), // we don't know the type of `15` yet
//!     (Origin::2, Return(RefIdx::Ref(1))), // reference to Origin::1
//!     (Origin::3, Block { stmts: [ RefIdx::Ref(2) ] }), // only one statement, the `return` one
//!     (Origin::4, Function {
//!             args: [],
//!             generics: [],
//!             return_ty: Some(RefIdx::Ref(0)),
//!             block: Some(RefIdx::Ref(3)),
//!         }
//!     ),
//! ]
//! ```
//!
//! If we keep going and add the missing calls, we get something like the following:
//!
//! ```text
//! Fir [
//!     (Origin::0, TypeUsage(RefIdx::Unresolved)),
//!     (Origin::1, Constant(RefIdx::Unresolved)), // we don't know the type of `15` yet
//!     (Origin::2, Return(RefIdx::Ref(1))), // reference to Origin::1
//!     (Origin::3, Block { stmts: [RefIdx::Ref(2)] }), // only one statement, the `return` one
//!     (Origin::4, Function {
//!             args: [],
//!             generics: [],
//!             return_ty: Some(RefIdx::Ref(0)),
//!             block: Some(RefIdx::Ref(3)),
//!         }
//!     ),
//!     (Origin::5, Call(to: RefIdx::Unresolved, args: [])),
//!     (Origin::6, Constant(RefIdx::Unresolved)), // we don't know that both `15` nodes can be factored together yet
//!     (Origin::7, Call(to: RefIdx::Unresolved, args: [ RefIdx::Ref(6) ])),
//! ]
//! ```

use ast::{
    Ast, Call, Declaration, GenericParameter, LoopKind, Node as AstNode, Type, TypeContent,
    TypeKind, TypedValue,
};
use fir::{Fir, Kind, Node, OriginIdx, RefIdx};
use location::SpanTuple;
use symbol::Symbol;

#[doc(hidden)]
trait VecExt<T> {
    fn with(self, elt: T) -> Self;
}

impl<T> VecExt<T> for Vec<T> {
    fn with(mut self, elt: T) -> Vec<T> {
        self.push(elt);

        self
    }
}

trait OriginExt {
    /// Returns the new value
    fn increment(&mut self) -> Self;
}

impl OriginExt for OriginIdx {
    fn increment(&mut self) -> OriginIdx {
        self.0 += 1;

        *self
    }
}

#[derive(Clone, Debug)]
pub enum AstInfo<'ast> {
    Node(&'ast Ast),
    Type(&'ast Type),
    Helper(Symbol, SpanTuple),
}

impl AstInfo<'_> {
    pub fn location(&self) -> &SpanTuple {
        match self {
            AstInfo::Node(Ast { location, .. })
            | AstInfo::Type(Type { location, .. })
            | AstInfo::Helper(_, location) => location,
        }
    }

    pub fn symbol(&self) -> Option<&Symbol> {
        match self {
            AstInfo::Node(Ast { node, .. }) => match node {
                AstNode::Block { .. }
                | AstNode::Incl { .. }
                | AstNode::BinaryOp(_, _, _)
                | AstNode::FieldAccess(_, _)
                | AstNode::Loop(..)
                | AstNode::Return(..)
                // FIXME: Is that valid?
                | AstNode::Constant(..)
                | AstNode::IfElse { .. }
                | AstNode::Empty => None,
                AstNode::Function { decl: Declaration { name: sym, .. }, .. }
                | AstNode::Type { name: sym, .. }
                | AstNode::FunctionCall(Call { to: sym, .. })
                | AstNode::TypeInstantiation(Call { to: sym, .. })
                | AstNode::MethodCall {
                    instance: _,
                    call: Call { to: sym, .. },
                    ..
                }
                | AstNode::VarOrEmptyType(sym)
                | AstNode::Assignment { to_assign: sym, .. }
                | AstNode::VarDeclaration {
                    to_declare: sym, ..
                } => Some(sym),
            },
            AstInfo::Type(Type {
                kind: TypeKind::Simple(sym),
                ..
            }) => Some(sym),
            // FIXME: is that valid?
            AstInfo::Type(_) => None,
            AstInfo::Helper(symbol, _) => Some(symbol),
        }
    }

    /// Fetch the [`AstInfo::Node`] from an [`AstInfo`]. This function will panic if the [`AstInfo`] is *not* an [`AstInfo::Node`]
    pub fn node(&self) -> &Ast {
        match self {
            AstInfo::Node(node) => node,
            _ => unreachable!("asked for AST node from a non `AstInfo::Node`"),
        }
    }

    /// Fetch the [`AstInfo::Node`] from an [`AstInfo`]. This function will panic if the [`AstInfo`] is *not* an [`AstInfo::Type`]
    pub fn type_node(&self) -> &Type {
        match self {
            AstInfo::Type(ty) => ty,
            _ => unreachable!("asked for type node from a non `AstInfo::Type`"),
        }
    }

    /// Fetch the [`AstInfo::Helper`] from an [`AstInfo`]. This function will panic if the [`AstInfo`] is *not* an [`AstInfo::Helper`]
    pub fn helper(&self) -> (&Symbol, &SpanTuple) {
        match self {
            AstInfo::Helper(sym, loc) => (sym, loc),
            _ => unreachable!("asked for helper from a non `AstInfo::Helper`"),
        }
    }
}

#[derive(Clone, Debug)]
pub struct FlattenData<'ast> {
    pub ast: AstInfo<'ast>,
    pub scope: usize,
}

struct Ctx<'ast> {
    pub fir: Fir<FlattenData<'ast>>,
    pub origin: OriginIdx,
    pub scope: usize,
}

impl<'ast> Ctx<'ast> {
    fn append(mut self, data: FlattenData<'ast>, kind: Kind) -> (Ctx<'ast>, RefIdx) {
        let next = self.origin.increment();
        let node = Node {
            data,
            origin: next,
            kind,
        };

        (
            Ctx {
                fir: self.fir.append(node),
                ..self
            },
            RefIdx::Resolved(next),
        )
    }

    fn handle_generic_node(self, generic: &GenericParameter) -> (Ctx<'ast>, RefIdx) {
        // FIXME: Needs AST to be fixed
        // let (ctx, default_ty) = self.handle_ty_node(generic.ty);

        let data = FlattenData {
            ast: AstInfo::Helper(
                generic.name.clone(),
                SpanTuple::with_source(
                    // FIXME: The location here is wrong. We need location from the AST but it doesn't
                    // keep it yet for [`GenericArgument`]
                    location::SourceOwned::Empty,
                    location::Location::new(1, 1),
                    location::Location::new(1, 1),
                ),
            ),
            scope: self.scope,
        };
        let kind = Kind::Generic { default: None }; // FIXME: Invalid

        self.append(data, kind)
    }

    fn handle_type_node(self, ty: &'ast Type) -> (Ctx<'ast>, RefIdx) {
        let (ctx, generics) = self.visit_fold(ty.generics.iter(), Ctx::handle_type_node);

        // so in some cases this should be a type reference, and in other cases this should be a type definition which contains multiple type references - e.g
        // a: int // type reference
        // a: int | string // type declaration, which gets flattened as a union type with two type references: int and string

        // FIXME: This should be a type reference probably, not a `Type`. Or `visit_function` uses `handle_ty_node` when
        // it shouldn't for its return type
        match &ty.kind {
            // FIXME: Do we need to create a type reference here as well? `handle_multi_ty` returns the actual union type
            TypeKind::Multi(variants) => ctx.handle_multi_type(ty, generics, variants),
            TypeKind::Literal(ast) => {
                let (ctx, idx) = ctx.visit(ast);

                let data = FlattenData {
                    ast: AstInfo::Node(ast),
                    scope: ctx.scope,
                };

                ctx.append(data, Kind::TypeReference(idx))
            }
            TypeKind::Simple(_) => {
                let data = FlattenData {
                    ast: AstInfo::Type(ty),
                    //     match &ty.kind {
                    //         // TODO: how do we handle multi types properly here?
                    //         TypeKind::Simple(s) => s.clone(),
                    //         TypeKind::Multi(_) => Symbol::from("multi"),
                    //         TypeKind::FunctionLike(_, _) => Symbol::from("func"), // FIXME: Invalid but w/ever for now
                    //     },
                    //     ty.location.clone(),
                    // ),
                    scope: ctx.scope,
                };

                // FIXME: where do we put the generics?
                // In the flatten data? Basically we're flattening all of the generics of the
                // type and putting them... nowhere? Should `TypeReference` keep a `generics: Vec<RefIdx>` or no?
                // or is the goal to instantiate a proper monomorphized type down the line and have
                // typereference point to it?
                //  {
                //     generics,
                //     fields: vec![],
                // };

                // TODO: Should this have a generics field then? probably, right?
                // or... if we have generics, are we creating a new type instead?
                ctx.append(data, Kind::TypeReference(RefIdx::Unresolved))
            }
            TypeKind::FunctionLike(_, _) => {
                todo!("function like types cannot be flattened yet. this is an interpreter error.")
            }
        }
    }

    fn handle_declaration_argument(self, arg: &'ast TypedValue) -> (Ctx<'ast>, RefIdx) {
        let (ctx, ty) = self.handle_type_node(&arg.ty);

        let data = FlattenData {
            ast: AstInfo::Helper(arg.symbol.clone(), arg.location.clone()),
            scope: ctx.scope,
        };

        let kind = Kind::Binding {
            to: None,
            ty: Some(ty),
        };

        ctx.append(data, kind)
    }

    fn scoped(
        mut self,
        ast: AstInfo<'ast>,
        f: impl Fn(Ctx<'ast>, AstInfo<'ast>) -> (Ctx<'ast>, RefIdx),
    ) -> (Ctx<'ast>, RefIdx) {
        self.scope += 1;

        let (mut ctx, idx) = f(self, ast);

        ctx.scope -= 1;

        (ctx, idx)
    }

    fn visit_binary_op(
        self,
        location: &SpanTuple,
        op: &ast::Operator,
        lhs: &'ast Ast,
        rhs: &'ast Ast,
    ) -> (Ctx<'ast>, RefIdx) {
        // This desugars `l + r` to `+(l, r)`.
        // Is it valid? We assume that later on during type resolution
        // and actual code generation/interpretation, a builtin method
        // will be added to the interpreter:
        //
        // ```
        // func +[L, R, O](lhs: L, rhs: R) -> O { /* builtin */ }
        // ````
        // FIXME: Is that valid?

        let (ctx, l_id) = self.visit(lhs);
        let (ctx, r_id) = ctx.visit(rhs);

        let data = FlattenData {
            ast: AstInfo::Helper(
                Symbol::from(match op {
                    ast::Operator::Add => builtins::name::ADD,
                    ast::Operator::Sub => builtins::name::SUB,
                    ast::Operator::Mul => builtins::name::MUL,
                    ast::Operator::Div => builtins::name::DIV,
                    ast::Operator::Lt => builtins::name::LT,
                    ast::Operator::Gt => builtins::name::GT,
                    ast::Operator::LtEq => builtins::name::LTE,
                    ast::Operator::GtEq => builtins::name::GTE,
                    ast::Operator::Equals => builtins::name::EQ,
                    ast::Operator::NotEquals => builtins::name::NE,
                }),
                location.clone(),
            ),
            // FIXME: Is that the correct scope? Add unit test
            scope: ctx.scope,
        };

        // FIXME: Should this be a call? Is this a good idea?
        let kind = Kind::Call {
            to: RefIdx::Unresolved,
            generics: vec![],
            args: vec![l_id, r_id],
        };

        ctx.append(data, kind)
    }

    fn visit_block(
        self,
        ast: AstInfo<'ast>,
        stmts: &'ast [Ast],
        last_is_expr: bool,
    ) -> (Ctx<'ast>, RefIdx) {
        self.scoped(ast, |ctx, ast| {
            let (ctx, refs) = if let Some((maybe_return, nodes)) = stmts.split_last() {
                let (ctx, refs) = ctx.visit_fold(nodes.iter(), Ctx::visit);
                let (ctx, idx) = ctx.visit(maybe_return);

                // If the block contains a last expression, transform it into a return
                let (ctx, last_idx) = if last_is_expr {
                    let data = FlattenData {
                        scope: ctx.scope,
                        // we're sure we have a last expression, so we can unwrap
                        ast: AstInfo::Node(stmts.last().unwrap()),
                    };
                    let kind = Kind::Return(Some(idx));

                    ctx.append(data, kind)
                } else {
                    (ctx, idx)
                };

                (ctx, refs.with(last_idx))
            } else {
                (ctx, vec![])
            };

            let data = FlattenData {
                scope: ctx.scope,
                ast,
            };
            let kind = Kind::Statements(refs);

            ctx.append(data, kind)
        })
    }

    fn visit_field_access(
        self,
        ast: AstInfo<'ast>,
        instance: &'ast Ast,
        _field_name: &Symbol,
    ) -> (Ctx<'ast>, RefIdx) {
        let (ctx, instance) = self.visit(instance);

        let data = FlattenData {
            // FIXME: Isn't this actually more part of typechecking than name resolution?
            scope: ctx.scope,
            ast,
        };

        let kind = Kind::TypeOffset {
            instance,
            field: RefIdx::Unresolved,
        };

        ctx.append(data, kind)
    }

    fn visit_function_call(
        self,
        ast: AstInfo<'ast>,
        Call {
            to: _,
            generics,
            args,
        }: &'ast Call,
    ) -> (Ctx<'ast>, RefIdx) {
        let (ctx, generics) = self.visit_fold(generics.iter(), Ctx::handle_type_node);
        let (ctx, args) = ctx.visit_fold(args.iter(), Ctx::visit);

        let data = FlattenData {
            scope: ctx.scope,
            ast,
        };
        let kind = Kind::Call {
            to: RefIdx::Unresolved,
            generics,
            args,
        };

        ctx.append(data, kind)
    }

    fn visit_fold<T>(
        self,
        iter: impl Iterator<Item = T>,
        visitor: impl Fn(Ctx<'ast>, T) -> (Ctx<'ast>, RefIdx),
    ) -> (Ctx<'ast>, Vec<RefIdx>) {
        iter.fold((self, vec![]), |(ctx, refs), node| {
            let (ctx, new_ref) = visitor(ctx, node);

            (ctx, refs.with(new_ref))
        })
    }

    fn visit_function(
        self,
        ast: AstInfo<'ast>,
        Declaration {
            name: _,
            generics,
            args,
            return_type,
        }: &'ast Declaration,
        block: &'ast Option<Box<Ast>>,
    ) -> (Ctx<'ast>, RefIdx) {
        self.scoped(ast, |ctx, ast| {
            let (ctx, generics) = ctx.visit_fold(generics.iter(), Ctx::handle_generic_node);
            let (ctx, args) = ctx.visit_fold(args.iter(), Ctx::handle_declaration_argument);
            let (ctx, return_type) = ctx.visit_opt(return_type.as_ref(), Ctx::handle_type_node);
            let (ctx, block) = ctx.visit_opt(block.as_deref(), Ctx::visit);

            let data = FlattenData {
                scope: ctx.scope - 1,
                ast,
                // we set the scope to the one "outside" the `scoped` method - the function
                // lives where it is defined, not in its own scope. This will never trigger
                // an underflow as scoped() indicates `ctx.scope` is at least incremented by one.
            };
            let kind = Kind::Function {
                generics,
                args,
                return_type,
                block,
            };

            ctx.append(data, kind)
        })
    }

    fn visit_if_else(
        self,
        ast: AstInfo<'ast>,
        condition: &'ast Ast,
        if_block: &'ast Ast,
        else_block: &'ast Option<Box<Ast>>,
    ) -> (Ctx<'ast>, RefIdx) {
        let (ctx, condition) = self.visit(condition);
        let (ctx, true_block) = ctx.visit(if_block);
        let (ctx, false_block) = ctx.visit_opt(else_block.as_deref(), Ctx::visit);

        let data = FlattenData {
            scope: ctx.scope,
            ast,
        };

        let kind = Kind::Conditional {
            condition,
            true_block,
            false_block,
        };

        ctx.append(data, kind)
    }

    fn visit_loop(
        self,
        ast: AstInfo<'ast>,
        kind: &'ast LoopKind,
        block: &'ast Ast,
    ) -> (Ctx<'ast>, RefIdx) {
        let (ctx, condition) = match kind {
            // At this point, only `while` loops remain: `for` loops and `loop` loops will
            // have been desugared by an earlier pass on the AST itself.
            LoopKind::While(condition) => self.visit(condition),
            // FIXME: Should this get desugared earlier?
            // would make things easier...
            LoopKind::Infinite | LoopKind::For { .. } => {
                unreachable!(
                    "AST node `Loop` has not been desugared properly before flattening: {:?}",
                    block
                )
            }
        };

        let (ctx, block) = ctx.visit(block);

        let data = FlattenData {
            scope: ctx.scope,
            ast,
        };

        let kind = Kind::Loop { condition, block };

        ctx.append(data, kind)
    }

    fn visit_method_call(
        self,
        ast: AstInfo<'ast>,
        instance: &'ast Ast,
        Call {
            to: _,
            generics,
            args,
        }: &'ast Call,
    ) -> (Ctx<'ast>, RefIdx) {
        let (ctx, idx) = self.visit(instance);
        let (ctx, generics) = ctx.visit_fold(generics.iter(), Ctx::handle_type_node);
        let (ctx, mut args) = ctx.visit_fold(args.iter(), Ctx::visit);

        args.insert(0, idx);

        let data = FlattenData {
            scope: ctx.scope,
            ast,
        };
        let kind = Kind::Call {
            to: RefIdx::Unresolved,
            generics,
            args,
        };

        ctx.append(data, kind)
    }

    fn visit_opt<T>(
        self,
        node: Option<T>,
        visitor: impl Fn(Ctx<'ast>, T) -> (Ctx<'ast>, RefIdx),
    ) -> (Ctx<'ast>, Option<RefIdx>) {
        match node {
            Some(node) => {
                let (ctx, idx) = visitor(self, node);
                (ctx, Some(idx))
            }
            None => (self, None),
        }
    }

    fn visit_return(
        self,
        ast: AstInfo<'ast>,
        to_return: &'ast Option<Box<Ast>>,
    ) -> (Ctx<'ast>, RefIdx) {
        let (ctx, idx) = self.visit_opt(to_return.as_deref(), Ctx::visit);

        let data = FlattenData {
            scope: ctx.scope,
            ast,
        };
        let kind = Kind::Return(idx);

        ctx.append(data, kind)
    }

    fn handle_multi_type(
        self,
        ty: &'ast Type,
        generics: Vec<RefIdx>,
        variants: &'ast [Type],
    ) -> (Ctx<'ast>, RefIdx) {
        let (ctx, variants) = self.visit_fold(variants.iter(), Ctx::handle_type_node);

        let data = FlattenData {
            ast: AstInfo::Type(ty),
            scope: ctx.scope,
        };

        let kind = Kind::UnionType { generics, variants };

        ctx.append(data, kind)
    }

    fn handle_type_fields(
        self,
        ast: AstInfo<'ast>,
        generics: Vec<RefIdx>,
        fields: &'ast TypeContent,
    ) -> (Ctx<'ast>, RefIdx) {
        let data = FlattenData {
            scope: self.scope,
            ast,
        };

        match fields {
            TypeContent::None => {
                self.append(data,  Kind::RecordType { generics, fields: vec![] })
            },
            TypeContent::Record(fields) => {
                let (ctx, fields) =
                    self.visit_fold(fields.iter(), Ctx::handle_declaration_argument);

                ctx.append(data, Kind::RecordType { generics, fields })
            },
            // FIXME: Surely there is something we need to do with generics here and in the following variants, right?
            // TODO: Add Kind::TypeAlias?
            TypeContent::Alias(ty) => {
                let (ctx, aliased) = self.handle_type_node(ty);

                ctx.append(data, Kind::TypeReference(aliased))
            }
            TypeContent::Tuple(_) => todo!("tuple fields are not handled yet: map to a RecordType with fields named `.0`, `.1`..."),
            // FIXME: function-like types are not handled yet
        }
    }

    fn visit_type(
        self,
        ast: AstInfo<'ast>,
        generics: &[GenericParameter],
        fields: &'ast TypeContent,
        _with: &Option<Box<Ast>>,
    ) -> (Ctx<'ast>, RefIdx) {
        let (ctx, generics) = self.visit_fold(generics.iter(), Ctx::handle_generic_node);
        ctx.handle_type_fields(ast, generics, fields)
    }

    fn visit_var_declaration(
        self,
        ast: AstInfo<'ast>,
        _to_declare: &Symbol,
        value: &'ast Ast,
    ) -> (Ctx<'ast>, RefIdx) {
        // A declaration consists of two things
        // 1. Instantiating the value which we'll assign
        // 2. Creating the first reference to that value - "binding" it

        let (ctx, to_bind) = self.visit(value);

        let data = FlattenData {
            scope: ctx.scope,
            ast,
        };

        // FIXME: If there is a given type, we need to handle it here

        let kind = Kind::Binding {
            to: Some(to_bind),
            ty: None,
        };

        ctx.append(data, kind)
    }

    fn visit_var_assign(
        self,
        ast: AstInfo<'ast>,
        _to_assign: &Symbol,
        value: &'ast Ast,
    ) -> (Ctx<'ast>, RefIdx) {
        // visit a variable or an empty type, but it's not possible to have an
        // empty type here.
        // A clone here is cheap as it will simply clone the reference
        let (ctx, to) = self.visit_var_or_empty_type(ast.clone());
        let (ctx, from) = ctx.visit(value);

        let data = FlattenData {
            scope: ctx.scope,
            ast,
        };

        let kind = Kind::Assignment { to, from };

        ctx.append(data, kind)
    }

    fn visit_var_or_empty_type(self, ast: AstInfo<'ast>) -> (Ctx<'ast>, RefIdx) {
        let data = FlattenData {
            scope: self.scope,
            ast,
        };

        let kind = Kind::NodeRef(RefIdx::Unresolved);

        self.append(data, kind)
    }

    fn handle_field_instantiation(self, instantiation: &'ast Ast) -> (Ctx<'ast>, RefIdx) {
        let AstNode::Assignment { value, .. } = &instantiation.node else {
            // FIXME: Ugly?
            unreachable!(
                "invalid AST: non var-assign in field instantiation, in type instantiation"
            )
        };

        let (ctx, value) = self.visit(value);

        let data = FlattenData {
            scope: ctx.scope,
            ast: AstInfo::Node(instantiation),
        };
        let kind = Kind::Assignment {
            to: RefIdx::Unresolved,
            from: value,
        };

        ctx.append(data, kind)
    }

    fn visit_type_instantiation(
        self,
        ast: AstInfo<'ast>,
        Call {
            to: _,
            generics,
            args: fields,
        }: &'ast Call,
    ) -> (Ctx<'ast>, RefIdx) {
        let (ctx, generics) = self.visit_fold(generics.iter(), Ctx::handle_type_node);
        let (ctx, fields) = ctx.visit_fold(fields.iter(), Ctx::handle_field_instantiation);

        let data = FlattenData {
            scope: ctx.scope,
            ast,
        };
        let kind = Kind::Instantiation {
            to: RefIdx::Unresolved,
            generics,
            fields,
        };

        ctx.append(data, kind)
    }

    fn visit(self, ast: &'ast Ast) -> (Ctx<'ast>, RefIdx) {
        let node = AstInfo::Node(ast);

        match &ast.node {
            AstNode::Block {
                stmts,
                last_is_expr,
            } => self.visit_block(node, stmts, *last_is_expr),
            AstNode::Function { decl, block, .. } => self.visit_function(node, decl, block),
            AstNode::Type {
                generics,
                fields,
                with,
                ..
            } => self.visit_type(node, generics, fields, with),
            AstNode::TypeInstantiation(call) => self.visit_type_instantiation(node, call),
            AstNode::FunctionCall(call) => self.visit_function_call(node, call),
            AstNode::MethodCall { instance, call } => self.visit_method_call(node, instance, call),
            AstNode::BinaryOp(op, lhs, rhs) => self.visit_binary_op(&ast.location, op, lhs, rhs),
            AstNode::FieldAccess(instance, field_name) => {
                self.visit_field_access(node, instance, field_name)
            }
            AstNode::IfElse {
                if_condition,
                if_block,
                else_block,
            } => self.visit_if_else(node, if_condition, if_block, else_block),
            AstNode::VarDeclaration { to_declare, value } => {
                self.visit_var_declaration(node, to_declare, value)
            }
            AstNode::Assignment { to_assign, value } => {
                self.visit_var_assign(node, to_assign, value)
            }
            AstNode::VarOrEmptyType(_) => self.visit_var_or_empty_type(node),
            AstNode::Loop(kind, block) => self.visit_loop(node, kind, block),
            AstNode::Return(value) => self.visit_return(node, value),
            // Leaf node
            AstNode::Constant(_) => {
                let data = FlattenData {
                    scope: self.scope,
                    ast: node,
                };
                // FIXME: Aren't we losing the actual constant here?
                let kind = Kind::Constant(RefIdx::Unresolved);
                self.append(data, kind)
            }
            AstNode::Incl { .. } => {
                unreachable!("invalid AST state: `incl` expressions still present")
            }
            // FIXME: Is that correct?
            AstNode::Empty => (self, RefIdx::Unresolved),
        }
    }
}

impl FlattenAst for ast::Ast {
    fn flatten(&self) -> Fir<FlattenData> {
        let ctx = Ctx {
            fir: Fir::new(),
            origin: OriginIdx::default(),
            scope: 0,
        };

        let (ctx, _last_ref) = ctx.visit(self);

        // FIXME: [cfg(not(release))] yada yada
        ctx.fir.check();

        ctx.fir
    }
}

pub trait FlattenAst: Sized {
    fn flatten(&self) -> Fir<FlattenData>;
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! ast {
        ($($tok:tt)*) => {
            xparser::parse(
                stringify!($($tok)*),
                location::Source::Input(stringify!($($tok)*)))
            .unwrap()
        }
    }

    #[test]
    fn block_order() {
        let block = ast! {
            { 14; 15; }
        };

        let fir = block.flatten();

        assert!(matches!(
            fir.nodes.get(&OriginIdx(1)).unwrap().kind,
            Kind::Constant(_)
        ));

        assert!(matches!(
            fir.nodes.get(&OriginIdx(2)).unwrap().kind,
            Kind::Constant(_)
        ));

        assert!(matches!(
            fir.nodes.get(&OriginIdx(3)).unwrap().kind,
            Kind::Statements(_)
        ));
    }

    #[test]
    fn block_expr() {
        let block = ast! {
            { 15 }
        };

        let fir = block.flatten();

        let block = fir.nodes.get(&OriginIdx(3)).unwrap();
        let stmts = match &block.kind {
            Kind::Statements(stmts) => stmts,
            _ => unreachable!(),
        };
        let ret_idx = stmts[0].expect_resolved();

        assert!(matches!(
            fir.nodes.get(&ret_idx).unwrap().kind,
            Kind::Return(_),
        ))
    }

    #[test]
    fn where_expr() {
        let ast = ast! {
            where x = 15;
            where y = x;
        };

        let fir = ast.flatten();

        let fifteen = &fir.nodes.get(&OriginIdx(1)).unwrap().kind;
        let x = &fir.nodes.get(&OriginIdx(2)).unwrap().kind;

        assert!(matches!(fifteen, Kind::Constant(_)));
        assert!(matches!(x, Kind::Binding { .. }));

        let x = &fir.nodes.get(&OriginIdx(3)).unwrap().kind;
        let y = &fir.nodes.get(&OriginIdx(4)).unwrap().kind;

        assert!(matches!(x, Kind::NodeRef { .. }));
        assert!(matches!(y, Kind::Binding { .. }));
    }
}
