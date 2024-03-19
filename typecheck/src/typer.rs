// TODO: Write module documentation
// TODO: Does `Typer` take care of monomorphization as well?

use ast::{Node as AstNode, Value};
use error::Error;
use fir::{Kind, Mapper, Node, OriginIdx, RefIdx};
use flatten::FlattenData;

use crate::{TypeCtx, TypeLinkMap, TypeVariable};

/// This pass takes care of "typing" each node in the [`Fir`], but to a non-terminal type. More explanation can be found
/// in the documentation for [`Typer::ty`].
pub(crate) struct Typer<'ctx>(pub(crate) &'ctx mut TypeCtx<TypeLinkMap>);

impl<'ctx> Typer<'ctx> {
    fn assign_type(&mut self, node: OriginIdx, ty: TypeVariable) {
        // Having non-unique ids in the Fir is an interpreter error
        // Or should we return an error here?
        assert!(self.0.types.insert(node, ty).is_none());
    }

    fn unit(&self) -> RefIdx {
        RefIdx::Resolved(self.0.primitives.unit_type)
    }

    // FIXME: Is this used for declarations? or only for type references? it should be used only for type references
    // FIXME: Add note about NOT using this for declarations
    // FIXME: Can we add newtypes for this?
    /// Assign a type to a node. This type can either be void (the unit type) in the case of a declaration or void
    /// statement, or may be a "type linked list": a reference to a type defined elsewhere in the [`Fir`].
    /// Let's consider a block of multiple statements, the last of which being a function call. The type of a
    /// block is the type of its last statement, which is often a return statement. In that case the return statement
    /// will be of the same type as the function call. Said function call is of the type returned by the function
    /// it will call. You can see how we'll go down this list of type to figure out the actual, final type
    /// of each node in the [`Fir`]. This is however done in another traversal on the [`Fir`] called "Actual" and defined
    /// in another module.
    fn ty<'ast>(
        &mut self,
        node: Node<FlattenData<'ast>>,
        ty: RefIdx,
    ) -> Result<Node<FlattenData<'ast>>, Error> {
        // so is this correct? or can we create actual types here too. I assume NO
        let ty = TypeVariable::Reference(ty);

        self.assign_type(node.origin, ty);

        Ok(node)
    }

    fn type_arithmetic_call(&self, op: builtins::Operator, args: &[RefIdx]) -> RefIdx {
        use builtins::*;

        match op {
            Operator::Arithmetic(_) | Operator::Unary(Unary::Minus) => args[0],
            Operator::Comparison(_) | Operator::Unary(Unary::Not) => {
                RefIdx::Resolved(self.0.primitives.bool_type)
            }
        }
    }
}

impl<'ast, 'ctx> Mapper<FlattenData<'ast>, FlattenData<'ast>, Error> for Typer<'ctx> {
    fn map_constant(
        &mut self,
        data: FlattenData<'ast>,
        origin: OriginIdx,
        _constant: RefIdx,
    ) -> Result<Node<FlattenData<'ast>>, Error> {
        let ast = data.ast.node();

        let ty = match &ast.node {
            // This does not take into account that primitives are multi types and will need to be fixed
            AstNode::Constant(Value::Bool(_)) => self.0.primitives.bool_type,
            AstNode::Constant(Value::Char(_)) => self.0.primitives.char_type,
            AstNode::Constant(Value::Integer(_)) => self.0.primitives.int_type,
            AstNode::Constant(Value::Float(_)) => self.0.primitives.float_type,
            AstNode::Constant(Value::Str(_)) => self.0.primitives.string_type,
            _ => unreachable!(),
        };

        // For constants, how will we look up the basic primitive type nodes before assigning them
        // here? Just a traversal and we do that based on name? Or will they need to be builtin at this point?
        // Some types, like string, int, char, are builtin multi types and will *need* to be builtin.
        // `bool` on the other hand, can be a multi type implemented within the standard library.

        // FIXME: Technically, in jinko, all constants are simply... types of themselves. Which then resolves to
        // the proper primitive multitype. We need to implement this.

        // FIXME: How do we get a TypeReference here? Or should we actually do that operation in the checker?
        let new_node = Node {
            data,
            origin,
            kind: Kind::Constant(RefIdx::Resolved(ty)),
        };

        self.ty(new_node, RefIdx::Resolved(ty))
    }

    fn map_call(
        &mut self,
        data: FlattenData<'ast>,
        origin: OriginIdx,
        to: RefIdx,
        generics: Vec<RefIdx>,
        args: Vec<RefIdx>,
    ) -> Result<Node<FlattenData<'ast>>, Error> {
        let arithmetic = builtins::Operator::try_from_str(data.ast.symbol().unwrap().access());

        let ty = arithmetic
            .map(|op| self.type_arithmetic_call(op, args.as_slice()))
            .unwrap_or(to /* how did this ever work? */);

        // FIXME: How do we access the function's return type here? should we do something in Actual?
        // should we do something here?
        // probably here right but how to access the fucken thing?
        // should Function get typed as their return type?

        let new_node = Node {
            data,
            origin,
            kind: Kind::Call { to, generics, args },
        };

        self.ty(new_node, ty)
    }

    // map_record_type and map_union_type are the two only functions which *create* actual types - all of the other
    // mappers create type references. they are declaration points.

    fn map_record_type(
        &mut self,
        data: FlattenData<'ast>,
        origin: OriginIdx,
        generics: Vec<RefIdx>,
        fields: Vec<RefIdx>,
    ) -> Result<Node<FlattenData<'ast>>, Error> {
        self.assign_type(origin, TypeVariable::Record(origin));

        // and we return the same node since this is mapper
        Ok(Node {
            origin,
            data,
            kind: Kind::RecordType { generics, fields },
        })
    }

    fn map_union_type(
        &mut self,
        data: FlattenData<'ast>,
        origin: OriginIdx,
        generics: Vec<RefIdx>,
        variants: Vec<RefIdx>,
    ) -> Result<Node<FlattenData<'ast>>, Error> {
        self.assign_type(origin, TypeVariable::Union(origin));

        Ok(Node {
            data,
            origin,
            kind: Kind::UnionType { generics, variants },
        })
    }

    fn map_node(
        &mut self,
        node: Node<FlattenData<'ast>>,
    ) -> Result<Node<FlattenData<'ast>>, Error> {
        match node.kind {
            fir::Kind::Constant(c) => self.map_constant(node.data, node.origin, c),
            // Assignments are void
            fir::Kind::Assignment { .. } => self.ty(node, self.unit()),
            // Functions are weird
            // FIXME: Handle them properly
            fir::Kind::Function { return_type, .. } => {
                self.ty(node, return_type.unwrap_or(self.unit()))
            }
            fir::Kind::UnionType { generics, variants } => {
                self.map_union_type(node.data, node.origin, generics, variants)
            }
            fir::Kind::RecordType { generics, fields } => {
                self.map_record_type(node.data, node.origin, generics, fields)
            }
            // These nodes all refer to other nodes, type references or typed values. They will need
            // to be flattened later on.
            fir::Kind::TypeReference(ty)
            | fir::Kind::NodeRef {
                ty: RefIdx::Unresolved,
                value: ty,
            }
            | fir::Kind::Binding { to: ty }
            | fir::Kind::NodeRef { ty, .. }
            | fir::Kind::Instantiation { to: ty, .. }
            | fir::Kind::Conditional { true_block: ty, .. } => self.ty(node, ty),
            // we need to special case `Call`s for arithmetic operators - otherwise, they
            // are also simply a call to `self.ty(node, Some(call.to))`
            fir::Kind::Call { to, generics, args } => {
                self.map_call(node.data, node.origin, to, generics, args)
            }
            // Returns are a bit special as they can already be void
            fir::Kind::Return(ty) => self.ty(node, ty.unwrap_or(self.unit())),
            // Blocks are the same type as their last stmt, or void if it does not exist
            fir::Kind::Statements(ref stmts) => {
                let last = stmts.last().copied();
                self.ty(node, last.unwrap_or(self.unit()))
            }
            // TODO: Figure out what to do with these
            fir::Kind::Generic { .. } | fir::Kind::TypeOffset { .. } | fir::Kind::Loop { .. } => {
                Ok(node)
            }
        }
    }
}
