//! Abstract Syntax Tree
//!
//! A tree that reperesents source code as `Node`s instead of `Token`s (in token.rs)

use ordered_float::OrderedFloat;
use thin_vec::ThinVec;

use crate::{create_index_wrapper_mut, token::*};

#[derive(Debug, PartialEq)]
pub struct Ast {
    pub module: StructTy,
    pub strings: String,
    pub inline_assembly: Vec<InlineAssembly>,
    pub nodes: Vec<Node>,
}

create_index_wrapper_mut!(Ast, inline_assembly, InlineAssembly, InlineAssemblyIdx);
create_index_wrapper_mut!(Ast, nodes, Node, NodeIdx);

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Binding {
    pub name: ByteRange,
    pub ty: Option<NodeIdx>,
    pub value: NodeIdx,
}

/// Unlike other uses of ByteRange which point to bytes in SourceFile.buffer, this ByteRange is pointing to a string inside of
/// Ast.strings
pub type StringRange = ByteRange;

#[derive(Debug, PartialEq)]
pub enum Node {
    Identifier(ByteRange),
    String(StringRange),
    Int(u64),
    Float(OrderedFloat<f64>),
    Function(Function),
    Slice(Slice),
    Struct(Struct),
    Array(Array),
    Conditional(Conditional),
    Switch(Switch),
    Assign(Assign),
    BuiltinCall(BuiltinCall),
    Call(Call),
    InlineAssembly(InlineAssemblyIdx),
    Cast(BinaryOperation),
    ElementAccess(BinaryOperation),
    FieldAccess(FieldAccess),
    Dereference(UnaryOperation),
    Reference(UnaryOperation),
    BoolNot(UnaryOperation),
    BitNot(UnaryOperation),
    Negate(UnaryOperation),
    Add(BinaryOperation),
    Subtract(BinaryOperation),
    Multiply(BinaryOperation),
    Divide(BinaryOperation),
    Modulo(BinaryOperation),
    LeftShift(BinaryOperation),
    RightShift(BinaryOperation),
    BitAnd(BinaryOperation),
    BitOr(BinaryOperation),
    BitXor(BinaryOperation),
    Eql(BinaryOperation),
    NotEql(BinaryOperation),
    LessThan(BinaryOperation),
    LessOrEql(BinaryOperation),
    GreaterThan(BinaryOperation),
    GreaterOrEql(BinaryOperation),
    FunctionTy(FunctionTy),
    ArrayTy(ArrayTy),
    PointerTy(PointerTy),
    StructTy(StructTy),
    EnumTy(EnumTy),
    Constant(Binding),
    Variable(Binding),
    WhileLoop(WhileLoop),
    Break(ByteOffset),
    Continue(ByteOffset),
    Defer(ThinVec<NodeIdx>),
    Block(ThinVec<NodeIdx>),
    Return(Return),
}

#[derive(Debug, PartialEq)]
pub enum UnaryOperator {
    BoolNot,
    BitNot,
    Negate,
    Reference,
}

#[derive(Debug, PartialEq)]
pub enum BinaryOperator {
    Plus,
    Minus,
    Multiply,
    Divide,
    Modulo,
    LeftShift,
    RightShift,
    BitAnd,
    BitOr,
    BitXor,
    Eql,
    NotEql,
    LessThan,
    LessOrEql,
    GreaterThan,
    GreaterOrEql,
}

impl From<Operator> for BinaryOperator {
    fn from(operator: Operator) -> Self {
        match operator {
            | Operator::Plus => BinaryOperator::Plus,
            | Operator::Minus => BinaryOperator::Minus,
            | Operator::Multiply => BinaryOperator::Multiply,
            | Operator::Divide => BinaryOperator::Divide,
            | Operator::Modulo => BinaryOperator::Modulo,
            | Operator::LeftShift => BinaryOperator::LeftShift,
            | Operator::RightShift => BinaryOperator::RightShift,

            | Operator::Bitwise(bitwise) => match bitwise {
                | Bitwise::Not => unimplemented!("bitwise not operator is not a binary operator"),
                | Bitwise::And => BinaryOperator::BitAnd,
                | Bitwise::Or => BinaryOperator::BitOr,
                | Bitwise::Xor => BinaryOperator::BitXor,
            },
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Function {
    pub signature: NodeIdx,
    pub foreign: Option<ByteRange>,
    pub body: ThinVec<NodeIdx>,
}

#[derive(Debug, PartialEq, Default)]
pub struct FunctionTy {
    pub parameters: ThinVec<(ByteRange, NodeIdx)>,
    pub is_var_args: bool,
    pub return_ty: Option<NodeIdx>,
    pub calling_convention: CallingConvention,
    pub start: ByteOffset,
}

#[derive(Debug, Hash, PartialEq, Eq, Clone, Copy, Default)]
pub enum CallingConvention {
    #[default]
    Auto,
    C,
    Inline,
    Naked,
}

#[derive(Debug, PartialEq)]
pub struct ArrayTy {
    pub len: NodeIdx,
    pub child_ty: NodeIdx,
    pub start: ByteOffset,
}

#[derive(Debug, PartialEq)]
pub struct PointerTy {
    pub size: PointerSize,
    pub is_const: bool,
    pub child_ty: NodeIdx,
    pub start: ByteOffset,
}

#[derive(Debug, Hash, Clone, Copy, PartialEq, Eq)]
pub enum PointerSize {
    One,
    Many,
    Slice,
}

#[derive(Default, Debug, PartialEq)]
pub struct StructTy {
    pub fields: ThinVec<(ByteRange, NodeIdx)>,
    pub constants: ThinVec<Binding>,
    pub variables: ThinVec<Binding>,
    pub start: ByteOffset,
}

#[derive(Debug, PartialEq)]
pub struct EnumTy {
    pub backing_ty: Option<NodeIdx>,
    pub fields: ThinVec<(ByteRange, Option<NodeIdx>)>,
    pub constants: ThinVec<Binding>,
    pub variables: ThinVec<Binding>,
    pub start: ByteOffset,
}

#[derive(Debug, PartialEq, Default)]
pub struct InlineAssembly {
    pub content: StringRange,
    pub input_constraints: ThinVec<Constraint>,
    pub output_constraint: Option<Constraint>,
    pub clobbers: ThinVec<ByteRange>,
    pub start: ByteOffset,
}

#[derive(Debug, PartialEq)]
pub struct Constraint {
    pub register: ByteRange,
    pub value: NodeIdx,
}

#[derive(Debug, PartialEq)]
pub struct Conditional {
    pub condition: NodeIdx,
    pub then_case: NodeIdx,
    pub else_case: Option<NodeIdx>,
    pub start: ByteOffset,
}

#[derive(Debug, PartialEq)]
pub struct Switch {
    pub value: NodeIdx,
    pub cases: ThinVec<(ThinVec<NodeIdx>, NodeIdx)>,
    pub else_case: Option<NodeIdx>,
    pub start: ByteOffset,
}

#[derive(Debug, PartialEq)]
pub struct BuiltinCall {
    pub kind: BuiltinKind,
    pub arguments: ThinVec<NodeIdx>,
    pub start: ByteOffset,
}

#[derive(Debug, PartialEq)]
pub enum BuiltinKind {
    Import,
    Uninitialized,
    HasField,
}

#[derive(Debug, PartialEq)]
pub struct Assign {
    pub target: NodeIdx,
    pub operator: Option<BinaryOperator>,
    pub value: NodeIdx,
    pub start: ByteOffset,
}

#[derive(Debug, PartialEq)]
pub struct Call {
    pub callable: NodeIdx,
    pub arguments: ThinVec<NodeIdx>,
    pub start: ByteOffset,
}

#[derive(Debug, PartialEq)]
pub struct Slice {
    pub target: NodeIdx,
    pub range_start: NodeIdx,
    pub range_end: NodeIdx,
    pub start: ByteOffset,
}

#[derive(Debug, PartialEq)]
pub struct Struct {
    pub ty: NodeIdx,
    pub fields: ThinVec<(ByteRange, NodeIdx)>,
    pub start: ByteOffset,
}

#[derive(Debug, PartialEq)]
pub struct Array {
    pub ty: NodeIdx,
    pub values: ThinVec<NodeIdx>,
    pub start: ByteOffset,
}

#[derive(Debug, PartialEq)]
pub struct FieldAccess {
    pub target: NodeIdx,
    pub field: ByteRange,
}

#[derive(Debug, PartialEq)]
pub struct UnaryOperation {
    pub rhs: NodeIdx,
    pub start: ByteOffset,
}

#[derive(Debug, PartialEq)]
pub struct BinaryOperation {
    pub lhs: NodeIdx,
    pub rhs: NodeIdx,
    pub start: ByteOffset,
}

#[derive(Debug, PartialEq)]
pub struct WhileLoop {
    pub condition: NodeIdx,
    pub body: NodeIdx,
    pub start: ByteOffset,
}

#[derive(Debug, PartialEq)]
pub struct Defer {
    pub deferred: NodeIdx,
    pub start: ByteOffset,
}

#[derive(Debug, PartialEq)]
pub struct Return {
    pub value: Option<NodeIdx>,
    pub start: ByteOffset,
}
