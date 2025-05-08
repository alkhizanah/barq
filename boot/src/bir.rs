//! Barq Intermediate Representation
//!
//! A data structure that reperesnts the `Ast` (in ast.rs) as instuctions that can be used as values
//! or commands to `Analyzer` (in analyzer.rs)

use ordered_float::OrderedFloat;
use thin_vec::ThinVec;

use crate::{
    ast::{BuiltinKind, CallingConvention, PointerSize},
    create_index_wrapper_mut,
    token::{ByteOffset, ByteRange},
};

#[derive(Debug, PartialEq)]
pub struct Bir {
    pub module: StructTy,
    pub inline_assembly: Vec<InlineAssembly>,
    pub instructions: Vec<Inst>,
    pub strings: String,
}

create_index_wrapper_mut!(Bir, inline_assembly, InlineAssembly, InlineAssemblyIdx);
create_index_wrapper_mut!(Bir, instructions, Inst, InstIdx);

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Binding {
    pub ty: Option<InstIdx>,
    pub value: InstIdx,
    pub start: ByteOffset,
}

/// Unlike other uses of ByteRange which point to bytes in SourceFile.buffer, this ByteRange is pointing to a string inside of
/// Bir.strings
pub type StringRange = ByteRange;

#[derive(Debug, PartialEq)]
pub enum Inst {
    String(StringRange),
    Int(u64),
    Float(OrderedFloat<f64>),
    Bool(bool),
    Function(Function),
    Slice(Slice),
    Struct(Struct),
    Array(Array),
    Conditional(Conditional),
    Switch(Switch),
    BuiltinCall(BuiltinCall),
    Call(Call),
    InlineAssembly(InlineAssemblyIdx),
    Cast(BinaryOperation),
    Assign(BinaryOperation),
    Dereference(UnaryOperation),
    Reference(UnaryOperation),
    ElementAccess(BinaryOperation),
    FieldAccess(FieldAccess),
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
    VoidTy,
    BoolTy,
    SIntTy(u16),
    UIntTy(u16),
    FloatTy(u16),
    FunctionTy(FunctionTy),
    ArrayTy(ArrayTy),
    PointerTy(PointerTy),
    StructTy(StructTy),
    EnumTy(EnumTy),
    Duplicate(InstIdx),
    Argument(InstIdx),
    Constant(Binding),
    Variable(Binding),
    Loop(InstIdx),
    Block(ThinVec<InstIdx>),
    Break,
    Continue,
    Return(Return),
}

#[derive(Debug, PartialEq)]
pub struct Function {
    pub signature: InstIdx,
    pub foreign: Option<ByteRange>,
    pub body: ThinVec<InstIdx>,
}

#[derive(Debug, PartialEq)]
pub struct FunctionTy {
    pub parameters: ThinVec<(ByteRange, InstIdx)>,
    pub is_var_args: bool,
    pub return_ty: InstIdx,
    pub calling_convention: CallingConvention,
    pub start: ByteOffset,
}

#[derive(Debug, PartialEq)]
pub struct ArrayTy {
    pub len: InstIdx,
    pub child_ty: InstIdx,
    pub start: ByteOffset,
}

#[derive(Debug, PartialEq)]
pub struct PointerTy {
    pub size: PointerSize,
    pub is_const: bool,
    pub child_ty: InstIdx,
    pub start: ByteOffset,
}

#[derive(Debug, Default, PartialEq, Eq)]
pub struct StructTy {
    pub fields: ThinVec<(ByteRange, InstIdx)>,
    pub body: ThinVec<InstIdx>,
    pub start: ByteOffset,
}

#[derive(Debug, PartialEq)]
pub struct EnumTy {
    pub backing_ty: Option<InstIdx>,
    pub fields: ThinVec<(ByteRange, Option<InstIdx>)>,
    pub body: ThinVec<InstIdx>,
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
    pub value: InstIdx,
}

#[derive(Debug, PartialEq)]
pub struct Conditional {
    pub condition: InstIdx,
    pub then_case: InstIdx,
    pub else_case: Option<InstIdx>,
    pub start: ByteOffset,
}

#[derive(Debug, PartialEq)]
pub struct Switch {
    pub value: InstIdx,
    pub cases: ThinVec<(ThinVec<InstIdx>, InstIdx)>,
    pub else_case: Option<InstIdx>,
    pub start: ByteOffset,
}

#[derive(Debug, PartialEq)]
pub struct BuiltinCall {
    pub kind: BuiltinKind,
    pub arguments: ThinVec<InstIdx>,
    pub start: ByteOffset,
}

#[derive(Debug, PartialEq)]
pub struct Call {
    pub callable: InstIdx,
    pub arguments: ThinVec<InstIdx>,
    pub start: ByteOffset,
}

#[derive(Debug, PartialEq)]
pub struct Slice {
    pub target: InstIdx,
    pub range_start: InstIdx,
    pub range_end: InstIdx,
    pub start: ByteOffset,
}

#[derive(Debug, PartialEq)]
pub struct Struct {
    pub ty: InstIdx,
    pub fields: ThinVec<(ByteRange, InstIdx)>,
    pub start: ByteOffset,
}

#[derive(Debug, PartialEq)]
pub struct Array {
    pub ty: InstIdx,
    pub values: ThinVec<InstIdx>,
    pub start: ByteOffset,
}

#[derive(Debug, PartialEq)]
pub struct FieldAccess {
    pub target: InstIdx,
    pub field: ByteRange,
}

#[derive(Debug, PartialEq)]
pub struct UnaryOperation {
    pub rhs: InstIdx,
    pub start: ByteOffset,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct BinaryOperation {
    pub lhs: InstIdx,
    pub rhs: InstIdx,
    pub start: ByteOffset,
}

#[derive(Debug, PartialEq)]
pub struct Return {
    pub value: Option<InstIdx>,
    pub start: ByteOffset,
}
