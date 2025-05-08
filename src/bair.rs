//! Barq Analyzed Intermediate Representation
//!
//! A data structure that `Analyzer` (in analyzer.rs) emits after semantically analyzing `Bir` (in bir.rs)
//! and the last one before it goes a code generator

use indexmap::{IndexMap, IndexSet};
use ordered_float::OrderedFloat;
use thin_vec::ThinVec;

use crate::{
    ast::{CallingConvention, PointerSize},
    create_index_wrapper, create_index_wrapper_mut,
    token::ByteRange,
};

#[derive(Debug, PartialEq)]
pub struct Bair {
    pub variables: IndexMap<String, Variable>,
    pub functions: IndexMap<String, Function>,
    pub inline_assembly: Vec<InlineAssembly>,
    pub strings: String,
    pub intern_pool: IndexSet<InternKey>,
    pub instructions: Vec<Inst>,
}

create_index_wrapper!(Bair, variables, Variable, VariableIdx);
create_index_wrapper!(Bair, functions, Function, FunctionIdx);
create_index_wrapper_mut!(Bair, inline_assembly, InlineAssembly, InlineAssemblyIdx);
create_index_wrapper_mut!(Bair, instructions, Inst, InstIdx);
create_index_wrapper!(Bair, intern_pool, InternKey, InternIdx);

/// Unlike other uses of ByteRange which point to bytes in SourceFile.buffer, this ByteRange is pointing to a string inside of
/// Bir.strings
pub type StringRange = ByteRange;

#[derive(Debug, Hash, PartialEq, Eq)]
pub struct Variable {
    pub ty: InternIdx,
    pub initializer: InternIdx,
}

#[derive(Debug, Hash, PartialEq, Eq)]
pub struct Function {
    pub signature: InternIdx,
    pub body: ThinVec<InstIdx>,
}

#[derive(Debug, PartialEq)]
pub enum Inst {
    Argument(InternIdx),
    Constant(InternIdx),
    Reference(VariableIdx),
    Loop(InstIdx),
    Break,
    Continue,
    Conditional(Conditional),
    Switch(Switch),
    Call(Call),
    Block(ThinVec<InstIdx>),
    Return(Option<InstIdx>),
    InlineAssembly(InlineAssemblyIdx),
    Cast(Cast),
    Slice(Slice),
    Struct(Struct),
    Array(Array),
    Assign(BinaryOperation),
    ElementOffset(BinaryOperation),
    FieldOffset(FieldOffset),
    BoolNot(InstIdx),
    BitNot(InstIdx),
    Negate(InstIdx),
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
}

#[derive(Debug, PartialEq)]
pub struct Conditional {
    pub condition: InstIdx,
    pub then_case: InstIdx,
    pub else_case: Option<InstIdx>,
}

#[derive(Debug, PartialEq)]
pub struct Switch {
    pub value: InstIdx,
    pub cases: ThinVec<(ThinVec<InstIdx>, InstIdx)>,
    pub else_case: Option<InstIdx>,
}

#[derive(Debug, PartialEq)]
pub struct Call {
    pub callable: InstIdx,
    pub arguments: ThinVec<InstIdx>,
}

#[derive(Debug, PartialEq)]
pub struct InlineAssembly {
    pub content: StringRange,
    pub input_constraints: ThinVec<Constraint>,
    pub output_constraint: Option<OutputConstraint>,
    pub clobbers: ThinVec<ByteRange>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Constraint {
    pub register: ByteRange,
    pub value: InstIdx,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct OutputConstraint {
    pub register: ByteRange,
    pub ty: InternIdx,
}

#[derive(Debug, PartialEq)]
pub struct FieldOffset {
    pub target: InstIdx,
    pub index: usize,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Cast {
    pub target: InstIdx,
    pub ty: InternIdx,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct BinaryOperation {
    pub lhs: InstIdx,
    pub rhs: InstIdx,
}

#[derive(Debug, Hash, PartialEq, Eq)]
pub struct Slice {
    pub target: InstIdx,
    pub start: InstIdx,
    pub end: InstIdx,
}

#[derive(Debug, Hash, PartialEq, Eq)]
pub struct Struct {
    pub ty: InternIdx,
    pub fields: ThinVec<(ByteRange, InstIdx)>,
}

#[derive(Debug, Hash, PartialEq, Eq)]
pub struct Array {
    pub ty: InternIdx,
    pub fields: ThinVec<InstIdx>,
}

#[derive(Debug, Hash, PartialEq, Eq)]
pub enum InternKey {
    String(StringRange),
    Int(i128),
    Float(OrderedFloat<f64>),
    Bool(bool),
    Function(FunctionIdx),
    Slice(InternedSlice),
    Struct(InternedStruct),
    Array(InternedArray),
    TypeTy,
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
}

#[derive(Debug, Hash, PartialEq, Eq)]
pub struct InternedSlice {
    pub array: InternIdx,
    pub start: InternIdx,
    pub end: InternIdx,
}

#[derive(Debug, Hash, PartialEq, Eq)]
pub struct InternedStruct {
    pub ty: InternIdx,
    pub fields: ThinVec<(ByteRange, InternIdx)>,
}

#[derive(Debug, Hash, PartialEq, Eq)]
pub struct InternedArray {
    pub ty: InternIdx,
    pub fields: ThinVec<InternIdx>,
}

#[derive(Debug, Hash, PartialEq, Eq)]
pub struct FunctionTy {
    pub parameter_tys: ThinVec<InternIdx>,
    pub is_var_args: bool,
    pub return_ty: InternIdx,
    pub calling_convention: CallingConvention,
}

#[derive(Debug, Hash, Clone, Copy, PartialEq, Eq)]
pub struct ArrayTy {
    pub len: InternIdx,
    pub child_ty: InternIdx,
}

#[derive(Debug, Hash, Clone, Copy, PartialEq, Eq)]
pub struct PointerTy {
    pub size: PointerSize,
    pub is_const: bool,
    pub child_ty: InternIdx,
}

#[derive(Debug, Hash, PartialEq, Eq)]
pub struct StructTy {
    pub fields: ThinVec<(ByteRange, InternIdx)>,
}

#[derive(Debug, Hash, PartialEq, Eq)]
pub struct EnumTy {
    pub backing_ty: InternIdx,
    pub fields: ThinVec<(ByteRange, InternIdx)>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Ty<'a> {
    Type,
    Void,
    Bool,
    SInt(u16),
    UInt(u16),
    Float(u16),
    Function(&'a FunctionTy),
    Array(ArrayTy),
    Pointer(PointerTy),
    Struct(&'a StructTy),
    Enum(&'a EnumTy),
}

impl<'a> Ty<'a> {
    pub fn can_negate(&self) -> bool {
        matches!(self, Ty::SInt(_) | Ty::Float(_))
    }

    pub fn int_fitting_range(start: i128, end: i128) -> Ty<'a> {
        let is_signed = start < 0;

        let largest_positive_value = end.max(if is_signed { (-start) - 1 } else { start });

        let base = ((largest_positive_value as f64).log2().ceil()) as u16;
        let upper = (1i128 << base) - 1;

        let magnitude_bits = if upper >= largest_positive_value {
            base
        } else {
            base + 1
        };

        match is_signed {
            | true => Ty::SInt(magnitude_bits),
            | false => Ty::UInt(magnitude_bits),
        }
    }

    pub fn float_fitting_range(start: f64, end: f64) -> Ty<'a> {
        let largest_positive_value = end.max(if start < 0.0 { (-start) - 1.0 } else { start });

        let base = (largest_positive_value.log2().ceil()) as u16;
        let upper = ((1i128 << base) - 1) as f64;

        let magnitude_bits = if upper >= largest_positive_value {
            base
        } else {
            base + 1
        };

        match magnitude_bits {
            | x if x <= 16 => Ty::Float(16),
            | x if x <= 32 => Ty::Float(32),
            | x if x <= 64 => Ty::Float(64),
            | x if x <= 128 => Ty::Float(128),
            | _ => unimplemented!("magnitude_bits is too large"),
        }
    }
}

impl<'a> TryFrom<&'a InternKey> for Ty<'a> {
    type Error = ();

    fn try_from(key: &'a InternKey) -> Result<Ty<'a>, Self::Error> {
        Ok(match key {
            | InternKey::VoidTy => Ty::Void,
            | InternKey::BoolTy => Ty::Bool,
            | &InternKey::SIntTy(bits) => Ty::SInt(bits),
            | &InternKey::UIntTy(bits) => Ty::UInt(bits),
            | &InternKey::FloatTy(bits) => Ty::Float(bits),
            | InternKey::FunctionTy(function_ty) => Ty::Function(function_ty),
            | &InternKey::ArrayTy(array_ty) => Ty::Array(array_ty),
            | &InternKey::PointerTy(pointer_ty) => Ty::Pointer(pointer_ty),
            | InternKey::StructTy(struct_ty) => Ty::Struct(struct_ty),
            | InternKey::EnumTy(enum_ty) => Ty::Enum(enum_ty),

            | _ => return Err(()),
        })
    }
}
