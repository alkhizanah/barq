//! Semantic Analyzer
//!
//! An analyzer which checks each construction in `Bir` (in bir.rs) to be valid and then emits
//! `Bair` (in bair.rs), includes: type-checking, constant-folding, and other algorithems, also
//! performs some of the built-in calls, like `@import` and `@has_field` calls

use std::{collections::HashMap, fmt};

use indexmap::{IndexMap, IndexSet};

use crate::{
    ast::PointerSize,
    bair::*,
    bcu::{Bcu, SourceFile},
    bir::{self, Bir},
    token::{ByteOffset, TokenLoc},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Value {
    Comptime(InternIdx),
    Runtime(InstIdx),
}

pub enum AnalyzerError {
    UnexpectedValue(TokenLoc, String, String),
}

impl fmt::Display for AnalyzerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            | AnalyzerError::UnexpectedValue(loc, reason, provided) => {
                write!(f, "{}: {}: '{}'", loc, reason, provided)?;
            }
        }

        Ok(())
    }
}

pub type AnalyzerResult<T> = Result<T, AnalyzerError>;

pub struct Analyzer<'a> {
    bcu: &'a Bcu,
    file: &'a SourceFile,
    bir: Bir,
    variables: IndexMap<String, Variable>,
    functions: IndexMap<String, Function>,
    inline_assembly: Vec<InlineAssembly>,
    instructions: Vec<Inst>,
    instructions_map: HashMap<bir::InstIdx, Value>,
    intern_pool: IndexSet<InternKey>,
}

impl<'a> Analyzer<'a> {
    pub fn new(bcu: &'a Bcu, file: &'a SourceFile, bir: Bir) -> Analyzer<'a> {
        let inline_assembly = Vec::with_capacity(bir.inline_assembly.len());
        let instructions = Vec::with_capacity(bir.instructions.len());
        let instructions_map = HashMap::with_capacity(bir.instructions.len());
        let intern_pool = IndexSet::with_capacity(bir.instructions.len() / 2);

        Analyzer {
            bcu,
            file,
            bir,
            variables: IndexMap::new(),
            functions: IndexMap::new(),
            inline_assembly,
            instructions,
            instructions_map,
            intern_pool,
        }
    }

    pub fn analyze(mut self) -> AnalyzerResult<Bair> {
        let module = std::mem::take(&mut self.bir.module);

        for instruction_idx in module.body {
            self.analyze_instruction(instruction_idx)?;
        }

        Ok(Bair {
            variables: self.variables,
            functions: self.functions,
            strings: self.bir.strings,
            inline_assembly: self.inline_assembly,
            instructions: self.instructions,
            intern_pool: self.intern_pool,
        })
    }

    #[inline]
    fn intern(&mut self, key: InternKey) -> InternIdx {
        let (idx, _) = self.intern_pool.insert_full(key);

        InternIdx(idx as u32)
    }

    #[inline]
    fn as_type(&self, interned: InternIdx) -> Ty {
        Ty::try_from(&self.intern_pool[interned.0 as usize])
            .expect("compiler provided an intern index to a value that is not a type in an unexpected place")
    }

    fn type_of(&mut self, value: Value) -> Ty {
        match value {
            | Value::Comptime(idx) => match &self.intern_pool[idx.0 as usize] {
                | InternKey::String(range) => {
                    let len = self.intern(InternKey::Int(range.end as i128 - range.start as i128));
                    let child_ty = self.intern(InternKey::UIntTy(8));

                    Ty::Pointer(PointerTy {
                        size: PointerSize::One,
                        is_const: true,
                        child_ty: self.intern(InternKey::ArrayTy(ArrayTy { len, child_ty })),
                    })
                }

                | &InternKey::Int(int) => Ty::int_fitting_range(int, int + 1),
                | &InternKey::Float(float) => Ty::float_fitting_range(*float, *float + 1.0),
                | InternKey::Bool(_) => Ty::Bool,

                | InternKey::Function(idx) => self.as_type(self.functions[idx.0 as usize].signature),

                | InternKey::Slice(slice) => self.type_of(Value::Comptime(slice.array)),

                | InternKey::Struct(r#struct) => self.as_type(r#struct.ty),

                | InternKey::Array(array) => self.as_type(array.ty),

                | InternKey::TypeTy
                | InternKey::VoidTy
                | InternKey::BoolTy
                | InternKey::SIntTy(_)
                | InternKey::UIntTy(_)
                | InternKey::FloatTy(_)
                | InternKey::FunctionTy(_)
                | InternKey::ArrayTy(_)
                | InternKey::PointerTy(_)
                | InternKey::StructTy(_)
                | InternKey::EnumTy(_) => Ty::Type,
            },

            | Value::Runtime(idx) => match &self.instructions[idx.0 as usize] {
                | &Inst::Argument(type_idx) => self.as_type(type_idx),

                | &Inst::Constant(value_idx) => self.type_of(Value::Comptime(value_idx)),

                | &Inst::Reference(variable_idx) => Ty::Pointer(PointerTy {
                    size: PointerSize::One,
                    is_const: false,
                    child_ty: self.variables[variable_idx.0 as usize].ty,
                }),

                | Inst::Conditional(conditional) => {
                    let Some(else_case) = conditional.else_case else {
                        return Ty::Void;
                    };

                    self.type_of(Value::Runtime(else_case))
                }

                | Inst::Switch(switch) => {
                    let Some(else_case) = switch.else_case else {
                        return Ty::Void;
                    };

                    self.type_of(Value::Runtime(else_case))
                }

                | Inst::Call(call) => match self.type_of(Value::Runtime(call.callable)) {
                    | Ty::Function(&FunctionTy { return_ty, .. }) => self.as_type(return_ty),

                    | Ty::Pointer(PointerTy { child_ty, .. }) => {
                        let child_ty = self.as_type(child_ty);

                        let Ty::Function(&FunctionTy { return_ty, .. }) = child_ty else {
                            return Ty::Void;
                        };

                        self.as_type(return_ty)
                    }

                    | _ => Ty::Void,
                },

                | Inst::InlineAssembly(inline_assembly_idx) => {
                    let inline_assembly = &self.inline_assembly[inline_assembly_idx.0 as usize];

                    let Some(output_constraint) = inline_assembly.output_constraint else {
                        return Ty::Void;
                    };

                    self.as_type(output_constraint.ty)
                }

                | Inst::Cast(cast) => self.as_type(cast.ty),

                | Inst::Slice(slice) => {
                    let Ty::Pointer(target_ty) = self.type_of(Value::Runtime(slice.target)) else {
                        return Ty::Void;
                    };

                    let child_ty = match target_ty.size {
                        | PointerSize::One => {
                            let Ty::Array(target_ty) = self.as_type(target_ty.child_ty) else {
                                return Ty::Void;
                            };

                            target_ty.child_ty
                        }

                        | PointerSize::Many | PointerSize::Slice => target_ty.child_ty,
                    };

                    Ty::Pointer(PointerTy {
                        is_const: false,
                        size: PointerSize::Slice,
                        child_ty,
                    })
                }

                | Inst::Struct(r#struct) => self.as_type(r#struct.ty),
                | Inst::Array(array) => self.as_type(array.ty),
                | Inst::Assign(assign) => self.type_of(Value::Runtime(assign.rhs)),

                | Inst::ElementOffset(offset) => {
                    let Ty::Pointer(target_ty) = self.type_of(Value::Runtime(offset.lhs)) else {
                        return Ty::Void;
                    };

                    let child_ty = match target_ty.size {
                        | PointerSize::One => {
                            let Ty::Array(target_ty) = self.as_type(target_ty.child_ty) else {
                                return Ty::Void;
                            };

                            target_ty.child_ty
                        }

                        | PointerSize::Many | PointerSize::Slice => target_ty.child_ty,
                    };

                    Ty::Pointer(PointerTy {
                        is_const: false,
                        size: PointerSize::One,
                        child_ty,
                    })
                }

                | &Inst::FieldOffset(FieldOffset { target, index }) => {
                    let Ty::Struct(target_ty) = self.type_of(Value::Runtime(target)) else {
                        return Ty::Void;
                    };

                    let (_, child_ty) = target_ty.fields[index];

                    Ty::Pointer(PointerTy {
                        is_const: false,
                        size: PointerSize::One,
                        child_ty,
                    })
                }

                | &Inst::BitNot(target) | &Inst::Negate(target) => self.type_of(Value::Runtime(target)),

                | &Inst::Add(BinaryOperation { lhs, .. })
                | &Inst::Subtract(BinaryOperation { lhs, .. })
                | &Inst::Multiply(BinaryOperation { lhs, .. })
                | &Inst::Divide(BinaryOperation { lhs, .. })
                | &Inst::Modulo(BinaryOperation { lhs, .. })
                | &Inst::LeftShift(BinaryOperation { lhs, .. })
                | &Inst::RightShift(BinaryOperation { lhs, .. })
                | &Inst::BitAnd(BinaryOperation { lhs, .. })
                | &Inst::BitOr(BinaryOperation { lhs, .. })
                | &Inst::BitXor(BinaryOperation { lhs, .. }) => self.type_of(Value::Runtime(lhs)),

                | Inst::BoolNot(_)
                | Inst::Eql(_)
                | Inst::NotEql(_)
                | Inst::LessThan(_)
                | Inst::LessOrEql(_)
                | Inst::GreaterThan(_)
                | Inst::GreaterOrEql(_) => Ty::Bool,

                | Inst::Loop(_) | Inst::Break | Inst::Continue | Inst::Block(_) | Inst::Return(_) => Ty::Void,
            },
        }
    }

    fn value_as_string(&self, value: Value) -> String {
        match value {
            | Value::Comptime(idx) => match &self.intern_pool[idx.0 as usize] {
                | InternKey::String(range) => format!("<string '{}'>", range.get(&self.bir.strings)),
                | InternKey::Int(int) => format!("{}", int),
                | InternKey::Float(float) => format!("{}", float),
                | InternKey::Bool(boolean) => format!("{}", boolean),

                | InternKey::Function(idx) => {
                    format!("<function '{}'>", self.functions.keys()[idx.0 as usize])
                }

                | InternKey::Slice(_) => "<slice>".to_string(),
                | InternKey::Struct(_) => "<struct>".to_string(),
                | InternKey::Array(_) => "<array>".to_string(),
                | InternKey::TypeTy => "type".to_string(),
                | InternKey::VoidTy => "void".to_string(),
                | InternKey::BoolTy => "bool".to_string(),
                | InternKey::SIntTy(bits) => format!("s{}", bits),
                | InternKey::UIntTy(bits) => format!("u{}", bits),
                | InternKey::FloatTy(bits) => format!("f{}", bits),

                | InternKey::FunctionTy(signature) => {
                    let mut output = "fn (".to_string();

                    let parameter_count = signature.parameter_tys.len();

                    for (i, &parameter_ty) in signature.parameter_tys.iter().enumerate() {
                        output.push_str(self.value_as_string(Value::Comptime(parameter_ty)).as_str());

                        if i != parameter_count - 1 {
                            output.push_str(", ");
                        }
                    }

                    output.push_str(") ");

                    output.push_str(
                        self.value_as_string(Value::Comptime(signature.return_ty))
                            .as_str(),
                    );

                    output
                }

                | &InternKey::ArrayTy(ArrayTy { len, child_ty }) => {
                    let mut output = "[".to_string();

                    output.push_str(self.value_as_string(Value::Comptime(len)).as_str());

                    output.push(']');

                    output.push_str(self.value_as_string(Value::Comptime(child_ty)).as_str());

                    output
                }

                | &InternKey::PointerTy(PointerTy {
                    size,
                    is_const,
                    child_ty,
                }) => {
                    let mut output = match size {
                        | PointerSize::One => "*",
                        | PointerSize::Many => "[*]",
                        | PointerSize::Slice => "[]",
                    }
                    .to_string();

                    if is_const {
                        output.push_str("const ");
                    }

                    output.push_str(self.value_as_string(Value::Comptime(child_ty)).as_str());

                    output
                }

                // TODO: Structs and Enums should be displayed by name
                | InternKey::StructTy(_) => "<struct type>".to_string(),
                | InternKey::EnumTy(_) => "<enum type>".to_string(),
            },

            | Value::Runtime(_) => "<runtime value>".to_string(),
        }
    }

    #[inline]
    fn not_a(&self, what: &str, value: Value, start: ByteOffset) -> AnalyzerError {
        AnalyzerError::UnexpectedValue(
            TokenLoc::find(start, self.file),
            "not a ".to_string() + what,
            self.value_as_string(value),
        )
    }

    #[inline]
    fn not_an(&self, what: &str, value: Value, start: ByteOffset) -> AnalyzerError {
        AnalyzerError::UnexpectedValue(
            TokenLoc::find(start, self.file),
            "not an ".to_string() + what,
            self.value_as_string(value),
        )
    }

    #[inline]
    fn push_instruction(&mut self, instruction: Inst) -> InstIdx {
        let idx = InstIdx(self.instructions.len() as u32);

        self.instructions.push(instruction);

        idx
    }

    #[inline]
    fn promote_to_instruction(&mut self, value: Value) -> InstIdx {
        match value {
            | Value::Comptime(intern_idx) => self.push_instruction(Inst::Constant(intern_idx)),
            | Value::Runtime(instruction_idx) => instruction_idx,
        }
    }

    #[inline]
    #[allow(dead_code)]
    fn analyze_optional_instruction(
        &mut self,
        optional_instruction: Option<bir::InstIdx>,
    ) -> AnalyzerResult<Option<Value>> {
        if let Some(instruction_idx) = optional_instruction {
            self.analyze_instruction(instruction_idx).map(Some)
        } else {
            Ok(None)
        }
    }

    fn analyze_instruction(&mut self, bir_instruction_idx: bir::InstIdx) -> AnalyzerResult<Value> {
        if let Some(&value) = self.instructions_map.get(&bir_instruction_idx) {
            return Ok(value);
        }

        let bir_instruction = std::mem::replace(&mut self.bir[bir_instruction_idx], bir::Inst::Int(0));

        let value = match bir_instruction {
            | bir::Inst::String(range) => Ok(Value::Comptime(self.intern(InternKey::String(range)))),
            | bir::Inst::Int(int) => Ok(Value::Comptime(self.intern(InternKey::Int(int as i128)))),
            | bir::Inst::Float(float) => Ok(Value::Comptime(self.intern(InternKey::Float(float)))),
            | bir::Inst::Bool(boolean) => Ok(Value::Comptime(self.intern(InternKey::Bool(boolean)))),
            | bir::Inst::Slice(slice) => self.analyze_slice(slice),

            | _ => todo!("analyze more instructions"),
        }?;

        self.instructions_map.insert(bir_instruction_idx, value);

        Ok(value)
    }

    fn analyze_slice(&mut self, slice: bir::Slice) -> AnalyzerResult<Value> {
        let start = slice.start;

        let target = self.analyze_instruction(slice.target)?;

        let Ty::Pointer(target_ty) = self.type_of(target) else {
            return Err(self.not_a("pointer", target, start));
        };

        if target_ty.size == PointerSize::One || !matches!(self.as_type(target_ty.child_ty), Ty::Array(_)) {
            return Err(self.not_an("array", Value::Comptime(target_ty.child_ty), start));
        };

        let range_start = self.analyze_instruction(slice.range_start)?;

        let range_start_ty = self.type_of(range_start);

        let Ty::UInt(range_start_ty_bits) = range_start_ty else {
            return Err(self.not_an("unsigned int", range_start, start));
        };

        if range_start_ty_bits > self.bcu.target.pointer_bit_width() {
            return Err(AnalyzerError::UnexpectedValue(
                TokenLoc::find(start, self.file),
                "range start exceeded the max of target's usize".to_string(),
                self.value_as_string(range_start),
            ));
        }

        let range_end = self.analyze_instruction(slice.range_end)?;

        let range_end_ty = self.type_of(range_end);

        let Ty::UInt(range_end_ty_bits) = range_end_ty else {
            return Err(self.not_an("unsigned int", range_end, start));
        };

        if range_end_ty_bits > self.bcu.target.pointer_bit_width() {
            return Err(AnalyzerError::UnexpectedValue(
                TokenLoc::find(start, self.file),
                "range end exceeded the max of target's usize".to_string(),
                self.value_as_string(range_end),
            ));
        }

        if let Value::Comptime(comptime_range_start) = range_start {
            if let Value::Comptime(comptime_range_end) = range_end {
                let InternKey::Int(comptime_range_start) = self.intern_pool[comptime_range_start.0 as usize]
                else {
                    unreachable!();
                };

                let InternKey::Int(comptime_range_end) = self.intern_pool[comptime_range_end.0 as usize]
                else {
                    unreachable!();
                };

                if comptime_range_start > comptime_range_end {
                    return Err(AnalyzerError::UnexpectedValue(
                        TokenLoc::find(start, self.file),
                        "range start is bigger than range end, which is undefined behaviour".to_string(),
                        self.value_as_string(range_start),
                    ));
                }

                if let Ty::Array(array_ty) = self.as_type(target_ty.child_ty) {
                    let InternKey::Int(len) = self.intern_pool[array_ty.len.0 as usize] else {
                        unreachable!();
                    };

                    if comptime_range_start >= len {
                        return Err(AnalyzerError::UnexpectedValue(
                            TokenLoc::find(start, self.file),
                            "range start is out of bounds, array len is ".to_string()
                                + len.to_string().as_str(),
                            self.value_as_string(range_start),
                        ));
                    }

                    if comptime_range_end > len {
                        return Err(AnalyzerError::UnexpectedValue(
                            TokenLoc::find(start, self.file),
                            "range end is out of bounds, array len is ".to_string()
                                + len.to_string().as_str(),
                            self.value_as_string(range_end),
                        ));
                    }
                }

                if let Value::Comptime(comptime_array) = target {
                    let InternKey::Array(ref comptime_array) = self.intern_pool[comptime_array.0 as usize]
                    else {
                        unreachable!();
                    };

                    if range_start_ty_bits as u32 > usize::BITS {
                        return Err(AnalyzerError::UnexpectedValue(
                            TokenLoc::find(start, self.file),
                            "range start exceeded the max of compiler's usize".to_string(),
                            self.value_as_string(range_start),
                        ));
                    }

                    if range_end_ty_bits as u32 > usize::BITS {
                        return Err(AnalyzerError::UnexpectedValue(
                            TokenLoc::find(start, self.file),
                            "range end exceeded the max of compiler's usize".to_string(),
                            self.value_as_string(range_end),
                        ));
                    }

                    return Ok(Value::Comptime(
                        self.intern(InternKey::Array(InternedArray {
                            ty: comptime_array.ty,
                            fields: comptime_array.fields
                                [comptime_range_start as usize..comptime_range_end as usize]
                                .into(),
                        })),
                    ));
                }
            }
        }

        let target = self.promote_to_instruction(target);
        let range_start = self.promote_to_instruction(range_start);
        let range_end = self.promote_to_instruction(range_end);

        Ok(Value::Runtime(self.push_instruction(Inst::Slice(Slice {
            target,
            start: range_start,
            end: range_end,
        }))))
    }
}
