//! Lowerer
//
//! A data transformer that converts `Ast` (in ast.rs) to `Bir` (in bir.rs)

use std::{collections::HashSet, fmt};

use thin_vec::ThinVec;

use crate::{
    ast::{self, Ast, BinaryOperator},
    bcu::{Bcu, CType, SourceFile},
    bir::*,
    scope::Scope,
    token::{ByteOffset, TokenLoc},
};

pub enum LowererError {
    UnexpectedNode(TokenLoc, &'static str),
    UnexpectedValue(TokenLoc, &'static str, String),
}

impl fmt::Display for LowererError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            | LowererError::UnexpectedNode(loc, reason) => write!(f, "{}: {}", loc, reason)?,

            | LowererError::UnexpectedValue(loc, reason, provided) => {
                write!(f, "{}: {}: '{}'", loc, reason, provided)?
            }
        }

        Ok(())
    }
}

pub type LowererResult<T> = Result<T, LowererError>;

pub struct Lowerer<'a> {
    bcu: &'a Bcu,
    file: &'a SourceFile,
    ast: Ast,
    instructions: Vec<Inst>,
    scope: Scope<&'a str, InstIdx>,
    scope_backtrack: Scope<&'a str, ThinVec<InstIdx>>,
    within_loop: bool,
    break_inst_idx: InstIdx,
    continue_inst_idx: InstIdx,
}

impl<'a> Lowerer<'a> {
    pub fn new(bcu: &'a Bcu, file: &'a SourceFile, ast: Ast) -> Lowerer<'a> {
        let mut instructions = Vec::with_capacity(ast.nodes.len() + 2);

        instructions.push(Inst::Break);
        instructions.push(Inst::Continue);

        Lowerer {
            bcu,
            file,
            ast,
            instructions,
            scope: Scope::with_layers_capacity(255),
            scope_backtrack: Scope::with_layers_capacity(128),
            within_loop: false,
            break_inst_idx: InstIdx(0),
            continue_inst_idx: InstIdx(1),
        }
    }

    fn push_instruction(&mut self, instruction: Inst) -> InstIdx {
        let idx = InstIdx(self.instructions.len() as u32);

        self.instructions.push(instruction);

        idx
    }

    pub fn lower(mut self) -> LowererResult<Bir> {
        let module = std::mem::take(&mut self.ast.module);

        self.lower_struct_ty_inner(module).map(|module| {
            self.instructions.shrink_to_fit();

            Bir {
                module,
                strings: self.ast.strings,
                instructions: self.instructions,
            }
        })
    }

    fn lower_node(&mut self, node_idx: ast::NodeIdx) -> LowererResult<InstIdx> {
        let node = std::mem::replace(&mut self.ast[node_idx], ast::Node::Int(0));

        #[rustfmt::skip]
        let instruction = match node {
            | ast::Node::Identifier(range) => return self.lower_identifier(range.get(&self.file.buffer), range.start),
            | ast::Node::String(range) => Ok(Inst::String(range)),
            | ast::Node::Int(int) => Ok(Inst::Int(int)),
            | ast::Node::Float(float) => Ok(Inst::Float(float)),
            | ast::Node::Function(function) => self.lower_function(function),
            | ast::Node::Slice(slice) => self.lower_slice(slice),
            | ast::Node::Struct(r#struct) => self.lower_struct(r#struct),
            | ast::Node::Array(array) => self.lower_array(array),
            | ast::Node::Conditional(conditional) => self.lower_conditional(conditional),
            | ast::Node::Switch(switch) => self.lower_switch(switch),
            | ast::Node::Assign(assign) => self.lower_assign(assign),
            | ast::Node::BuiltinCall(builtin_call) => self.lower_builtin_call(builtin_call),
            | ast::Node::Call(call) => self.lower_call(call),
            | ast::Node::InlineAssembly(inline_assembly) => self.lower_inline_assembly(inline_assembly),
            | ast::Node::Cast(binary_operation) => self.lower_binary_operation(binary_operation).map(Inst::Cast),
            | ast::Node::ElementAccess(binary_operation) => self.lower_binary_operation(binary_operation).map(Inst::ElementAccess),
            | ast::Node::FieldAccess(field_access) => self.lower_field_access(field_access),
            | ast::Node::Dereference(unary_operation) => self.lower_unary_operation(unary_operation).map(Inst::Dereference),
            | ast::Node::Reference(unary_operation) => self.lower_unary_operation(unary_operation).map(Inst::Reference),
            | ast::Node::BoolNot(unary_operation) => self.lower_unary_operation(unary_operation).map(Inst::BoolNot),
            | ast::Node::BitNot(unary_operation) =>  self.lower_unary_operation(unary_operation).map(Inst::BitNot),
            | ast::Node::Negate(unary_operation) => self.lower_unary_operation(unary_operation).map(Inst::Negate),
            | ast::Node::Add(binary_operation) => self.lower_binary_operation(binary_operation).map(Inst::Add),
            | ast::Node::Subtract(binary_operation) => self.lower_binary_operation(binary_operation).map(Inst::Subtract),
            | ast::Node::Multiply(binary_operation) => self.lower_binary_operation(binary_operation).map(Inst::Multiply),
            | ast::Node::Divide(binary_operation) => self.lower_binary_operation(binary_operation).map(Inst::Divide),
            | ast::Node::Modulo(binary_operation) => self.lower_binary_operation(binary_operation).map(Inst::Modulo),
            | ast::Node::LeftShift(binary_operation) => self.lower_binary_operation(binary_operation).map(Inst::LeftShift),
            | ast::Node::RightShift(binary_operation) => self.lower_binary_operation(binary_operation).map(Inst::RightShift),
            | ast::Node::BitAnd(binary_operation) => self.lower_binary_operation(binary_operation).map(Inst::BitAnd),
            | ast::Node::BitOr(binary_operation) => self.lower_binary_operation(binary_operation).map(Inst::BitOr),
            | ast::Node::BitXor(binary_operation) => self.lower_binary_operation(binary_operation).map(Inst::BitXor),
            | ast::Node::Eql(binary_operation) => self.lower_binary_operation(binary_operation).map(Inst::Eql),
            | ast::Node::NotEql(binary_operation) => self.lower_binary_operation(binary_operation).map(Inst::NotEql),
            | ast::Node::LessThan(binary_operation) => self.lower_binary_operation(binary_operation).map(Inst::LessThan),
            | ast::Node::LessOrEql(binary_operation) => self.lower_binary_operation(binary_operation).map(Inst::LessOrEql),
            | ast::Node::GreaterThan(binary_operation) => self.lower_binary_operation(binary_operation).map(Inst::GreaterThan),
            | ast::Node::GreaterOrEql(binary_operation) => self.lower_binary_operation(binary_operation).map(Inst::GreaterOrEql),
            | ast::Node::FunctionTy(function_ty) => self.lower_function_ty(function_ty),
            | ast::Node::ArrayTy(array_ty) => self.lower_array_ty(array_ty),
            | ast::Node::PointerTy(pointer_ty) => self.lower_pointer_ty(pointer_ty),
            | ast::Node::StructTy(struct_ty) => self.lower_struct_ty(struct_ty),
            | ast::Node::EnumTy(enum_ty) => self.lower_enum_ty(enum_ty),
            | ast::Node::Constant(binding) => return self.lower_constant(binding),
            | ast::Node::Variable(binding) => return self.lower_variable(binding),
            | ast::Node::WhileLoop(while_loop) => self.lower_while_loop(while_loop),
            | ast::Node::Break(start) => return self.lower_break(start),
            | ast::Node::Continue(start) => return self.lower_continue(start),
            | ast::Node::Defer(_) => todo!(),
            | ast::Node::Block(block) => self.lower_block(block),
            | ast::Node::Return(r#return) => self.lower_return(r#return),
        }?;

        Ok(self.push_instruction(instruction))
    }

    fn lower_optional_node(&mut self, optional_node: Option<ast::NodeIdx>) -> LowererResult<Option<InstIdx>> {
        if let Some(node_idx) = optional_node {
            self.lower_node(node_idx).map(Some)
        } else {
            Ok(None)
        }
    }

    fn lower_nodes(&mut self, nodes: ThinVec<ast::NodeIdx>) -> LowererResult<ThinVec<InstIdx>> {
        let mut lowered = ThinVec::new();

        lowered.reserve(nodes.len());

        for node_idx in nodes {
            lowered.push(self.lower_node(node_idx)?);
        }

        Ok(lowered)
    }

    fn lower_identifier(&mut self, name: &'a str, start: ByteOffset) -> LowererResult<InstIdx> {
        if let Some(local) = self.scope.get(&name) {
            if matches!(self.instructions[local.0 as usize], Inst::Variable(_)) {
                return Ok(self.push_instruction(Inst::Dereference(UnaryOperation { rhs: *local, start })));
            } else {
                return Ok(*local);
            }
        } else if self.scope_backtrack.contains_key(&name) {
            let global = self.push_instruction(Inst::Int(0));

            let bucket = self.scope_backtrack.get_mut(&name).unwrap();

            bucket.push(global);

            return Ok(global);
        }

        let primative = match name {
            | "usize" => Inst::IntTy(IntTy {
                is_signed: false,
                bits: self.bcu.target.pointer_bit_width(),
            }),

            | "ssize" => Inst::IntTy(IntTy {
                is_signed: true,
                bits: self.bcu.target.pointer_bit_width(),
            }),

            | "c_char" => Inst::IntTy(IntTy {
                is_signed: self.bcu.target.is_c_char_signed(),
                bits: self.bcu.target.c_type_bit_width(CType::Char),
            }),

            | "c_uchar" => Inst::IntTy(IntTy {
                is_signed: false,
                bits: self.bcu.target.c_type_bit_width(CType::Char),
            }),

            | "c_schar" => Inst::IntTy(IntTy {
                is_signed: true,
                bits: self.bcu.target.c_type_bit_width(CType::Char),
            }),

            | "c_short" => Inst::IntTy(IntTy {
                is_signed: true,
                bits: self.bcu.target.c_type_bit_width(CType::Short),
            }),

            | "c_ushort" => Inst::IntTy(IntTy {
                is_signed: false,
                bits: self.bcu.target.c_type_bit_width(CType::UShort),
            }),

            | "c_int" => Inst::IntTy(IntTy {
                is_signed: true,
                bits: self.bcu.target.c_type_bit_width(CType::Int),
            }),

            | "c_uint" => Inst::IntTy(IntTy {
                is_signed: false,
                bits: self.bcu.target.c_type_bit_width(CType::UInt),
            }),

            | "c_long" => Inst::IntTy(IntTy {
                is_signed: true,
                bits: self.bcu.target.c_type_bit_width(CType::Long),
            }),

            | "c_ulong" => Inst::IntTy(IntTy {
                is_signed: false,
                bits: self.bcu.target.c_type_bit_width(CType::ULong),
            }),

            | "c_longlong" => Inst::IntTy(IntTy {
                is_signed: true,
                bits: self.bcu.target.c_type_bit_width(CType::LongLong),
            }),

            | "c_ulonglong" => Inst::IntTy(IntTy {
                is_signed: false,
                bits: self.bcu.target.c_type_bit_width(CType::ULongLong),
            }),

            | "c_float" => Inst::FloatTy(FloatTy {
                bits: self.bcu.target.c_type_bit_width(CType::Float),
            }),

            | "c_double" => Inst::FloatTy(FloatTy {
                bits: self.bcu.target.c_type_bit_width(CType::Double),
            }),

            | "c_longdouble" => Inst::FloatTy(FloatTy {
                bits: self.bcu.target.c_type_bit_width(CType::LongDouble),
            }),

            | "true" => Inst::Bool(true),
            | "false" => Inst::Bool(false),

            | "void" => Inst::VoidTy,
            | "bool" => Inst::BoolTy,

            | "f16" => Inst::FloatTy(FloatTy { bits: 16 }),
            | "f32" => Inst::FloatTy(FloatTy { bits: 32 }),
            | "f64" => Inst::FloatTy(FloatTy { bits: 64 }),

            | _ => {
                if name.len() >= 2 && name.starts_with('u') || name.starts_with('s') {
                    let is_signed = name.starts_with('s');
                    let bits = &name[1..];

                    if let Ok(bits) = bits.parse() {
                        Inst::IntTy(IntTy { is_signed, bits })
                    } else {
                        return Err(LowererError::UnexpectedValue(
                            TokenLoc::find(start, self.file),
                            "could not find in scope",
                            name.to_string(),
                        ));
                    }
                } else {
                    return Err(LowererError::UnexpectedValue(
                        TokenLoc::find(start, self.file),
                        "could not find in scope",
                        name.to_string(),
                    ));
                }
            }
        };

        Ok(self.push_instruction(primative))
    }

    fn lower_function(&mut self, function: ast::Function) -> LowererResult<Inst> {
        let signature = self.lower_function_ty_inner(function.signature)?;

        let previous_lowering_loop = self.within_loop;
        self.within_loop = false;

        self.scope.start();

        let mut body = ThinVec::new();

        for &(name, ty) in signature.parameters.iter() {
            let argument = self.push_instruction(Inst::Argument(ty));

            body.push(argument);

            let name = &name.get(&self.file.buffer);

            self.scope.insert(name, argument);
        }

        body.extend(self.lower_nodes(function.body)?);

        self.scope.end();

        self.within_loop = previous_lowering_loop;

        Ok(Inst::Function(Function {
            signature,
            foreign: function.foreign,
            body,
        }))
    }

    fn lower_function_ty(&mut self, function_ty: ast::FunctionTy) -> LowererResult<Inst> {
        self.lower_function_ty_inner(function_ty).map(Inst::FunctionTy)
    }

    fn lower_function_ty_inner(&mut self, function_ty: ast::FunctionTy) -> LowererResult<FunctionTy> {
        let mut parameters = ThinVec::new();

        parameters.reserve(function_ty.parameters.len());

        for (name_range, ty_idx) in function_ty.parameters {
            parameters.push((name_range, self.lower_node(ty_idx)?));
        }

        Ok(FunctionTy {
            parameters,
            is_var_args: function_ty.is_var_args,
            calling_convention: function_ty.calling_convention,
            return_ty: self.lower_optional_node(function_ty.return_ty)?,
            start: function_ty.start,
        })
    }

    fn lower_array_ty(&mut self, array_ty: ast::ArrayTy) -> LowererResult<Inst> {
        Ok(Inst::ArrayTy(ArrayTy {
            len: self.lower_node(array_ty.len)?,
            child_ty: self.lower_node(array_ty.child_ty)?,
            start: array_ty.start,
        }))
    }

    fn lower_pointer_ty(&mut self, pointer_ty: ast::PointerTy) -> LowererResult<Inst> {
        Ok(Inst::PointerTy(PointerTy {
            size: pointer_ty.size,
            is_const: pointer_ty.is_const,
            child_ty: self.lower_node(pointer_ty.child_ty)?,
            start: pointer_ty.start,
        }))
    }

    fn lower_slice(&mut self, slice: ast::Slice) -> LowererResult<Inst> {
        Ok(Inst::Slice(Slice {
            target: self.lower_node(slice.target)?,
            range_start: self.lower_node(slice.range_start)?,
            range_end: self.lower_node(slice.range_end)?,
            start: slice.start,
        }))
    }

    fn lower_struct(&mut self, r#struct: ast::Struct) -> LowererResult<Inst> {
        let mut fields = ThinVec::new();
        let mut fields_set = HashSet::new();

        fields.reserve(r#struct.fields.len());
        fields_set.reserve(r#struct.fields.len());

        for (name_range, value_idx) in r#struct.fields {
            let name_buffer = name_range.get(&self.file.buffer);

            if !fields_set.insert(name_buffer) {
                return Err(LowererError::UnexpectedValue(
                    TokenLoc::find(name_range.start, self.file),
                    "duplicate struct initializaiton field",
                    name_buffer.to_string(),
                ));
            }

            fields.push((name_range, self.lower_node(value_idx)?));
        }

        Ok(Inst::Struct(Struct {
            ty: self.lower_node(r#struct.ty)?,
            fields,
            start: r#struct.start,
        }))
    }

    fn lower_struct_ty(&mut self, struct_ty: ast::StructTy) -> LowererResult<Inst> {
        self.lower_struct_ty_inner(struct_ty).map(Inst::StructTy)
    }

    fn lower_binding(&mut self, binding: &ast::Binding) -> LowererResult<Binding> {
        Ok(Binding {
            ty: self.lower_optional_node(binding.ty)?,
            value: self.lower_node(binding.value)?,
            start: binding.name.start,
        })
    }

    fn lower_global_bindings(
        &mut self,
        constants: ThinVec<ast::Binding>,
        variables: ThinVec<ast::Binding>,
    ) -> LowererResult<ThinVec<InstIdx>> {
        let mut body = ThinVec::new();

        body.reserve(constants.len() + variables.len());

        self.scope.start();
        self.scope_backtrack.start();

        for binding in constants.iter().chain(variables.iter()) {
            let name = binding.name.get(&self.file.buffer);

            self.scope_backtrack.insert(name, ThinVec::new());
        }

        for binding in constants {
            let lowered_constant = self.lower_constant(binding)?;

            body.push(lowered_constant);

            let name = binding.name.get(&self.file.buffer);

            if let Some(bucket) = self.scope_backtrack.remove(&name) {
                for entry in bucket {
                    self.instructions[entry.0 as usize] = Inst::Duplicate(lowered_constant);
                }
            }
        }

        for binding in variables {
            let lowered_variable = self.lower_variable(binding)?;

            body.push(lowered_variable);

            let name = binding.name.get(&self.file.buffer);

            if let Some(bucket) = self.scope_backtrack.remove(&name) {
                for entry in bucket {
                    self.instructions[entry.0 as usize] = Inst::Dereference(UnaryOperation {
                        rhs: lowered_variable,
                        start: binding.name.start,
                    });
                }
            }
        }

        self.scope_backtrack.end();
        self.scope.end();

        Ok(body)
    }

    fn lower_constant(&mut self, binding: ast::Binding) -> LowererResult<InstIdx> {
        let lowered_binding = self.lower_binding(&binding)?;

        let inst_idx = self.push_instruction(Inst::Constant(lowered_binding));

        let name = binding.name.get(&self.file.buffer);

        self.scope.insert(name, inst_idx);

        Ok(inst_idx)
    }

    fn lower_variable(&mut self, binding: ast::Binding) -> LowererResult<InstIdx> {
        let lowered_binding = self.lower_binding(&binding)?;

        let inst_idx = self.push_instruction(Inst::Variable(lowered_binding));

        let name = binding.name.get(&self.file.buffer);

        self.scope.insert(name, inst_idx);

        Ok(inst_idx)
    }

    fn lower_struct_ty_inner(&mut self, struct_ty: ast::StructTy) -> LowererResult<StructTy> {
        let mut fields = ThinVec::new();
        let mut fields_set = HashSet::new();

        fields.reserve(struct_ty.fields.len());
        fields_set.reserve(struct_ty.fields.len());

        for (name_range, ty_idx) in struct_ty.fields {
            let name_buffer = name_range.get(&self.file.buffer);

            if !fields_set.insert(name_buffer) {
                return Err(LowererError::UnexpectedValue(
                    TokenLoc::find(name_range.start, self.file),
                    "duplicate struct type field",
                    name_buffer.to_string(),
                ));
            }

            fields.push((name_range, self.lower_node(ty_idx)?));
        }

        let body = self.lower_global_bindings(struct_ty.constants, struct_ty.variables)?;

        Ok(StructTy {
            fields,
            body,
            start: struct_ty.start,
        })
    }

    fn lower_enum_ty(&mut self, enum_ty: ast::EnumTy) -> LowererResult<Inst> {
        let mut fields = ThinVec::new();
        let mut fields_set = HashSet::new();

        fields.reserve(enum_ty.fields.len());
        fields_set.reserve(enum_ty.fields.len());

        for (name_range, value_idx) in enum_ty.fields {
            let name_buffer = name_range.get(&self.file.buffer);

            if !fields_set.insert(name_buffer) {
                return Err(LowererError::UnexpectedValue(
                    TokenLoc::find(name_range.start, self.file),
                    "duplicate enum type field",
                    name_buffer.to_string(),
                ));
            }

            fields.push((name_range, self.lower_optional_node(value_idx)?));
        }

        let body = self.lower_global_bindings(enum_ty.constants, enum_ty.variables)?;

        Ok(Inst::EnumTy(EnumTy {
            backing_ty: self.lower_optional_node(enum_ty.backing_ty)?,
            fields,
            body,
            start: enum_ty.start,
        }))
    }

    fn lower_array(&mut self, array: ast::Array) -> LowererResult<Inst> {
        Ok(Inst::Array(Array {
            ty: self.lower_node(array.ty)?,
            values: self.lower_nodes(array.values)?,
            start: array.start,
        }))
    }

    fn lower_conditional(&mut self, conditional: ast::Conditional) -> LowererResult<Inst> {
        Ok(Inst::Conditional(Conditional {
            condition: self.lower_node(conditional.condition)?,
            then_case: self.lower_node(conditional.then_case)?,
            else_case: self.lower_optional_node(conditional.else_case)?,
            start: conditional.start,
        }))
    }

    fn lower_switch(&mut self, switch: ast::Switch) -> LowererResult<Inst> {
        let mut cases = ThinVec::new();

        cases.reserve(switch.cases.len());

        for (case_values, case_node_idx) in switch.cases {
            cases.push((self.lower_nodes(case_values)?, self.lower_node(case_node_idx)?));
        }

        Ok(Inst::Switch(Switch {
            value: self.lower_node(switch.value)?,
            cases,
            else_case: self.lower_optional_node(switch.else_case)?,
            start: switch.start,
        }))
    }

    fn lower_assign(&mut self, assign: ast::Assign) -> LowererResult<Inst> {
        let mut operation = BinaryOperation {
            lhs: self.lower_node(assign.target)?,
            rhs: self.lower_node(assign.value)?,
            start: assign.start,
        };

        if let Some(operator) = assign.operator {
            operation.rhs = self.push_instruction(match operator {
                | BinaryOperator::Plus => Inst::Add(operation),
                | BinaryOperator::Minus => Inst::Subtract(operation),
                | BinaryOperator::Multiply => Inst::Multiply(operation),
                | BinaryOperator::Divide => Inst::Divide(operation),
                | BinaryOperator::Modulo => Inst::Modulo(operation),
                | BinaryOperator::LeftShift => Inst::LeftShift(operation),
                | BinaryOperator::RightShift => Inst::RightShift(operation),
                | BinaryOperator::BitAnd => Inst::BitAnd(operation),
                | BinaryOperator::BitOr => Inst::BitOr(operation),
                | BinaryOperator::BitXor => Inst::BitXor(operation),
                | BinaryOperator::Eql => Inst::Eql(operation),
                | BinaryOperator::NotEql => Inst::NotEql(operation),
                | BinaryOperator::LessThan => Inst::LessThan(operation),
                | BinaryOperator::LessOrEql => Inst::LessOrEql(operation),
                | BinaryOperator::GreaterThan => Inst::GreaterThan(operation),
                | BinaryOperator::GreaterOrEql => Inst::GreaterOrEql(operation),
            });
        }

        Ok(Inst::Assign(operation))
    }

    fn lower_builtin_call(&mut self, builtin_call: ast::BuiltinCall) -> LowererResult<Inst> {
        Ok(Inst::BuiltinCall(BuiltinCall {
            kind: builtin_call.kind,
            arguments: self.lower_nodes(builtin_call.arguments)?,
            start: builtin_call.start,
        }))
    }

    fn lower_call(&mut self, call: ast::Call) -> LowererResult<Inst> {
        Ok(Inst::Call(Call {
            callable: self.lower_node(call.callable)?,
            arguments: self.lower_nodes(call.arguments)?,
            start: call.start,
        }))
    }

    fn lower_inline_assembly(&mut self, inline_assembly: ast::InlineAssembly) -> LowererResult<Inst> {
        let mut input_constraints = ThinVec::new();

        input_constraints.reserve(inline_assembly.input_constraints.len());

        for constraint in inline_assembly.input_constraints {
            input_constraints.push(Constraint {
                register: constraint.register,
                value: self.lower_node(constraint.value)?,
            });
        }

        let output_constraint = if let Some(constraint) = inline_assembly.output_constraint {
            Some(Constraint {
                register: constraint.register,
                value: self.lower_node(constraint.value)?,
            })
        } else {
            None
        };

        Ok(Inst::InlineAssembly(InlineAssembly {
            content: inline_assembly.content,
            input_constraints,
            output_constraint,
            clobbers: inline_assembly.clobbers,
            start: inline_assembly.start,
        }))
    }

    fn lower_unary_operation(
        &mut self,
        unary_operation: ast::UnaryOperation,
    ) -> LowererResult<UnaryOperation> {
        Ok(UnaryOperation {
            rhs: self.lower_node(unary_operation.rhs)?,
            start: unary_operation.start,
        })
    }

    fn lower_binary_operation(
        &mut self,
        binary_operation: ast::BinaryOperation,
    ) -> LowererResult<BinaryOperation> {
        Ok(BinaryOperation {
            lhs: self.lower_node(binary_operation.lhs)?,
            rhs: self.lower_node(binary_operation.rhs)?,
            start: binary_operation.start,
        })
    }

    fn lower_field_access(&mut self, field_access: ast::FieldAccess) -> LowererResult<Inst> {
        Ok(Inst::FieldAccess(FieldAccess {
            target: self.lower_node(field_access.target)?,
            field: field_access.field,
        }))
    }

    fn lower_while_loop(&mut self, while_loop: ast::WhileLoop) -> LowererResult<Inst> {
        let previous_lowering_loop = self.within_loop;
        self.within_loop = true;

        let header = Inst::Conditional(Conditional {
            condition: self.lower_node(while_loop.condition)?,
            then_case: self.lower_node(while_loop.body)?,
            else_case: Some(self.break_inst_idx),
            start: while_loop.start,
        });

        self.within_loop = previous_lowering_loop;

        Ok(Inst::Loop(self.push_instruction(header)))
    }

    fn lower_break(&mut self, start: ByteOffset) -> LowererResult<InstIdx> {
        if !self.within_loop {
            return Err(LowererError::UnexpectedNode(
                TokenLoc::find(start, self.file),
                "break outside a loop",
            ));
        }

        Ok(self.break_inst_idx)
    }

    fn lower_continue(&mut self, start: ByteOffset) -> LowererResult<InstIdx> {
        if !self.within_loop {
            return Err(LowererError::UnexpectedNode(
                TokenLoc::find(start, self.file),
                "continue outside a loop",
            ));
        }

        Ok(self.continue_inst_idx)
    }

    fn lower_block(&mut self, nodes: ThinVec<ast::NodeIdx>) -> LowererResult<Inst> {
        self.scope.start();

        let instruction = self.lower_nodes(nodes).map(Inst::Block);

        self.scope.end();

        instruction
    }

    fn lower_return(&mut self, r#return: ast::Return) -> LowererResult<Inst> {
        Ok(Inst::Return(Return {
            value: self.lower_optional_node(r#return.value)?,
            start: r#return.start,
        }))
    }
}
