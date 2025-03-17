const std = @import("std");
const root = @import("root");

const c = @cImport({
    @cInclude("llvm-c/Core.h");
    @cInclude("llvm-c/Target.h");
    @cInclude("llvm-c/TargetMachine.h");
    @cInclude("llvm-c/Transforms/PassBuilder.h");
});

const Compilation = @import("../Compilation.zig");
const Air = @import("../Air.zig");
const Range = @import("../Range.zig");
const Symbol = @import("../Symbol.zig");
const Type = Symbol.Type;

const LlvmBackend = @This();

allocator: std.mem.Allocator,

compilation: *Compilation,

air: Air,

context: c.LLVMContextRef,
module: c.LLVMModuleRef,
builder: c.LLVMBuilderRef,

function_value: c.LLVMValueRef = undefined,
function_type: Type.Function = undefined,

strings: std.StringHashMapUnmanaged(c.LLVMValueRef) = .{},

stack: std.ArrayListUnmanaged(Register) = .{},

scope: *Scope = undefined,

pub const Error = std.mem.Allocator.Error;

const Scope = Symbol.Scope(Variable);

const Variable = struct {
    type_id: u32,
    pointer: c.LLVMValueRef,
};

const Register = struct {
    type_id: u32,
    value: c.LLVMValueRef,
};

pub fn init(allocator: std.mem.Allocator, compilation: *Compilation, air: Air) Error!LlvmBackend {
    const context = c.LLVMContextCreate();
    const module = c.LLVMModuleCreateWithNameInContext(try allocator.dupeZ(u8, compilation.root_file.path), context);
    const builder = c.LLVMCreateBuilderInContext(context);

    return LlvmBackend{
        .allocator = allocator,
        .compilation = compilation,
        .air = air,
        .context = context,
        .module = module,
        .builder = builder,
    };
}

pub fn deinit(self: *LlvmBackend) void {
    self.strings.deinit(self.allocator);
    self.stack.deinit(self.allocator);

    c.LLVMDisposeModule(self.module);
    c.LLVMDisposeBuilder(self.builder);
    c.LLVMContextDispose(self.context);
    c.LLVMShutdown();
}

pub fn emit(
    self: *LlvmBackend,
    output_file_path: [:0]const u8,
    output_kind: root.OutputKind,
    code_model: root.CodeModel,
    optimization_mode: root.OptimizationMode,
) Error!void {
    c.LLVMSetModuleInlineAsm2(self.module, self.air.global_assembly.items.ptr, self.air.global_assembly.items.len);

    var global_scope: Scope = .{};
    self.scope = &global_scope;

    try self.scope.ensureUnusedCapacity(self.allocator, @intCast(self.air.variables.count() + self.air.functions.count()));

    for (self.air.variables.keys(), self.air.variables.values()) |variable_name, variable| {
        const variable_name_z = try self.allocator.dupeZ(u8, variable_name);
        defer self.allocator.free(variable_name_z);

        const variable_type = self.compilation.getTypeFromId(variable.type_id);

        const llvm_type = try self.llvmType(variable_type);

        const llvm_pointer = c.LLVMAddGlobal(self.module, llvm_type, variable_name_z.ptr);

        if (variable.initializer) |initializer| {
            try self.emitInstruction(initializer);

            var initializer_register = self.stack.pop();

            try self.unaryImplicitCast(&initializer_register, variable.type_id);

            _ = c.LLVMSetInitializer(llvm_pointer, initializer_register.value);
        } else {
            _ = c.LLVMSetInitializer(llvm_pointer, c.LLVMGetUndef(llvm_type));
        }

        self.scope.putAssumeCapacity(variable_name, .{
            .type_id = variable.type_id,
            .pointer = llvm_pointer,
        });
    }

    for (self.air.functions.keys(), self.air.functions.values()) |function_name, function| {
        const function_name_z = try self.allocator.dupeZ(u8, function_name);
        defer self.allocator.free(function_name_z);

        const function_type = self.compilation.getTypeFromId(function.type_id);

        const llvm_type = try self.llvmType(function_type);

        const llvm_pointer = c.LLVMAddFunction(self.module, function_name_z.ptr, llvm_type);

        switch (function_type.function.calling_convention) {
            .auto => {
                c.LLVMSetFunctionCallConv(llvm_pointer, c.LLVMFastCallConv);
                c.LLVMSetLinkage(llvm_pointer, c.LLVMInternalLinkage);
            },
            .c => c.LLVMSetFunctionCallConv(llvm_pointer, c.LLVMCCallConv),
            .@"inline" => {
                c.LLVMAddAttributeAtIndex(
                    llvm_pointer,
                    @bitCast(c.LLVMAttributeFunctionIndex),
                    c.LLVMCreateEnumAttribute(self.context, c.LLVMGetEnumAttributeKindForName("alwaysinline", 12), 1),
                );

                c.LLVMSetLinkage(llvm_pointer, c.LLVMPrivateLinkage);
            },
            .naked => c.LLVMAddAttributeAtIndex(
                llvm_pointer,
                @bitCast(c.LLVMAttributeFunctionIndex),
                c.LLVMCreateEnumAttribute(self.context, c.LLVMGetEnumAttributeKindForName("naked", 5), 1),
            ),
        }

        self.scope.putAssumeCapacity(function_name, .{
            .type_id = function.type_id,
            .pointer = llvm_pointer,
        });
    }

    for (self.air.functions.keys(), self.air.functions.values()) |function_name, function| {
        const llvm_pointer = self.scope.get(function_name).?.pointer;

        const function_type = self.compilation.getTypeFromId(function.type_id);

        self.function_value = llvm_pointer;
        self.function_type = function_type.function;

        if (function.body_block) |body_block| {
            const llvm_entry_block = c.LLVMAppendBasicBlockInContext(self.context, self.function_value, "");

            c.LLVMPositionBuilderAtEnd(self.builder, llvm_entry_block);

            _ = try self.emitBlock(body_block);
        }
    }

    c.LLVMInitializeAllTargetInfos();
    c.LLVMInitializeAllTargets();
    c.LLVMInitializeAllTargetMCs();
    c.LLVMInitializeAllAsmParsers();
    c.LLVMInitializeAllAsmPrinters();

    const target = self.compilation.env.target;

    const target_triple = try llvmTargetTripleZ(self.allocator, target);
    defer self.allocator.free(target_triple);

    _ = c.LLVMSetTarget(self.module, target_triple);

    var llvm_target: c.LLVMTargetRef = undefined;

    _ = c.LLVMGetTargetFromTriple(target_triple, &llvm_target, null);

    const llvm_cpu_name = target.cpu.model.llvm_name orelse "generic";

    const llvm_cpu_features = try llvmCpuFeaturesZ(self.allocator, target);
    defer self.allocator.free(llvm_cpu_features);

    const target_machine = c.LLVMCreateTargetMachine(
        llvm_target,
        target_triple,
        llvm_cpu_name,
        llvm_cpu_features,
        switch (optimization_mode) {
            .debug => c.LLVMCodeGenLevelDefault,
            .release => c.LLVMCodeGenLevelAggressive,
        },
        c.LLVMRelocPIC,
        switch (code_model) {
            .default => c.LLVMCodeModelDefault,
            .tiny => c.LLVMCodeModelTiny,
            .small => c.LLVMCodeModelSmall,
            .kernel => c.LLVMCodeModelKernel,
            .medium => c.LLVMCodeModelMedium,
            .large => c.LLVMCodeModelLarge,
        },
    );

    const pass_builder_options = c.LLVMCreatePassBuilderOptions();

    c.LLVMPassBuilderOptionsSetLoopUnrolling(pass_builder_options, 1);
    c.LLVMPassBuilderOptionsSetLoopVectorization(pass_builder_options, 1);
    c.LLVMPassBuilderOptionsSetLoopInterleaving(pass_builder_options, 1);
    c.LLVMPassBuilderOptionsSetMergeFunctions(pass_builder_options, 1);
    c.LLVMPassBuilderOptionsSetCallGraphProfile(pass_builder_options, 1);

    const debug_optimization_passes = "function(ee-instrument<>),always-inline,coro-cond(coro-early,cgscc(coro-split),coro-cleanup,globaldce),function(annotation-remarks)";
    const release_optimization_passes = "annotation2metadata,forceattrs,inferattrs,coro-early,function<eager-inv>(ee-instrument<>,lower-expect,simplifycfg<bonus-inst-threshold=1;no-forward-switch-cond;no-switch-range-to-icmp;no-switch-to-lookup;keep-loops;no-hoist-common-insts;no-sink-common-insts;speculate-blocks;simplify-cond-branch;no-speculate-unpredictables>,early-cse<>,callsite-splitting),openmp-opt,ipsccp,called-value-propagation,globalopt,function<eager-inv>(mem2reg,simplifycfg<bonus-inst-threshold=1;no-forward-switch-cond;switch-range-to-icmp;no-switch-to-lookup;keep-loops;no-hoist-common-insts;no-sink-common-insts;speculate-blocks;simplify-cond-branch;no-speculate-unpredictables>),always-inline,require<globals-aa>,function(invalidate<aa>),require<profile-summary>,cgscc(devirt<4>(inline,function-attrs<skip-non-recursive-function-attrs>,argpromotion,openmp-opt-cgscc,function<eager-inv;no-rerun>(early-cse<memssa>,speculative-execution<only-if-divergent-target>,jump-threading,correlated-propagation,simplifycfg<bonus-inst-threshold=1;no-forward-switch-cond;switch-range-to-icmp;no-switch-to-lookup;keep-loops;no-hoist-common-insts;no-sink-common-insts;speculate-blocks;simplify-cond-branch;no-speculate-unpredictables>,libcalls-shrinkwrap,tailcallelim,simplifycfg<bonus-inst-threshold=1;no-forward-switch-cond;switch-range-to-icmp;no-switch-to-lookup;keep-loops;no-hoist-common-insts;no-sink-common-insts;speculate-blocks;simplify-cond-branch;no-speculate-unpredictables>,reassociate,constraint-elimination,loop-mssa(loop-instsimplify,loop-simplifycfg,licm<no-allowspeculation>,loop-rotate<header-duplication;no-prepare-for-lto>,licm<allowspeculation>,simple-loop-unswitch<nontrivial;trivial>),simplifycfg<bonus-inst-threshold=1;no-forward-switch-cond;switch-range-to-icmp;no-switch-to-lookup;keep-loops;no-hoist-common-insts;no-sink-common-insts;speculate-blocks;simplify-cond-branch;no-speculate-unpredictables>,loop(loop-idiom,indvars,simple-loop-unswitch<nontrivial;trivial>,loop-deletion,loop-unroll-full),vector-combine,mldst-motion<no-split-footer-bb>,gvn<>,sccp,bdce,jump-threading,correlated-propagation,adce,memcpyopt,dse,move-auto-init,loop-mssa(licm<allowspeculation>),coro-elide,simplifycfg<bonus-inst-threshold=1;no-forward-switch-cond;switch-range-to-icmp;no-switch-to-lookup;keep-loops;hoist-common-insts;sink-common-insts;speculate-blocks;simplify-cond-branch;no-speculate-unpredictables>),function-attrs,function(require<should-not-run-function-passes>),coro-split)),deadargelim,coro-cleanup,globalopt,globaldce,elim-avail-extern,rpo-function-attrs,recompute-globalsaa,function<eager-inv>(float2int,lower-constant-intrinsics,chr,loop(loop-rotate<header-duplication;no-prepare-for-lto>,loop-deletion),loop-distribute,inject-tli-mappings,loop-vectorize<no-interleave-forced-only;no-vectorize-forced-only;>,infer-alignment,loop-load-elim,simplifycfg<bonus-inst-threshold=1;forward-switch-cond;switch-range-to-icmp;switch-to-lookup;no-keep-loops;hoist-common-insts;sink-common-insts;speculate-blocks;simplify-cond-branch;no-speculate-unpredictables>,slp-vectorizer,vector-combine,loop-unroll<O3>,transform-warning,infer-alignment,loop-mssa(licm<allowspeculation>),alignment-from-assumptions,loop-sink,instsimplify,div-rem-pairs,tailcallelim,simplifycfg<bonus-inst-threshold=1;no-forward-switch-cond;switch-range-to-icmp;no-switch-to-lookup;keep-loops;no-hoist-common-insts;no-sink-common-insts;speculate-blocks;simplify-cond-branch;speculate-unpredictables>),globaldce,constmerge,cg-profile,rel-lookup-table-converter,function(annotation-remarks)";

    _ = c.LLVMRunPasses(
        self.module,
        switch (optimization_mode) {
            .debug => debug_optimization_passes,
            .release => release_optimization_passes,
        },
        target_machine,
        pass_builder_options,
    );

    c.LLVMDisposePassBuilderOptions(pass_builder_options);

    _ = c.LLVMTargetMachineEmitToFile(
        target_machine,
        self.module,
        output_file_path,
        switch (output_kind) {
            .object, .executable => c.LLVMObjectFile,
            .assembly => c.LLVMAssemblyFile,
            .none => unreachable,
        },
        null,
    );
}

fn emitInstruction(self: *LlvmBackend, air_instruction: Air.Instruction) Error!void {
    switch (air_instruction) {
        .duplicate => try self.stack.append(self.allocator, self.stack.getLast()),
        .reverse => |count| std.mem.reverse(Register, self.stack.items[self.stack.items.len - count ..]),
        .pop => _ = self.stack.pop(),

        .string => |string| try self.emitString(string),
        .int => |int| try self.emitInt(int),
        .float => |float| try self.emitFloat(float),
        .boolean => |boolean| try self.emitBoolean(boolean),

        .negate => try self.emitNegate(),

        .bool_not, .bit_not => try self.emitNot(),

        .bit_and => try self.emitBitwiseArithmetic(.bit_and),
        .bit_or => try self.emitBitwiseArithmetic(.bit_or),
        .bit_xor => try self.emitBitwiseArithmetic(.bit_xor),

        .write => try self.emitWrite(),
        .read => try self.emitRead(),

        .add => try self.emitArithmetic(.add),
        .sub => try self.emitArithmetic(.sub),
        .mul => try self.emitArithmetic(.mul),
        .div => try self.emitArithmetic(.div),
        .rem => try self.emitArithmetic(.rem),

        .lt => try self.emitComparison(.lt),
        .gt => try self.emitComparison(.gt),
        .eql => try self.emitComparison(.eql),

        .shl => try self.emitBitwiseShift(.left),
        .shr => try self.emitBitwiseShift(.right),

        .cast => |to_id| try self.emitCast(to_id),

        .inline_assembly => |inline_assembly| try self.emitInlineAssembly(inline_assembly),

        .call => |arguments_count| try self.emitCall(arguments_count),

        .parameters => |names| try self.emitParameters(names),

        .variable => |variable| try self.emitVariable(variable),

        .get_variable_ptr => |name| try self.emitGetVariablePtr(name),
        .get_element_ptr => try self.emitGetElementPtr(),
        .get_field_ptr => |field_index| try self.emitGetFieldPtr(field_index),

        .slice => try self.emitSlice(),

        .block => |block| try self.emitBlock(block),
        .loop => |loop| try self.emitLoop(loop),
        .@"break" => try self.emitBreak(),
        .@"continue" => try self.emitContinue(),
        .conditional => |conditional| try self.emitConditional(conditional),
        .@"switch" => |@"switch"| try self.emitSwitch(@"switch"),

        .ret => try self.emitReturn(true),
        .ret_void => try self.emitReturn(false),
    }
}

fn emitString(self: *LlvmBackend, range: Range) Error!void {
    const string = self.compilation.getStringFromRange(range);

    const string_type_id = try self.compilation.putType(try self.compilation.makeStringType(string.len));

    if (self.strings.get(string)) |string_pointer| {
        try self.stack.append(self.allocator, .{ .value = string_pointer, .type_id = string_type_id });
    } else {
        const string_pointer =
            c.LLVMBuildGlobalStringPtr(
            self.builder,
            try self.allocator.dupeZ(u8, string),
            "",
        );

        try self.strings.put(self.allocator, string, string_pointer);

        try self.stack.append(self.allocator, .{ .value = string_pointer, .type_id = string_type_id });
    }
}

fn emitInt(self: *LlvmBackend, id: u32) Error!void {
    const int = self.compilation.getIntFromId(id);

    const int_type = Type.intFittingRange(int, int);

    const int_repr: c_ulonglong = @truncate(@as(u128, @bitCast(int)));

    const llvm_int_type = try self.llvmType(int_type);
    const llvm_int_value = c.LLVMConstInt(llvm_int_type, int_repr, @intFromBool(int < 0));

    try self.stack.append(self.allocator, .{ .type_id = try self.compilation.putType(int_type), .value = llvm_int_value });
}

fn emitFloat(self: *LlvmBackend, float: f64) Error!void {
    const float_type = Type.floatFittingRange(float, float);

    const llvm_float_type = try self.llvmType(float_type);
    const llvm_float_value = c.LLVMConstReal(llvm_float_type, float);

    try self.stack.append(self.allocator, .{ .type_id = try self.compilation.putType(float_type), .value = llvm_float_value });
}

fn emitBoolean(self: *LlvmBackend, boolean: bool) Error!void {
    try self.stack.append(self.allocator, .{
        .type_id = try self.compilation.putType(.bool),
        .value = c.LLVMConstInt(try self.llvmType(.bool), @intFromBool(boolean), 0),
    });
}

fn emitNegate(self: *LlvmBackend) Error!void {
    const rhs = self.stack.pop();

    try self.stack.append(self.allocator, .{
        .type_id = rhs.type_id,
        .value = if (self.compilation.getTypeFromId(rhs.type_id) == .int)
            c.LLVMBuildNeg(self.builder, rhs.value, "")
        else
            c.LLVMBuildFNeg(self.builder, rhs.value, ""),
    });
}

fn emitNot(self: *LlvmBackend) Error!void {
    const rhs = self.stack.pop();

    try self.stack.append(self.allocator, .{
        .type_id = rhs.type_id,
        .value = c.LLVMBuildNot(self.builder, rhs.value, ""),
    });
}

const BitwiseArithmeticOperation = enum {
    bit_and,
    bit_or,
    bit_xor,
};

fn emitBitwiseArithmetic(self: *LlvmBackend, comptime operation: BitwiseArithmeticOperation) Error!void {
    const rhs = self.stack.pop();
    const lhs = self.stack.pop();

    try self.stack.append(
        self.allocator,
        .{
            .type_id = lhs.type_id,
            .value = switch (operation) {
                .bit_and => c.LLVMBuildAnd(self.builder, lhs.value, rhs.value, ""),
                .bit_or => c.LLVMBuildOr(self.builder, lhs.value, rhs.value, ""),
                .bit_xor => c.LLVMBuildXor(self.builder, lhs.value, rhs.value, ""),
            },
        },
    );
}

fn emitWrite(self: *LlvmBackend) Error!void {
    const pointer = self.stack.pop();

    const pointer_type = self.compilation.getTypeFromId(pointer.type_id).pointer;
    const element_type_id = pointer_type.child_type_id;

    var register = self.stack.pop();

    try self.unaryImplicitCast(&register, element_type_id);

    _ = c.LLVMBuildStore(self.builder, register.value, pointer.value);
}

fn emitRead(self: *LlvmBackend) Error!void {
    const pointer = self.stack.pop();

    const pointer_type = self.compilation.getTypeFromId(pointer.type_id).pointer;
    const element_type_id = pointer_type.child_type_id;
    const element_type = self.compilation.getTypeFromId(element_type_id);

    const llvm_value = c.LLVMBuildLoad2(
        self.builder,
        try self.llvmType(element_type),
        pointer.value,
        "",
    );

    try self.stack.append(self.allocator, .{
        .type_id = element_type_id,
        .value = llvm_value,
    });
}

fn emitGetElementPtr(self: *LlvmBackend) Error!void {
    var index = self.stack.pop();

    const usize_type: Type = .{ .int = .{ .signedness = .unsigned, .bits = self.compilation.env.target.ptrBitWidth() } };
    const usize_type_id = try self.compilation.putType(usize_type);

    try self.unaryImplicitCast(&index, usize_type_id);

    const array_pointer = self.stack.pop();
    const array_pointer_type = self.compilation.getTypeFromId(array_pointer.type_id).pointer;

    const child_type_id = if (array_pointer_type.size == .one)
        self.compilation.getTypeFromId(array_pointer_type.child_type_id).array.child_type_id
    else
        array_pointer_type.child_type_id;

    const child_type = self.compilation.getTypeFromId(child_type_id);

    const pointer_type_id = try self.compilation.putType(.{
        .pointer = .{
            .size = .one,
            .is_const = array_pointer_type.is_const,
            .child_type_id = child_type_id,
        },
    });

    const child_llvm_type = try self.llvmType(child_type);

    try self.stack.append(
        self.allocator,
        .{
            .type_id = pointer_type_id,

            .value = c.LLVMBuildGEP2(
                self.builder,
                child_llvm_type,
                array_pointer.value,
                &index.value,
                1,
                "",
            ),
        },
    );
}

fn emitGetFieldPtr(self: *LlvmBackend, field_index: u32) Error!void {
    const container = self.stack.pop();
    const container_type = self.compilation.getTypeFromId(self.compilation.getTypeFromId(container.type_id).pointer.child_type_id);
    const llvm_container_type = try self.llvmType(container_type);

    var field_type_id: u32 = undefined;

    switch (container_type) {
        .pointer => |pointer_type| {
            std.debug.assert(pointer_type.size == .slice);

            switch (field_index) {
                0 => {
                    field_type_id = try self.compilation.putType(.{
                        .int = .{
                            .signedness = .unsigned,
                            .bits = self.compilation.env.target.ptrBitWidth(),
                        },
                    });
                },

                1 => {
                    field_type_id = try self.compilation.putType(.{
                        .pointer = .{
                            .size = .many,
                            .is_const = pointer_type.is_const,
                            .child_type_id = pointer_type.child_type_id,
                        },
                    });
                },

                else => unreachable,
            }
        },

        .@"struct" => |struct_type| {
            field_type_id = struct_type.fields[field_index].type_id;
        },

        else => unreachable,
    }

    try self.stack.append(
        self.allocator,
        .{
            .type_id = try self.compilation.putType(.{
                .pointer = .{
                    .size = .one,
                    .is_const = false,
                    .child_type_id = field_type_id,
                },
            }),

            .value = c.LLVMBuildStructGEP2(
                self.builder,
                llvm_container_type,
                container.value,
                @intCast(field_index),
                "",
            ),
        },
    );
}

fn emitSlice(self: *LlvmBackend) Error!void {
    var end = self.stack.pop();
    var start = self.stack.pop();

    const usize_type: Type = .{ .int = .{ .signedness = .unsigned, .bits = self.compilation.env.target.ptrBitWidth() } };
    const usize_type_id = try self.compilation.putType(usize_type);

    try self.unaryImplicitCast(&end, usize_type_id);
    try self.unaryImplicitCast(&start, usize_type_id);

    const array_pointer = self.stack.pop();
    const array_pointer_type = self.compilation.getTypeFromId(array_pointer.type_id).pointer;

    const child_type_id = if (array_pointer_type.size == .one)
        self.compilation.getTypeFromId(array_pointer_type.child_type_id).array.child_type_id
    else
        array_pointer_type.child_type_id;

    const child_type = self.compilation.getTypeFromId(child_type_id);

    const child_llvm_type = try self.llvmType(child_type);

    const slice_type: Type = .{
        .pointer = .{
            .size = .slice,
            .is_const = array_pointer_type.is_const,
            .child_type_id = child_type_id,
        },
    };

    const slice_type_id = try self.compilation.putType(slice_type);

    try self.stack.append(self.allocator, .{
        .type_id = slice_type_id,

        .value = try self.makeSlice(
            try self.llvmType(slice_type),
            c.LLVMBuildGEP2(
                self.builder,
                child_llvm_type,
                array_pointer.value,
                &start.value,
                1,
                "",
            ),
            c.LLVMBuildSub(self.builder, end.value, start.value, ""),
        ),
    });
}

const ArithmeticOperation = enum {
    add,
    sub,
    mul,
    div,
    rem,
};

fn emitArithmetic(self: *LlvmBackend, comptime operation: ArithmeticOperation) Error!void {
    var rhs = self.stack.pop();
    var lhs = self.stack.pop();

    const lhs_type = self.compilation.getTypeFromId(lhs.type_id);
    const rhs_type = self.compilation.getTypeFromId(rhs.type_id);

    const usize_type: Type = .{ .int = .{ .signedness = .unsigned, .bits = self.compilation.env.target.ptrBitWidth() } };

    if (lhs_type == .pointer and rhs_type != .pointer) {
        rhs.value = try self.saneIntCast(rhs, usize_type);
        rhs.type_id = try self.compilation.putType(usize_type);
    } else if (rhs_type == .pointer and lhs_type != .pointer) {
        lhs.value = try self.saneIntCast(lhs, usize_type);
        lhs.type_id = try self.compilation.putType(usize_type);
    }

    if (lhs_type == .int or lhs_type == .pointer) {
        try self.stack.append(
            self.allocator,
            .{
                .type_id = lhs.type_id,

                .value = switch (operation) {
                    .add => c.LLVMBuildAdd(self.builder, lhs.value, rhs.value, ""),
                    .sub => c.LLVMBuildSub(self.builder, lhs.value, rhs.value, ""),
                    .mul => c.LLVMBuildMul(self.builder, lhs.value, rhs.value, ""),
                    .div => if (lhs_type.canBeNegative())
                        c.LLVMBuildSDiv(self.builder, lhs.value, rhs.value, "")
                    else
                        c.LLVMBuildUDiv(self.builder, lhs.value, rhs.value, ""),
                    .rem => if (lhs_type.canBeNegative())
                        c.LLVMBuildSRem(self.builder, lhs.value, rhs.value, "")
                    else
                        c.LLVMBuildURem(self.builder, lhs.value, rhs.value, ""),
                },
            },
        );
    } else {
        try self.stack.append(
            self.allocator,
            .{
                .type_id = lhs.type_id,

                .value = switch (operation) {
                    .add => c.LLVMBuildFAdd(self.builder, lhs.value, rhs.value, ""),
                    .sub => c.LLVMBuildFSub(self.builder, lhs.value, rhs.value, ""),
                    .mul => c.LLVMBuildFMul(self.builder, lhs.value, rhs.value, ""),
                    .div => c.LLVMBuildFDiv(self.builder, lhs.value, rhs.value, ""),
                    .rem => c.LLVMBuildFRem(self.builder, lhs.value, rhs.value, ""),
                },
            },
        );
    }
}

const ComparisonOperation = enum {
    lt,
    gt,
    eql,
};

fn emitComparison(self: *LlvmBackend, comptime operation: ComparisonOperation) Error!void {
    const rhs = self.stack.pop();
    const lhs = self.stack.pop();

    const lhs_type = self.compilation.getTypeFromId(lhs.type_id);

    if (lhs_type == .int) {
        try self.stack.append(
            self.allocator,
            .{
                .type_id = try self.compilation.putType(.bool),

                .value = switch (operation) {
                    .lt => c.LLVMBuildICmp(self.builder, if (lhs_type.canBeNegative()) c.LLVMIntSLT else c.LLVMIntULT, lhs.value, rhs.value, ""),
                    .gt => c.LLVMBuildICmp(self.builder, if (lhs_type.canBeNegative()) c.LLVMIntSGT else c.LLVMIntUGT, lhs.value, rhs.value, ""),
                    .eql => c.LLVMBuildICmp(self.builder, c.LLVMIntEQ, lhs.value, rhs.value, ""),
                },
            },
        );
    } else {
        try self.stack.append(
            self.allocator,
            .{
                .type_id = try self.compilation.putType(.bool),

                .value = switch (operation) {
                    .lt => c.LLVMBuildFCmp(self.builder, c.LLVMRealOLT, lhs.value, rhs.value, ""),
                    .gt => c.LLVMBuildFCmp(self.builder, c.LLVMRealOGT, lhs.value, rhs.value, ""),
                    .eql => c.LLVMBuildFCmp(self.builder, c.LLVMRealOEQ, lhs.value, rhs.value, ""),
                },
            },
        );
    }
}

const BitwiseShiftDirection = enum {
    left,
    right,
};

fn emitBitwiseShift(self: *LlvmBackend, comptime direction: BitwiseShiftDirection) Error!void {
    var rhs = self.stack.pop();
    const lhs = self.stack.pop();

    const lhs_type = self.compilation.getTypeFromId(lhs.type_id);

    const count_type: Type = .{ .int = .{ .signedness = .unsigned, .bits = std.math.log2(lhs_type.int.bits) } };
    const count_type_id = try self.compilation.putType(count_type);

    try self.unaryImplicitCast(&rhs, count_type_id);

    try self.stack.append(
        self.allocator,
        .{
            .type_id = lhs.type_id,

            .value = switch (direction) {
                .left => c.LLVMBuildShl(self.builder, lhs.value, rhs.value, ""),
                .right => c.LLVMBuildAShr(self.builder, lhs.value, rhs.value, ""),
            },
        },
    );
}

fn emitCast(self: *LlvmBackend, to_id: u32) Error!void {
    const lhs = self.stack.pop();

    const value = lhs.value;

    const from_type = self.compilation.getTypeFromId(lhs.type_id);
    const to_type = self.compilation.getTypeFromId(to_id);

    const llvm_to_type = try self.llvmType(to_type);

    try self.stack.append(
        self.allocator,
        .{
            .type_id = to_id,

            .value = switch (from_type) {
                .int => switch (to_type) {
                    .int => try self.saneIntCast(lhs, to_type),

                    .float => if (from_type.canBeNegative())
                        c.LLVMBuildSIToFP(self.builder, value, llvm_to_type, "")
                    else
                        c.LLVMBuildUIToFP(self.builder, value, llvm_to_type, ""),

                    .pointer => c.LLVMBuildIntToPtr(self.builder, value, llvm_to_type, ""),

                    else => unreachable,
                },

                .@"enum" => switch (to_type) {
                    .int => try self.saneIntCast(lhs, to_type),

                    .float => if (from_type.canBeNegative())
                        c.LLVMBuildSIToFP(self.builder, value, llvm_to_type, "")
                    else
                        c.LLVMBuildUIToFP(self.builder, value, llvm_to_type, ""),

                    .pointer => c.LLVMBuildIntToPtr(self.builder, value, llvm_to_type, ""),

                    else => unreachable,
                },

                .float => switch (to_type) {
                    .int => if (to_type.canBeNegative())
                        c.LLVMBuildFPToSI(self.builder, value, llvm_to_type, "")
                    else
                        c.LLVMBuildFPToUI(self.builder, value, llvm_to_type, ""),

                    .float => c.LLVMBuildFPCast(self.builder, value, llvm_to_type, ""),

                    else => unreachable,
                },

                .pointer => switch (to_type) {
                    .int => c.LLVMBuildPtrToInt(self.builder, value, llvm_to_type, ""),
                    .pointer => c.LLVMBuildPointerCast(self.builder, value, llvm_to_type, ""),

                    else => unreachable,
                },

                .bool => c.LLVMBuildZExt(self.builder, value, llvm_to_type, ""),

                .void => unreachable,
                .type => unreachable,
                .function => unreachable,
                .@"struct" => unreachable,
                .array => unreachable,
            },
        },
    );
}

fn emitInlineAssembly(self: *LlvmBackend, assembly: Air.Instruction.InlineAssembly) Error!void {
    const assembly_inputs = try self.allocator.alloc(c.LLVMValueRef, assembly.input_constraints.len);

    var assembly_constraints: std.ArrayListUnmanaged(u8) = .{};

    if (assembly.output_constraint) |output_constraint| {
        try assembly_constraints.appendSlice(self.allocator, self.compilation.getStringFromRange(output_constraint.register));
        if (assembly.input_constraints.len != 0) try assembly_constraints.append(self.allocator, ',');
    }

    for (assembly.input_constraints, 0..) |register, i| {
        assembly_inputs[i] = self.stack.pop().value;

        try assembly_constraints.appendSlice(self.allocator, self.compilation.getStringFromRange(register));
        if (assembly.clobbers.len != 0 or i != assembly.input_constraints.len - 1) try assembly_constraints.append(self.allocator, ',');
    }

    for (assembly.clobbers, 0..) |clobber, i| {
        try assembly_constraints.writer(self.allocator).print("~{{{s}}}", .{self.compilation.getStringFromRange(clobber)});
        if (i != assembly.clobbers.len - 1) try assembly_constraints.append(self.allocator, ',');
    }

    if (self.compilation.env.target.cpu.arch.isX86()) {
        if (assembly_constraints.items.len != 0) try assembly_constraints.append(self.allocator, ',');
        try assembly_constraints.appendSlice(self.allocator, "~{dirflag},~{fpsr},~{flags}");
    }

    const assembly_parameter_types = try self.allocator.alloc(c.LLVMTypeRef, assembly.input_constraints.len);

    for (assembly_inputs, 0..) |input, i| {
        assembly_parameter_types[i] = c.LLVMTypeOf(input);
    }

    const assmebly_return_type = if (assembly.output_constraint) |output_constraint|
        try self.llvmType(self.compilation.getTypeFromId(output_constraint.type_id))
    else
        c.LLVMVoidTypeInContext(self.context);

    const assembly_function_type = c.LLVMFunctionType(assmebly_return_type, assembly_parameter_types.ptr, @intCast(assembly_parameter_types.len), 0);

    const assembly_function = c.LLVMGetInlineAsm(
        assembly_function_type,
        try self.allocator.dupeZ(u8, assembly.content),
        assembly.content.len,
        assembly_constraints.items.ptr,
        assembly_constraints.items.len,
        1,
        0,
        c.LLVMInlineAsmDialectATT,
        0,
    );

    const assembly_output = c.LLVMBuildCall2(
        self.builder,
        assembly_function_type,
        assembly_function,
        assembly_inputs.ptr,
        @intCast(assembly_inputs.len),
        "",
    );

    if (assembly.output_constraint) |assembly_output_constraint| {
        try self.stack.append(self.allocator, .{ .value = assembly_output, .type_id = assembly_output_constraint.type_id });
    }
}

fn emitCall(self: *LlvmBackend, arguments_count: usize) Error!void {
    const function_pointer = self.stack.pop();

    const function_type = self.compilation.getTypeFromId(self.compilation.getTypeFromId(function_pointer.type_id).pointer.child_type_id).function;
    const function_return_type = self.compilation.getTypeFromId(function_type.return_type_id);

    const arguments = try self.allocator.alloc(c.LLVMValueRef, arguments_count);

    for (0..arguments_count) |i| {
        var argument = self.stack.pop();

        if (i < function_type.parameter_type_ids.len) {
            try self.unaryImplicitCast(&argument, function_type.parameter_type_ids[i]);
        }

        arguments[i] = argument.value;
    }

    const call = c.LLVMBuildCall2(
        self.builder,
        try self.llvmType(.{ .function = function_type }),
        function_pointer.value,
        arguments.ptr,
        @intCast(arguments_count),
        "",
    );

    if (function_return_type != .void) {
        try self.stack.append(self.allocator, .{ .type_id = function_type.return_type_id, .value = call });
    }
}

fn emitParameters(self: *LlvmBackend, names: [][]const u8) Error!void {
    try self.scope.ensureTotalCapacity(self.allocator, @intCast(names.len));

    for (names, 0..) |name, i| {
        const parameter_type_id = self.function_type.parameter_type_ids[i];

        const parameter_type = self.compilation.getTypeFromId(parameter_type_id);

        const llvm_type = try self.llvmType(parameter_type);

        const llvm_pointer = c.LLVMBuildAlloca(self.builder, llvm_type, "");

        _ = c.LLVMBuildStore(self.builder, c.LLVMGetParam(self.function_value, @intCast(i)), llvm_pointer);

        self.scope.putAssumeCapacity(name, .{
            .type_id = parameter_type_id,
            .pointer = llvm_pointer,
        });
    }

    self.allocator.free(names);
}

fn emitVariable(self: *LlvmBackend, variable: struct { []const u8, u32 }) Error!void {
    const name, const type_id = variable;

    const @"type" = self.compilation.getTypeFromId(type_id);

    const llvm_type = try self.llvmType(@"type");

    const current_block = c.LLVMGetInsertBlock(self.builder);
    const first_block = c.LLVMGetFirstBasicBlock(self.function_value);
    const first_instruction = c.LLVMGetFirstInstruction(first_block);

    if (first_instruction != null)
        c.LLVMPositionBuilderBefore(self.builder, first_instruction)
    else
        c.LLVMPositionBuilderAtEnd(self.builder, first_block);

    const llvm_pointer = c.LLVMBuildAlloca(self.builder, llvm_type, "");

    c.LLVMPositionBuilderAtEnd(self.builder, current_block);

    try self.scope.put(self.allocator, name, .{
        .type_id = type_id,
        .pointer = llvm_pointer,
    });
}

fn emitGetVariablePtr(self: *LlvmBackend, name: []const u8) Error!void {
    const variable = self.scope.get(name).?;

    try self.stack.append(
        self.allocator,
        .{
            .type_id = try self.compilation.putType(.{
                .pointer = .{
                    .size = .one,
                    .is_const = false,
                    .child_type_id = variable.type_id,
                },
            }),

            .value = variable.pointer,
        },
    );
}

fn emitBlock(self: *LlvmBackend, id: u32) Error!void {
    var scope: Scope = .{ .maybe_parent = self.scope };
    self.scope = &scope;

    const instructions = self.air.blocks.items[id].items;

    for (instructions) |instruction|
        try self.emitInstruction(instruction);

    self.scope = self.scope.maybe_parent.?;
}

var llvm_loop_condition_block: c.LLVMBasicBlockRef = null;
var llvm_loop_continue_block: c.LLVMBasicBlockRef = null;

fn emitLoop(self: *LlvmBackend, loop: Air.Instruction.Loop) Error!void {
    const llvm_header_block = c.LLVMGetInsertBlock(self.builder);
    if (c.LLVMGetBasicBlockTerminator(llvm_header_block) != null) return;

    const previous_llvm_loop_continue_block = llvm_loop_continue_block;
    defer llvm_loop_continue_block = previous_llvm_loop_continue_block;

    const previous_llvm_loop_condition_block = llvm_loop_condition_block;
    defer llvm_loop_condition_block = previous_llvm_loop_condition_block;

    llvm_loop_condition_block = c.LLVMAppendBasicBlockInContext(self.context, self.function_value, "");
    const llvm_body_block = c.LLVMAppendBasicBlockInContext(self.context, self.function_value, "");
    llvm_loop_continue_block = c.LLVMAppendBasicBlockInContext(self.context, self.function_value, "");

    _ = c.LLVMBuildBr(self.builder, llvm_loop_condition_block);

    c.LLVMPositionBuilderAtEnd(self.builder, llvm_loop_condition_block);

    try self.emitBlock(loop.condition_block);

    const condition_value = self.stack.pop().value;

    _ = c.LLVMBuildCondBr(
        self.builder,
        condition_value,
        llvm_body_block,
        llvm_loop_continue_block,
    );

    c.LLVMPositionBuilderAtEnd(self.builder, llvm_body_block);

    try self.emitBlock(loop.body_block);

    if (c.LLVMGetBasicBlockTerminator(c.LLVMGetInsertBlock(self.builder)) == null) {
        _ = c.LLVMBuildBr(self.builder, llvm_loop_condition_block);
    }

    c.LLVMPositionBuilderAtEnd(self.builder, llvm_loop_continue_block);
}

fn emitBreak(self: *LlvmBackend) Error!void {
    if (c.LLVMGetBasicBlockTerminator(c.LLVMGetInsertBlock(self.builder)) == null) {
        _ = c.LLVMBuildBr(self.builder, llvm_loop_continue_block);
    }
}

fn emitContinue(self: *LlvmBackend) Error!void {
    if (c.LLVMGetBasicBlockTerminator(c.LLVMGetInsertBlock(self.builder)) == null) {
        _ = c.LLVMBuildBr(self.builder, llvm_loop_condition_block);
    }
}

fn emitConditional(self: *LlvmBackend, conditional: Air.Instruction.Conditional) Error!void {
    const llvm_header_block = c.LLVMGetInsertBlock(self.builder);
    if (c.LLVMGetBasicBlockTerminator(llvm_header_block) != null) return;

    const condition_value = self.stack.pop().value;

    const llvm_then_block = c.LLVMAppendBasicBlockInContext(self.context, self.function_value, "");
    const llvm_else_block = c.LLVMAppendBasicBlockInContext(self.context, self.function_value, "");
    const llvm_continue_block = c.LLVMAppendBasicBlockInContext(self.context, self.function_value, "");

    _ = c.LLVMBuildCondBr(
        self.builder,
        condition_value,
        llvm_then_block,
        llvm_else_block,
    );

    c.LLVMPositionBuilderAtEnd(self.builder, llvm_then_block);

    const previous_stack = self.stack;

    self.stack = .{};

    try self.emitBlock(conditional.then_block);

    const maybe_then_value = self.stack.popOrNull();

    if (c.LLVMGetBasicBlockTerminator(c.LLVMGetInsertBlock(self.builder)) == null) {
        _ = c.LLVMBuildBr(self.builder, llvm_continue_block);
    }

    c.LLVMPositionBuilderAtEnd(self.builder, llvm_else_block);

    self.stack = .{};

    if (conditional.else_block) |else_block|
        try self.emitBlock(else_block);

    const maybe_else_value = self.stack.popOrNull();

    self.stack = previous_stack;

    if (c.LLVMGetBasicBlockTerminator(c.LLVMGetInsertBlock(self.builder)) == null) {
        _ = c.LLVMBuildBr(self.builder, llvm_continue_block);
    }

    c.LLVMPositionBuilderAtEnd(self.builder, llvm_continue_block);

    if (maybe_then_value) |then_value| {
        const llvm_result_type = try self.llvmType(self.compilation.getTypeFromId(then_value.type_id));

        const llvm_result_phi = c.LLVMBuildPhi(self.builder, llvm_result_type, "");

        var llvm_incoming_values: [2]c.LLVMValueRef = .{
            then_value.value,
            maybe_else_value.?.value,
        };

        var llvm_incoming_blocks: [2]c.LLVMBasicBlockRef = .{
            llvm_then_block,
            llvm_else_block,
        };

        c.LLVMAddIncoming(llvm_result_phi, &llvm_incoming_values, &llvm_incoming_blocks, 2);

        try self.stack.append(self.allocator, .{
            .type_id = then_value.type_id,
            .value = llvm_result_phi,
        });
    }
}

fn emitSwitch(self: *LlvmBackend, @"switch": Air.Instruction.Switch) Error!void {
    const llvm_header_block = c.LLVMGetInsertBlock(self.builder);
    if (c.LLVMGetBasicBlockTerminator(llvm_header_block) != null) return;

    const switched_on_register = self.stack.pop();

    const llvm_else_block = c.LLVMAppendBasicBlockInContext(self.context, self.function_value, "");
    const llvm_continue_block = c.LLVMCreateBasicBlockInContext(self.context, "");

    const switch_instruction = c.LLVMBuildSwitch(
        self.builder,
        switched_on_register.value,
        llvm_else_block,
        @intCast(@"switch".case_blocks.len),
    );

    var llvm_incoming_phi_entries: std.MultiArrayList(struct {
        value: c.LLVMValueRef,
        block: c.LLVMBasicBlockRef,
    }) = .{};

    defer llvm_incoming_phi_entries.deinit(self.allocator);

    var maybe_result_type_id: ?u32 = null;

    {
        const previous_stack = self.stack;

        self.stack = .{};

        c.LLVMPositionBuilderAtEnd(self.builder, llvm_else_block);

        try self.emitBlock(@"switch".else_block);

        if (self.stack.popOrNull()) |else_value| {
            try llvm_incoming_phi_entries.append(self.allocator, .{
                .value = else_value.value,
                .block = llvm_else_block,
            });

            maybe_result_type_id = else_value.type_id;
        }

        self.stack = previous_stack;
    }

    if (c.LLVMGetBasicBlockTerminator(c.LLVMGetInsertBlock(self.builder)) == null) {
        _ = c.LLVMBuildBr(self.builder, llvm_continue_block);
    }

    for (@"switch".case_blocks) |case_block| {
        var case_value = self.stack.pop();

        try self.unaryImplicitCast(&case_value, switched_on_register.type_id);

        const llvm_case_block = c.LLVMAppendBasicBlockInContext(self.context, self.function_value, "");

        const previous_stack = self.stack;

        self.stack = .{};

        c.LLVMPositionBuilderAtEnd(self.builder, llvm_case_block);

        try self.emitBlock(case_block);

        if (self.stack.popOrNull()) |case_result_value| {
            try llvm_incoming_phi_entries.append(self.allocator, .{
                .value = case_result_value.value,
                .block = llvm_case_block,
            });
        }

        self.stack = previous_stack;

        if (c.LLVMGetBasicBlockTerminator(c.LLVMGetInsertBlock(self.builder)) == null) {
            _ = c.LLVMBuildBr(self.builder, llvm_continue_block);
        }

        c.LLVMAddCase(switch_instruction, case_value.value, llvm_case_block);
    }

    c.LLVMAppendExistingBasicBlock(self.function_value, llvm_continue_block);

    c.LLVMPositionBuilderAtEnd(self.builder, llvm_continue_block);

    if (maybe_result_type_id) |result_type_id| {
        const result_type = self.compilation.getTypeFromId(result_type_id);

        const llvm_return_type = try self.llvmType(result_type);

        const llvm_result_phi = c.LLVMBuildPhi(self.builder, llvm_return_type, "");

        c.LLVMAddIncoming(
            llvm_result_phi,
            llvm_incoming_phi_entries.items(.value).ptr,
            llvm_incoming_phi_entries.items(.block).ptr,
            @intCast(llvm_incoming_phi_entries.len),
        );

        try self.stack.append(self.allocator, .{
            .type_id = result_type_id,
            .value = llvm_result_phi,
        });
    }
}

fn emitReturn(self: *LlvmBackend, comptime with_value: bool) Error!void {
    if (c.LLVMGetBasicBlockTerminator(c.LLVMGetInsertBlock(self.builder)) != null) return;

    if (with_value) {
        var return_register = self.stack.pop();

        try self.unaryImplicitCast(&return_register, self.function_type.return_type_id);

        _ = c.LLVMBuildRet(self.builder, return_register.value);
    } else {
        _ = c.LLVMBuildRetVoid(self.builder);
    }
}

fn subArchName(features: std.Target.Cpu.Feature.Set, arch: anytype, mappings: anytype) ?[]const u8 {
    inline for (mappings) |mapping| {
        if (arch.featureSetHas(features, mapping[0])) return mapping[1];
    }

    return null;
}

pub fn llvmTargetTripleZ(allocator: std.mem.Allocator, target: std.Target) ![:0]u8 {
    var llvm_triple = std.ArrayList(u8).init(allocator);
    defer llvm_triple.deinit();

    const features = target.cpu.features;

    const llvm_arch = switch (target.cpu.arch) {
        .arm => "arm",
        .armeb => "armeb",
        .aarch64 => if (target.abi == .ilp32) "aarch64_32" else "aarch64",
        .aarch64_be => "aarch64_be",
        .arc => "arc",
        .avr => "avr",
        .bpfel => "bpfel",
        .bpfeb => "bpfeb",
        .csky => "csky",
        .hexagon => "hexagon",
        .loongarch32 => "loongarch32",
        .loongarch64 => "loongarch64",
        .m68k => "m68k",
        // MIPS sub-architectures are a bit irregular, so we handle them manually here
        .mips => if (std.Target.mips.featureSetHas(features, .mips32r6)) "mipsisa32r6" else "mips",
        .mipsel => if (std.Target.mips.featureSetHas(features, .mips32r6)) "mipsisa32r6el" else "mipsel",
        .mips64 => if (std.Target.mips.featureSetHas(features, .mips64r6)) "mipsisa64r6" else "mips64",
        .mips64el => if (std.Target.mips.featureSetHas(features, .mips64r6)) "mipsisa64r6el" else "mips64el",
        .msp430 => "msp430",
        .powerpc => "powerpc",
        .powerpcle => "powerpcle",
        .powerpc64 => "powerpc64",
        .powerpc64le => "powerpc64le",
        .amdgcn => "amdgcn",
        .riscv32 => "riscv32",
        .riscv64 => "riscv64",
        .sparc => "sparc",
        .sparc64 => "sparc64",
        .s390x => "s390x",
        .thumb => "thumb",
        .thumbeb => "thumbeb",
        .x86 => "i386",
        .x86_64 => "x86_64",
        .xcore => "xcore",
        .xtensa => "xtensa",
        .nvptx => "nvptx",
        .nvptx64 => "nvptx64",
        .spirv => "spirv",
        .spirv32 => "spirv32",
        .spirv64 => "spirv64",
        .lanai => "lanai",
        .wasm32 => "wasm32",
        .wasm64 => "wasm64",
        .ve => "ve",

        else => unreachable,
    };

    try llvm_triple.appendSlice(llvm_arch);

    const llvm_sub_arch: ?[]const u8 = switch (target.cpu.arch) {
        .arm, .armeb, .thumb, .thumbeb => subArchName(features, std.Target.arm, .{
            .{ .v4t, "v4t" },
            .{ .v5t, "v5t" },
            .{ .v5te, "v5te" },
            .{ .v5tej, "v5tej" },
            .{ .v6, "v6" },
            .{ .v6k, "v6k" },
            .{ .v6kz, "v6kz" },
            .{ .v6m, "v6m" },
            .{ .v6t2, "v6t2" },
            .{ .v7a, "v7a" },
            .{ .v7em, "v7em" },
            .{ .v7m, "v7m" },
            .{ .v7r, "v7r" },
            .{ .v7ve, "v7ve" },
            .{ .v8a, "v8a" },
            .{ .v8_1a, "v8.1a" },
            .{ .v8_2a, "v8.2a" },
            .{ .v8_3a, "v8.3a" },
            .{ .v8_4a, "v8.4a" },
            .{ .v8_5a, "v8.5a" },
            .{ .v8_6a, "v8.6a" },
            .{ .v8_7a, "v8.7a" },
            .{ .v8_8a, "v8.8a" },
            .{ .v8_9a, "v8.9a" },
            .{ .v8m, "v8m.base" },
            .{ .v8m_main, "v8m.main" },
            .{ .v8_1m_main, "v8.1m.main" },
            .{ .v8r, "v8r" },
            .{ .v9a, "v9a" },
            .{ .v9_1a, "v9.1a" },
            .{ .v9_2a, "v9.2a" },
            .{ .v9_3a, "v9.3a" },
            .{ .v9_4a, "v9.4a" },
            .{ .v9_5a, "v9.5a" },
        }),
        .powerpc => subArchName(features, std.Target.powerpc, .{
            .{ .spe, "spe" },
        }),
        .spirv => subArchName(features, std.Target.spirv, .{
            .{ .v1_5, "1.5" },
        }),
        .spirv32, .spirv64 => subArchName(features, std.Target.spirv, .{
            .{ .v1_5, "1.5" },
            .{ .v1_4, "1.4" },
            .{ .v1_3, "1.3" },
            .{ .v1_2, "1.2" },
            .{ .v1_1, "1.1" },
        }),
        else => null,
    };

    if (llvm_sub_arch) |sub| try llvm_triple.appendSlice(sub);

    // Unlike CPU backends, GPU backends actually care about the vendor tag
    try llvm_triple.appendSlice(switch (target.cpu.arch) {
        .amdgcn => if (target.os.tag == .mesa3d) "-mesa-" else "-amd-",
        .nvptx, .nvptx64 => "-nvidia-",
        .spirv64 => if (target.os.tag == .amdhsa) "-amd-" else "-unknown-",
        else => "-unknown-",
    });

    const llvm_os = switch (target.os.tag) {
        .freestanding => "unknown",
        .dragonfly => "dragonfly",
        .freebsd => "freebsd",
        .fuchsia => "fuchsia",
        .linux => "linux",
        .ps3 => "lv2",
        .netbsd => "netbsd",
        .openbsd => "openbsd",
        .solaris, .illumos => "solaris",
        .windows, .uefi => "windows",
        .zos => "zos",
        .haiku => "haiku",
        .rtems => "rtems",
        .aix => "aix",
        .cuda => "cuda",
        .nvcl => "nvcl",
        .amdhsa => "amdhsa",
        .opencl => "unknown", // https://llvm.org/docs/SPIRVUsage.html#target-triples
        .ps4 => "ps4",
        .ps5 => "ps5",
        .elfiamcu => "elfiamcu",
        .mesa3d => "mesa3d",
        .amdpal => "amdpal",
        .hermit => "hermit",
        .hurd => "hurd",
        .wasi => "wasi",
        .emscripten => "emscripten",
        .bridgeos => "bridgeos",
        .macos => "macosx",
        .ios => "ios",
        .tvos => "tvos",
        .watchos => "watchos",
        .driverkit => "driverkit",
        .visionos => "xros",
        .serenity => "serenity",
        .vulkan => "vulkan",

        else => "unknown",
    };
    try llvm_triple.appendSlice(llvm_os);

    if (target.os.tag.isDarwin()) {
        const min_version = target.os.version_range.semver.min;
        try llvm_triple.writer().print("{d}.{d}.{d}", .{
            min_version.major,
            min_version.minor,
            min_version.patch,
        });
    }
    try llvm_triple.append('-');

    const llvm_abi = switch (target.abi) {
        .none, .ilp32 => "unknown",
        .gnu => "gnu",
        .gnuabin32 => "gnuabin32",
        .gnuabi64 => "gnuabi64",
        .gnueabi => "gnueabi",
        .gnueabihf => "gnueabihf",
        .gnuf32 => "gnuf32",
        .gnusf => "gnusf",
        .gnux32 => "gnux32",
        .gnuilp32 => "gnuilp32",
        .code16 => "code16",
        .eabi => "eabi",
        .eabihf => "eabihf",
        .android => "android",
        .androideabi => "androideabi",
        .musl => "musl",
        .muslabin32 => "musl", // Should be "muslabin32" in LLVM 20
        .muslabi64 => "musl", // Should be "muslabi64" in LLVM 20
        .musleabi => "musleabi",
        .musleabihf => "musleabihf",
        .muslx32 => "muslx32",
        .msvc => "msvc",
        .itanium => "itanium",
        .cygnus => "cygnus",
        .simulator => "simulator",
        .macabi => "macabi",
        .ohos => "ohos",
        .ohoseabi => "ohoseabi",
    };
    try llvm_triple.appendSlice(llvm_abi);

    return llvm_triple.toOwnedSliceSentinel(0);
}

fn llvmCpuFeaturesZ(allocator: std.mem.Allocator, target: std.Target) std.mem.Allocator.Error![:0]u8 {
    var llvm_cpu_features = std.ArrayList(u8).init(allocator);

    var llvm_cpu_disabled_features = std.ArrayList(u8).init(allocator);
    defer llvm_cpu_disabled_features.deinit();

    const all_features_list = target.cpu.arch.allFeaturesList();

    for (all_features_list) |feature| {
        if (feature.llvm_name) |llvm_name| {
            const is_enabled = target.cpu.features.isEnabled(feature.index);

            if (is_enabled) {
                try llvm_cpu_features.ensureUnusedCapacity(llvm_name.len + 2);

                llvm_cpu_features.appendAssumeCapacity('+');
                llvm_cpu_features.appendSliceAssumeCapacity(llvm_name);
                llvm_cpu_features.appendAssumeCapacity(',');
            } else {
                try llvm_cpu_disabled_features.ensureUnusedCapacity(llvm_name.len + 2);

                llvm_cpu_disabled_features.appendAssumeCapacity('-');
                llvm_cpu_disabled_features.appendSliceAssumeCapacity(llvm_name);
                llvm_cpu_disabled_features.appendAssumeCapacity(',');
            }
        }
    }

    // Append disabled features after enabled ones so their effect doesn't get overwritten
    try llvm_cpu_features.appendSlice(llvm_cpu_disabled_features.items);

    if (llvm_cpu_features.items.len == 0) {
        try llvm_cpu_features.append(0);
    } else {
        std.debug.assert(llvm_cpu_features.items[llvm_cpu_features.items.len - 1] == ',');
        llvm_cpu_features.items[llvm_cpu_features.items.len - 1] = 0;
    }

    llvm_cpu_features.shrinkAndFree(llvm_cpu_features.items.len);

    return llvm_cpu_features.items[0 .. llvm_cpu_features.items.len - 1 :0];
}

fn llvmType(self: *LlvmBackend, @"type": Type) Error!c.LLVMTypeRef {
    return switch (@"type") {
        .type => unreachable,

        .void => c.LLVMVoidTypeInContext(self.context),
        .bool => c.LLVMIntTypeInContext(self.context, 1),

        .int => |int| c.LLVMIntTypeInContext(self.context, int.bits),

        .float => |float| if (float.bits == 16)
            c.LLVMHalfTypeInContext(self.context)
        else if (float.bits == 32)
            c.LLVMFloatTypeInContext(self.context)
        else
            c.LLVMDoubleTypeInContext(self.context),

        .pointer => |pointer| blk: {
            if (pointer.size == .slice) {
                var element_types: [2]c.LLVMTypeRef = .{
                    c.LLVMIntTypeInContext(self.context, self.compilation.env.target.ptrBitWidth()),
                    c.LLVMPointerTypeInContext(self.context, 0),
                };

                break :blk c.LLVMStructTypeInContext(self.context, &element_types, 2, 0);
            } else {
                break :blk c.LLVMPointerTypeInContext(self.context, 0);
            }
        },

        .array => |array| c.LLVMArrayType2(
            try self.llvmType(self.compilation.getTypeFromId(array.child_type_id)),
            @intCast(self.compilation.getIntFromId(array.len_int_id)),
        ),

        .function => |function| blk: {
            const return_type = self.compilation.getTypeFromId(function.return_type_id);

            const llvm_parameter_types = try self.allocator.alloc(c.LLVMTypeRef, function.parameter_type_ids.len);
            defer self.allocator.free(llvm_parameter_types);

            for (function.parameter_type_ids, 0..) |parameter_type_id, i| {
                const parameter_type = self.compilation.getTypeFromId(parameter_type_id);

                llvm_parameter_types[i] = try self.llvmType(parameter_type);
            }

            const llvm_return_type = try self.llvmType(return_type);

            break :blk c.LLVMFunctionType(
                llvm_return_type,
                llvm_parameter_types.ptr,
                @intCast(llvm_parameter_types.len),
                @intFromBool(function.is_var_args),
            );
        },

        .@"enum" => |@"enum"| self.llvmType(self.compilation.getTypeFromId(@"enum".backing_type_id)),

        .@"struct" => |@"struct"| blk: {
            const element_types = try self.allocator.alloc(c.LLVMTypeRef, @"struct".fields.len);
            defer self.allocator.free(element_types);

            for (@"struct".fields, 0..) |field, i| {
                element_types[i] = try self.llvmType(self.compilation.getTypeFromId(field.type_id));
            }

            break :blk c.LLVMStructTypeInContext(self.context, element_types.ptr, @intCast(element_types.len), 0);
        },
    };
}

/// Use this instead of `c.LLVMBuildIntCast2`
fn saneIntCast(self: *LlvmBackend, lhs: Register, to: Type) Error!c.LLVMValueRef {
    const lhs_type = self.compilation.getTypeFromId(lhs.type_id);

    const lhs_int_type = if (lhs_type == .int)
        lhs_type.int
    else if (lhs_type == .bool)
        Type.Int{ .signedness = .unsigned, .bits = 1 }
    else if (lhs_type == .@"enum")
        self.compilation.getTypeFromId(lhs_type.@"enum".backing_type_id).int
    else
        unreachable;

    const to_int_type = to.int;

    // u16 -> u8 (Regular IntCast)
    // u8 -> s8 (Regular IntCast)
    // s8 -> u16 (Regular IntCast)
    // u8 -> s16 (ZExt)
    // u8 -> u16 (ZExt)
    // s8 -> s16 (SExt)

    if (lhs_int_type.signedness == .unsigned) {
        if (to_int_type.bits > lhs_int_type.bits) {
            return c.LLVMBuildZExt(self.builder, lhs.value, try self.llvmType(to), "");
        } else {
            return c.LLVMBuildIntCast2(self.builder, lhs.value, try self.llvmType(to), @intFromEnum(to_int_type.signedness), "");
        }
    } else {
        if (lhs_int_type.signedness == to_int_type.signedness and to_int_type.bits > lhs_int_type.bits) {
            return c.LLVMBuildSExt(self.builder, lhs.value, try self.llvmType(to), "");
        } else {
            return c.LLVMBuildIntCast2(self.builder, lhs.value, try self.llvmType(to), @intFromEnum(to_int_type.signedness), "");
        }
    }
}

/// Use this instead of `c.LLVMBuildFPCast`
fn saneFloatCast(self: *LlvmBackend, lhs: Register, to: Type) Error!c.LLVMValueRef {
    return c.LLVMBuildFPCast(self.builder, lhs.value, try self.llvmType(to), "");
}

fn makeSlice(self: *LlvmBackend, slice_type: c.LLVMTypeRef, ptr: c.LLVMValueRef, len: c.LLVMValueRef) Error!c.LLVMValueRef {
    if (c.LLVMIsConstant(ptr) == 1 and c.LLVMIsConstant(len) == 1) {
        var slice_values: [2]c.LLVMValueRef = .{ len, ptr };

        return c.LLVMConstStructInContext(self.context, &slice_values, 2, 0);
    }

    const current_block = c.LLVMGetInsertBlock(self.builder);
    const first_block = c.LLVMGetFirstBasicBlock(self.function_value);
    const first_instruction = c.LLVMGetFirstInstruction(first_block);

    if (first_instruction != null)
        c.LLVMPositionBuilderBefore(self.builder, first_instruction)
    else
        c.LLVMPositionBuilderAtEnd(self.builder, first_block);

    const slice_pointer = c.LLVMBuildAlloca(self.builder, slice_type, "");

    c.LLVMPositionBuilderAtEnd(self.builder, current_block);

    const len_in_slice = c.LLVMBuildStructGEP2(self.builder, slice_type, slice_pointer, 0, "");

    _ = c.LLVMBuildStore(self.builder, len, len_in_slice);

    const ptr_in_slice = c.LLVMBuildStructGEP2(self.builder, slice_type, slice_pointer, 1, "");

    _ = c.LLVMBuildStore(self.builder, ptr, ptr_in_slice);

    return c.LLVMBuildLoad2(self.builder, slice_type, slice_pointer, "");
}

fn unaryImplicitCast(self: *LlvmBackend, lhs: *Register, to_type_id: u32) Error!void {
    const lhs_type = self.compilation.getTypeFromId(lhs.type_id);
    const to_type = self.compilation.getTypeFromId(to_type_id);

    if (lhs_type.eql(self.compilation.*, to_type)) return;

    // var x u8 = 4;
    // var y f32 = 4.0;
    // var z u16 = x;

    if (to_type == .int) {
        lhs.value = try self.saneIntCast(lhs.*, to_type);
    } else if (to_type == .float) {
        lhs.value = try self.saneFloatCast(lhs.*, to_type);
    } else if (to_type == .pointer and to_type.pointer.size == .slice) {
        const len = self.compilation.getIntFromId(self.compilation.getTypeFromId(lhs_type.pointer.child_type_id).array.len_int_id);

        const llvm_usize_type = c.LLVMIntTypeInContext(self.context, self.compilation.env.target.ptrBitWidth());
        const llvm_len_value = c.LLVMConstInt(llvm_usize_type, @intCast(len), 0);

        lhs.value = try self.makeSlice(try self.llvmType(to_type), lhs.value, llvm_len_value);
    }

    lhs.type_id = to_type_id;
}
