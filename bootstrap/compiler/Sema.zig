//! Semantic Analyzer.
//!
//! An analyzer that lowers down `Sir` to `Air` while checking if the instructions and the types are valid.

const std = @import("std");
const root = @import("root");

const Sir = @import("Sir.zig");
const Range = @import("Range.zig");
const Name = Sir.Name;
const SourceLoc = Sir.SourceLoc;
const Compilation = @import("Compilation.zig");
const Air = @import("Air.zig");
const Symbol = @import("Symbol.zig");
const Type = Symbol.Type;

const Sema = @This();

allocator: std.mem.Allocator,

compilation: *Compilation,

module: *Compilation.Pool.Module,
module_id: u32,

air: *Air,
air_instructions: std.ArrayListUnmanaged(Air.Instruction) = .{},

stack: std.ArrayListUnmanaged(Value) = .{},

scope: *Scope,
allocated_scopes: std.ArrayListUnmanaged(Scope) = .{},

container_name: ?[]const u8 = null,

function_type: Type.Function = undefined,

error_info: ?ErrorInfo = null,

pub const ErrorInfo = struct {
    message: []const u8,
    source_loc: SourceLoc,
};

pub const Error = error{ WithMessage, WithoutMessage } || std.mem.Allocator.Error;

pub const Scope = Symbol.Scope(Sema.Variable);

const Value = union(enum(u8)) {
    string: Range,
    int: u32,
    float: f64,
    boolean: bool,
    function: u32,
    module_id: u32,
    type_id: u32,
    runtime: u32,
};

fn getTypeIdFromValue(self: Sema, value: Value) std.mem.Allocator.Error!u32 {
    return switch (value) {
        .runtime => |id| id,
        else => try self.compilation.putType(try self.getTypeFromValue(value)),
    };
}

fn getTypeFromValue(self: Sema, value: Value) std.mem.Allocator.Error!Type {
    return switch (value) {
        .boolean => .bool,
        .module_id, .type_id => .type,
        .string => |range| try self.compilation.makeStringType(range.end - range.start),
        .int => |id| blk: {
            const int = self.compilation.getIntFromId(id);
            break :blk Type.intFittingRange(int, int);
        },
        .float => |float| Type.floatFittingRange(float, float),
        .function => |id| self.compilation.getTypeFromId(self.air.functions.values()[id].type_id),
        .runtime => |id| self.compilation.getTypeFromId(id),
    };
}

const Variable = struct {
    is_const: bool = false,
    air_name: []const u8,
    value: Value,
};

pub fn init(allocator: std.mem.Allocator, compilation: *Compilation, module_id: u32, air: *Air) Error!Sema {
    const module = &compilation.pool.modules.values()[module_id];

    var sema: Sema = .{
        .allocator = allocator,
        .compilation = compilation,
        .module = module,
        .module_id = module_id,
        .air = air,
        .scope = module.scope,
    };

    try sema.putBuiltinConstants();

    return sema;
}

pub fn deinit(self: *Sema) void {
    self.stack.deinit(self.allocator);
    self.allocated_scopes.deinit(self.allocator);
}

fn putBuiltinConstants(self: *Sema) std.mem.Allocator.Error!void {
    try self.scope.ensureUnusedCapacity(self.allocator, 256);
    try self.compilation.ensureTypesUnusedCapacity(256);

    const c_char_bits = self.compilation.env.target.cTypeBitSize(.char);

    {
        const c_char_type: Type = .{
            .int = .{
                .signedness = if (self.compilation.env.target.charSignedness() == .signed) .signed else .unsigned,
                .bits = c_char_bits,
            },
        };

        inline for (.{ "void", "bool", "c_char" }, .{ .void, .bool, c_char_type }) |name, @"type"| {
            self.scope.putAssumeCapacity(name, .{
                .air_name = name,
                .is_const = true,
                .value = .{ .type_id = self.compilation.putTypeAssumeCapacity(@"type") },
            });
        }
    }

    {
        const c_short_bits = self.compilation.env.target.cTypeBitSize(.short);
        const c_ushort_bits = self.compilation.env.target.cTypeBitSize(.ushort);
        const c_int_bits = self.compilation.env.target.cTypeBitSize(.int);
        const c_uint_bits = self.compilation.env.target.cTypeBitSize(.uint);
        const c_long_bits = self.compilation.env.target.cTypeBitSize(.long);
        const c_ulong_bits = self.compilation.env.target.cTypeBitSize(.ulong);
        const c_longlong_bits = self.compilation.env.target.cTypeBitSize(.longlong);
        const c_ulonglong_bits = self.compilation.env.target.cTypeBitSize(.ulonglong);
        const ptr_bits = self.compilation.env.target.ptrBitWidth();

        inline for (.{ "c_uchar", "c_ushort", "c_uint", "c_ulong", "c_ulonglong", "usize" }, .{ c_char_bits, c_ushort_bits, c_uint_bits, c_ulong_bits, c_ulonglong_bits, ptr_bits }) |name, bits| {
            self.scope.putAssumeCapacity(name, .{
                .air_name = name,
                .is_const = true,
                .value = .{
                    .type_id = self.compilation.putTypeAssumeCapacity(.{
                        .int = .{
                            .signedness = .unsigned,
                            .bits = @intCast(bits),
                        },
                    }),
                },
            });
        }

        inline for (.{ "c_schar", "c_short", "c_int", "c_long", "c_longlong", "ssize" }, .{ c_char_bits, c_short_bits, c_int_bits, c_long_bits, c_longlong_bits, ptr_bits }) |name, bits| {
            self.scope.putAssumeCapacity(name, .{
                .air_name = name,
                .is_const = true,
                .value = .{
                    .type_id = self.compilation.putTypeAssumeCapacity(.{
                        .int = .{
                            .signedness = .signed,
                            .bits = @intCast(bits),
                        },
                    }),
                },
            });
        }
    }

    // TODO: Find a better way, this is very verbose and doesn't scale well for bigger arbitrary sized integer types
    {
        const unsigned_int_names = [_][]const u8{ "u0", "u1", "u2", "u3", "u4", "u5", "u6", "u7", "u8", "u9", "u10", "u11", "u12", "u13", "u14", "u15", "u16", "u17", "u18", "u19", "u20", "u21", "u22", "u23", "u24", "u25", "u26", "u27", "u28", "u29", "u30", "u31", "u32", "u33", "u34", "u35", "u36", "u37", "u38", "u39", "u40", "u41", "u42", "u43", "u44", "u45", "u46", "u47", "u48", "u49", "u50", "u51", "u52", "u53", "u54", "u55", "u56", "u57", "u58", "u59", "u60", "u61", "u62", "u63", "u64" };

        for (unsigned_int_names, 0..) |name, bits| {
            self.scope.putAssumeCapacity(name, .{
                .air_name = name,
                .is_const = true,
                .value = .{
                    .type_id = self.compilation.putTypeAssumeCapacity(.{
                        .int = .{
                            .signedness = .unsigned,
                            .bits = @intCast(bits),
                        },
                    }),
                },
            });
        }

        const signed_int_names = [_][]const u8{ "s0", "s1", "s2", "s3", "s4", "s5", "s6", "s7", "s8", "s9", "s10", "s11", "s12", "s13", "s14", "s15", "s16", "s17", "s18", "s19", "s20", "s21", "s22", "s23", "s24", "s25", "s26", "s27", "s28", "s29", "s30", "s31", "s32", "s33", "s34", "s35", "s36", "s37", "s38", "s39", "s40", "s41", "s42", "s43", "s44", "s45", "s46", "s47", "s48", "s49", "s50", "s51", "s52", "s53", "s54", "s55", "s56", "s57", "s58", "s59", "s60", "s61", "s62", "s63", "s64" };

        for (signed_int_names, 0..) |name, bits| {
            self.scope.putAssumeCapacity(name, .{
                .air_name = name,
                .is_const = true,
                .value = .{
                    .type_id = self.compilation.putTypeAssumeCapacity(.{
                        .int = .{
                            .signedness = .signed,
                            .bits = @intCast(bits),
                        },
                    }),
                },
            });
        }
    }

    {
        const c_float_bits = self.compilation.env.target.cTypeBitSize(.float);
        const c_double_bits = self.compilation.env.target.cTypeBitSize(.double);
        // TODO: Type `c_longdouble` requires `f80` and `f128` to be supported.

        inline for (.{ "f16", "f32", "f64", "c_float", "c_double" }, .{ 16, 32, 64, c_float_bits, c_double_bits }) |name, bits| {
            self.scope.putAssumeCapacity(name, .{
                .air_name = name,
                .is_const = true,
                .value = .{ .type_id = self.compilation.putTypeAssumeCapacity(.{ .float = .{ .bits = @intCast(bits) } }) },
            });
        }
    }

    {
        inline for (.{ "true", "false" }, .{ true, false }) |name, value| {
            self.scope.putAssumeCapacity(name, .{
                .air_name = name,
                .is_const = true,
                .value = .{ .boolean = value },
            });
        }
    }
}

pub fn hoist(self: *Sema, sir: Sir) std.mem.Allocator.Error!void {
    try self.air.global_assembly.appendSlice(self.allocator, sir.global_assembly.items);

    try self.scope.ensureUnusedCapacity(self.allocator, @intCast(sir.constants.count() + sir.variables.count()));
    try self.compilation.pool.lazy_units.ensureUnusedCapacity(self.allocator, @intCast(sir.constants.count() + sir.variables.count()));

    for (sir.constants.keys(), sir.constants.values()) |constant_name, constant| {
        const constant_air_name = try std.fmt.allocPrint(self.allocator, "{s}.{s}", .{ self.module.file.path, constant_name });

        self.scope.putAssumeCapacity(constant_name, .{
            .is_const = true,
            .air_name = constant_air_name,
            .value = undefined,
        });

        self.compilation.pool.lazy_units.putAssumeCapacity(constant_air_name, .{
            .owner_id = self.module_id,
            .token_start = constant.token_start,
            .type_instructions = &.{},
            .value_instructions = sir.instructions.items[constant.value_instructions.start..constant.value_instructions.end],
        });
    }

    for (sir.variables.keys(), sir.variables.values()) |variable_name, variable| {
        const variable_air_name = try std.fmt.allocPrint(self.allocator, "{s}.{s}", .{ self.module.file.path, variable_name });

        self.scope.putAssumeCapacity(variable_name, .{
            .air_name = variable_air_name,
            .value = undefined,
        });

        self.compilation.pool.lazy_units.putAssumeCapacity(variable_air_name, .{
            .owner_id = self.module_id,
            .token_start = variable.token_start,
            .type_instructions = sir.instructions.items[variable.type_instructions.start..variable.type_instructions.end],
            .value_instructions = sir.instructions.items[variable.value_instructions.start..variable.value_instructions.end],
        });
    }
}

pub fn analyze(self: *Sema, sir: Sir) Error!void {
    try self.hoist(sir);

    for (sir.constants.keys()) |constant_name| {
        var variable = self.scope.get(constant_name).?;

        if (self.compilation.pool.lazy_units.get(variable.air_name)) |lazy_unit| {
            try self.analyzeLazyUnit(&variable, lazy_unit);
            self.scope.getPtr(constant_name).?.* = variable;

            _ = self.compilation.pool.lazy_units.remove(variable.air_name);
        }
    }

    for (sir.variables.keys()) |variable_name| {
        var variable = self.scope.get(variable_name).?;

        if (self.compilation.pool.lazy_units.get(variable.air_name)) |lazy_unit| {
            try self.analyzeLazyUnit(&variable, lazy_unit);
            self.scope.getPtr(variable_name).?.* = variable;

            _ = self.compilation.pool.lazy_units.remove(variable.air_name);
        }
    }
}

fn popType(self: *Sema, token_start: u32) Error!u32 {
    const value = self.stack.pop();

    if (value == .module_id) {
        self.error_info = .{
            .message = "modules are special values which should not be used as types",
            .source_loc = SourceLoc.find(self.module.file.buffer, token_start),
        };

        return error.WithMessage;
    } else if (value != .type_id) {
        const value_type = try self.getTypeFromValue(value);

        var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

        try error_message_buf.appendSlice(self.allocator, "expected a type, got value of type '");
        try value_type.format(self.compilation.*, error_message_buf.writer(self.allocator));
        try error_message_buf.append(self.allocator, '\'');

        self.error_info = .{ .message = error_message_buf.items, .source_loc = SourceLoc.find(self.module.file.buffer, token_start) };

        return error.WithMessage;
    } else if (self.compilation.getTypeFromId(value.type_id) == .@"enum") {
        return self.compilation.getTypeFromId(value.type_id).@"enum".backing_type_id;
    }

    return value.type_id;
}

fn analyzeLazyUnit(self: *Sema, variable: *Variable, lazy_unit: Compilation.Pool.LazyUnit) Error!void {
    if (lazy_unit.owner_id == self.module_id) {
        const previous_scope = self.scope;
        defer self.scope = previous_scope;

        while (self.scope.maybe_parent) |parent| self.scope = parent;

        const previous_air_instructions = self.air_instructions;

        defer {
            self.air_instructions.deinit(self.allocator);
            self.air_instructions = previous_air_instructions;
        }

        self.air_instructions = .{};

        if (variable.is_const) {
            for (lazy_unit.value_instructions) |instruction| {
                try self.analyzeInstruction(instruction);
            }

            const value = self.stack.pop();

            if (value == .runtime) {
                self.error_info = .{
                    .message = "expected the constant value to be compile time known",
                    .source_loc = SourceLoc.find(self.module.file.buffer, lazy_unit.token_start),
                };

                return error.WithMessage;
            }

            variable.value = value;
        } else {
            for (lazy_unit.value_instructions) |instruction| {
                try self.analyzeInstruction(instruction);
            }

            const maybe_value = if (lazy_unit.value_instructions.len == 0)
                null
            else
                self.stack.pop();

            const maybe_initializer: ?Air.Instruction = if (maybe_value) |value|
                switch (value) {
                    .string => |string| .{ .string = string },
                    .int => |int| .{ .int = int },
                    .float => |float| .{ .float = float },
                    .boolean => |boolean| .{ .boolean = boolean },
                    .module_id, .type_id, .function => {
                        self.error_info = .{
                            .message = "expected the global initializer to not be a value only available at compile time",
                            .source_loc = SourceLoc.find(self.module.file.buffer, lazy_unit.token_start),
                        };

                        return error.WithMessage;
                    },
                    .runtime => {
                        self.error_info = .{
                            .message = "expected the global initializer to be known at compile time",
                            .source_loc = SourceLoc.find(self.module.file.buffer, lazy_unit.token_start),
                        };

                        return error.WithMessage;
                    },
                }
            else
                null;

            const type_id = if (lazy_unit.type_instructions.len == 0)
                try self.getTypeIdFromValue(maybe_value.?)
            else blk: {
                for (lazy_unit.type_instructions) |instruction| {
                    try self.analyzeInstruction(instruction);
                }

                const type_id = try self.popType(lazy_unit.token_start);

                if (maybe_value) |value| {
                    try self.checkUnaryImplicitCast(value, self.compilation.getTypeFromId(type_id), lazy_unit.token_start);
                }

                break :blk type_id;
            };

            const @"type" = self.compilation.getTypeFromId(type_id);

            if (@"type" == .void) {
                self.error_info = .{
                    .message = "cannot declare a variable with type 'void'",
                    .source_loc = SourceLoc.find(self.module.file.buffer, lazy_unit.token_start),
                };

                return error.WithMessage;
            }

            variable.value = .{ .runtime = type_id };

            try self.air.variables.put(self.allocator, variable.air_name, .{
                .type_id = type_id,
                .maybe_initializer = maybe_initializer,
            });
        }
    } else {
        var sema = try Sema.init(self.allocator, self.compilation, lazy_unit.owner_id, self.air);

        sema.analyzeLazyUnit(variable, lazy_unit) catch |err| {
            switch (err) {
                error.OutOfMemory => std.debug.print("Error: {s}\n", .{root.Cli.errorDescription(err)}),

                error.WithMessage => std.debug.print("{s}:{}:{}: {s}\n", .{
                    sema.module.file.path,
                    sema.error_info.?.source_loc.line,
                    sema.error_info.?.source_loc.column,
                    sema.error_info.?.message,
                }),

                error.WithoutMessage => {},
            }

            return error.WithoutMessage;
        };
    }
}

fn analyzeInstruction(self: *Sema, instruction: Sir.Instruction) Error!void {
    switch (instruction) {
        .duplicate => try self.analyzeDuplicate(),
        .reverse => |count| try self.analyzeReverse(true, count),
        .comptime_reverse => |count| try self.analyzeReverse(false, count),
        .pop => try self.analyzePop(),

        .string => |string| try self.analyzeString(string),
        .int => |int| try self.analyzeInt(int),
        .float => |float| try self.analyzeFloat(float),
        .function => |function| try self.analyzeFunction(function),

        .array_type => |token_start| try self.analyzeArrayType(token_start),
        .pointer_type => |pointer_type| try self.analyzePointerType(pointer_type),
        .struct_type => |struct_type| try self.analyzeStructType(struct_type),
        .enum_type => |enum_type| try self.analyzeEnumType(enum_type),
        .function_type => |function_type| try self.analyzeFunctionType(function_type),

        .negate => |token_start| try self.analyzeNegate(token_start),

        .bool_not => |token_start| try self.analyzeNot(.bool, token_start),
        .bit_not => |token_start| try self.analyzeNot(.bit, token_start),

        .bit_and => |token_start| try self.analyzeBitwiseArithmetic(.bit_and, token_start),
        .bit_or => |token_start| try self.analyzeBitwiseArithmetic(.bit_or, token_start),
        .bit_xor => |token_start| try self.analyzeBitwiseArithmetic(.bit_xor, token_start),

        .write => |token_start| try self.analyzeWrite(token_start),
        .read => |token_start| try self.analyzeRead(token_start),
        .reference => |token_start| try self.analyzeReference(token_start),

        .add => |token_start| try self.analyzeArithmetic(.add, token_start),
        .sub => |token_start| try self.analyzeArithmetic(.sub, token_start),
        .mul => |token_start| try self.analyzeArithmetic(.mul, token_start),
        .div => |token_start| try self.analyzeArithmetic(.div, token_start),
        .rem => |token_start| try self.analyzeArithmetic(.rem, token_start),

        .lt => |token_start| try self.analyzeComparison(.lt, token_start),
        .gt => |token_start| try self.analyzeComparison(.gt, token_start),
        .eql => |token_start| try self.analyzeComparison(.eql, token_start),

        .shl => |token_start| try self.analyzeBitwiseShift(.left, token_start),
        .shr => |token_start| try self.analyzeBitwiseShift(.right, token_start),

        .parameters => |names| try self.analyzeParameters(names),

        .cast => |cast| try self.analyzeCast(cast),

        .import => |token_start| try self.analyzeImport(token_start),

        .inline_assembly => |inline_assembly| try self.analyzeInlineAssembly(inline_assembly),

        .call => |call| try self.analyzeCall(call),

        .constant => |name| try self.analyzeConstant(name),

        .variable => |name| try self.analyzeVariable(false, name),
        .variable_infer => |name| try self.analyzeVariable(true, name),

        .get => |name| try self.analyzeGet(null, name),
        .set => |name| try self.analyzeSet(name),
        .pre_get_element => |token_start| try self.analyzePreGetElement(token_start),
        .get_element => |token_start| try self.analyzeGetElement(token_start),
        .get_field => |name| try self.analyzeGetField(name),
        .make_slice => |token_start| try self.analyzeMakeSlice(token_start),

        .block => |block| try self.analyzeBlock(block),
        .br => |br| try self.analyzeBr(br),
        .cond_br => |cond_br| try self.analyzeCondBr(cond_br),
        .@"switch" => |@"switch"| try self.analyzeSwitch(@"switch"),

        .start_scope => try self.modifyScope(true),
        .end_scope => try self.modifyScope(false),

        .ret => |token_start| try self.analyzeReturn(true, token_start),
        .ret_void => |token_start| try self.analyzeReturn(false, token_start),
    }
}

fn modifyScope(self: *Sema, comptime start: bool) Error!void {
    if (start) {
        const new_scope = try self.allocated_scopes.addOne(self.allocator);
        new_scope.* = .{ .maybe_parent = self.scope };
        self.scope = new_scope;

        try self.air_instructions.append(self.allocator, .start_scope);
    } else {
        self.scope.deinit(self.allocator);
        self.scope = self.scope.maybe_parent.?;
        _ = self.allocated_scopes.pop();

        try self.air_instructions.append(self.allocator, .end_scope);
    }
}

fn analyzeDuplicate(self: *Sema) Error!void {
    try self.stack.append(self.allocator, self.stack.getLast());

    try self.air_instructions.append(self.allocator, .duplicate);
}

fn analyzeReverse(self: *Sema, comptime emit_air: bool, count: u32) Error!void {
    std.mem.reverse(Value, self.stack.items[self.stack.items.len - count ..]);

    if (emit_air) try self.air_instructions.append(self.allocator, .{ .reverse = count });
}

fn analyzePop(self: *Sema) Error!void {
    if (self.stack.popOrNull()) |unused_value| {
        switch (unused_value) {
            .type_id, .module_id => {},
            else => if (try self.getTypeFromValue(unused_value) != .void)
                try self.air_instructions.append(self.allocator, .pop),
        }
    }
}

fn analyzeString(self: *Sema, range: Range) Error!void {
    try self.stack.append(self.allocator, .{ .string = range });

    try self.air_instructions.append(self.allocator, .{ .string = range });
}

fn analyzeInt(self: *Sema, id: u32) Error!void {
    try self.stack.append(self.allocator, .{ .int = id });

    try self.air_instructions.append(self.allocator, .{ .int = id });
}

fn analyzeFloat(self: *Sema, float: f64) Error!void {
    try self.stack.append(self.allocator, .{ .float = float });

    try self.air_instructions.append(self.allocator, .{ .float = float });
}

threadlocal var prng = std.Random.DefaultPrng.init(0);

fn analyzeFunction(self: *Sema, function: Sir.Instruction.Function) Error!void {
    const previous_function_type = self.function_type;
    const function_type_id = try self.popType(function.token_start);
    defer self.function_type = previous_function_type;

    self.function_type = self.compilation.getTypeFromId(function_type_id).function;

    const previous_scope = self.scope;
    defer self.scope = previous_scope;

    while (self.scope.maybe_parent) |parent| self.scope = parent;

    const previous_allocated_scopes = self.allocated_scopes;
    defer {
        self.allocated_scopes.deinit(self.allocator);
        self.allocated_scopes = previous_allocated_scopes;
    }

    self.allocated_scopes = .{};

    var scopes_count: usize = 0;

    for (function.instructions) |instruction|
        switch (instruction) {
            .start_scope => scopes_count += 1,
            else => {},
        };

    try self.allocated_scopes.ensureUnusedCapacity(self.allocator, scopes_count);

    const air_name = if (function.maybe_foreign) |foreign|
        try self.allocator.dupe(u8, self.compilation.getStringFromRange(foreign))
    else if (function.maybe_named) |name|
        try std.fmt.allocPrint(self.allocator, "{s}.{s}", .{ self.module.file.path, name.buffer })
    else
        try std.fmt.allocPrint(self.allocator, "__anon_function_{}", .{prng.random().int(u32)});

    const id: u32 = @intCast(self.air.functions.count());

    try self.air.functions.put(self.allocator, air_name, .{
        .type_id = function_type_id,
        .instructions = &.{},
    });

    var maybe_previous_scope_variable: ?Variable = null;

    if (function.maybe_named) |name| {
        maybe_previous_scope_variable = self.scope.get(name.buffer);

        try self.scope.put(self.allocator, name.buffer, .{
            .is_const = true,
            .air_name = air_name,
            .value = .{ .function = id },
        });
    }

    const instructions_start = self.air_instructions.items.len;

    for (function.instructions) |instruction| {
        try self.analyzeInstruction(instruction);
    }

    self.air.functions.values()[id].instructions = try self.allocator.dupe(Air.Instruction, self.air_instructions.items[instructions_start..]);

    self.air_instructions.shrinkRetainingCapacity(instructions_start);

    if (function.maybe_named) |name| {
        if (maybe_previous_scope_variable) |previous_scope_variable| {
            try self.scope.put(self.allocator, name.buffer, previous_scope_variable);
        } else {
            _ = self.scope.remove(name.buffer);
        }
    }

    try self.air_instructions.append(self.allocator, .{ .get_variable_ptr = air_name });

    try self.stack.append(self.allocator, .{ .function = id });
}

fn analyzeArrayType(self: *Sema, token_start: u32) Error!void {
    const child_type_id = try self.popType(token_start);

    const array_length_value = self.stack.pop();

    if (array_length_value == .runtime) {
        self.error_info = .{
            .message = "array length must be known at compile time",
            .source_loc = SourceLoc.find(self.module.file.buffer, token_start),
        };
    }

    const usize_type: Type = .{
        .int = .{
            .signedness = .unsigned,
            .bits = self.compilation.env.target.ptrBitWidth(),
        },
    };

    try self.checkUnaryImplicitCast(array_length_value, usize_type, token_start);

    try self.stack.append(self.allocator, .{
        .type_id = try self.compilation.putType(.{
            .array = .{
                .len_int_id = array_length_value.int,
                .child_type_id = child_type_id,
            },
        }),
    });
}

fn analyzePointerType(self: *Sema, pointer_type: Sir.Instruction.PointerType) Error!void {
    const child_type_id = try self.popType(pointer_type.token_start);

    try self.stack.append(self.allocator, .{
        .type_id = try self.compilation.putType(.{
            .pointer = .{
                .size = pointer_type.size,
                .is_const = pointer_type.is_const,
                .child_type_id = child_type_id,
            },
        }),
    });
}

fn analyzeStructType(self: *Sema, struct_type: Sir.Instruction.StructType) Error!void {
    var fields: std.ArrayListUnmanaged(Type.Struct.Field) = .{};

    const fields_slice = try fields.addManyAsSlice(self.allocator, struct_type.fields.len);

    for (struct_type.fields, 0..) |name, i| {
        fields_slice[i] = .{
            .name = name.buffer,
            .type_id = try self.popType(name.token_start),
        };
    }

    try self.stack.append(self.allocator, .{
        .type_id = try self.compilation.putType(.{ .@"struct" = .{ .fields = fields_slice } }),
    });
}

fn analyzeEnumType(self: *Sema, enum_type: Sir.Instruction.EnumType) Error!void {
    var minimum_value: i128 = std.math.maxInt(i128);
    var minimum_value_int_id: u32 = 0;
    var maximum_value: i128 = std.math.minInt(i128);
    var maximum_value_int_id: u32 = 0;

    for (enum_type.fields) |enum_field| {
        const enum_field_int = self.compilation.getIntFromId(enum_field.int_id);

        if (enum_field_int < minimum_value) {
            minimum_value = enum_field_int;
            minimum_value_int_id = enum_field.int_id;
        }

        if (enum_field_int > maximum_value) {
            maximum_value = enum_field_int;
            maximum_value_int_id = enum_field.int_id;
        }
    }

    var backing_type_id: u32 = 0;

    if (self.stack.items.len > 0) {
        backing_type_id = try self.popType(enum_type.token_start);

        const backing_type = self.compilation.getTypeFromId(backing_type_id);

        try self.checkIntType(backing_type, enum_type.token_start);

        try self.checkUnaryImplicitCast(.{ .int = minimum_value_int_id }, backing_type, enum_type.token_start);
        try self.checkUnaryImplicitCast(.{ .int = maximum_value_int_id }, backing_type, enum_type.token_start);
    } else {
        backing_type_id = try self.compilation.putType(Type.intFittingRange(minimum_value, maximum_value));
    }

    try self.stack.append(
        self.allocator,
        .{
            .type_id = try self.compilation.putType(.{
                .@"enum" = .{
                    .backing_type_id = backing_type_id,
                    .fields = enum_type.fields,
                },
            }),
        },
    );
}

fn analyzeFunctionType(self: *Sema, function_type: Sir.Instruction.FunctionType) Error!void {
    const return_type_id = try self.popType(function_type.token_start);

    const parameter_type_ids = try self.allocator.alloc(u32, function_type.parameters_count);

    for (0..function_type.parameters_count) |i| {
        parameter_type_ids[i] = try self.compilation.putType(self.compilation.getTypeFromId(try self.popType(function_type.token_start)));
    }

    try self.stack.append(self.allocator, .{
        .type_id = try self.compilation.putType(.{
            .function = .{
                .parameter_type_ids = parameter_type_ids,
                .is_var_args = function_type.is_var_args,
                .return_type_id = return_type_id,
            },
        }),
    });
}

fn analyzeNegate(self: *Sema, token_start: u32) Error!void {
    const rhs = self.stack.pop();

    const rhs_type = try self.getTypeFromValue(rhs);

    if (rhs != .int and !rhs_type.canBeNegative()) {
        var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

        try error_message_buf.appendSlice(self.allocator, "value of type '");
        try rhs_type.format(self.compilation.*, error_message_buf.writer(self.allocator));
        try error_message_buf.appendSlice(self.allocator, "' cannot be negated");

        self.error_info = .{ .message = error_message_buf.items, .source_loc = SourceLoc.find(self.module.file.buffer, token_start) };

        return error.WithMessage;
    }

    switch (rhs) {
        .int => |rhs_int_id| {
            const rhs_int = self.compilation.getIntFromId(rhs_int_id);

            const result_int_id = try self.compilation.putInt(-rhs_int);

            self.air_instructions.items[self.air_instructions.items.len - 1] = .{ .int = result_int_id };

            try self.stack.append(self.allocator, .{ .int = result_int_id });
        },

        .float => |rhs_float| {
            self.air_instructions.items[self.air_instructions.items.len - 1] = .{ .float = -rhs_float };

            try self.stack.append(self.allocator, .{ .float = -rhs_float });
        },

        .runtime => |rhs_runtime| {
            try self.air_instructions.append(self.allocator, .negate);

            try self.stack.append(self.allocator, .{ .runtime = rhs_runtime });
        },

        else => unreachable,
    }
}

const NotOperation = enum {
    bool,
    bit,
};

fn analyzeNot(self: *Sema, comptime operand: NotOperation, token_start: u32) Error!void {
    const rhs = self.stack.pop();
    const rhs_type = try self.getTypeFromValue(rhs);

    if (operand == .bool) {
        try self.checkUnaryImplicitCast(rhs, .bool, token_start);
    } else if (operand == .bit) {
        try self.checkInt(rhs_type, token_start);
    }

    switch (rhs) {
        .int => |rhs_int_id| {
            const rhs_int = self.compilation.getIntFromId(rhs_int_id);

            const result_int_id = try self.compilation.putInt(~rhs_int);

            self.air_instructions.items[self.air_instructions.items.len - 1] = .{ .int = result_int_id };

            try self.stack.append(self.allocator, .{ .int = result_int_id });
        },

        .boolean => |rhs_boolean| {
            self.air_instructions.items[self.air_instructions.items.len - 1] = .{ .boolean = !rhs_boolean };

            try self.stack.append(self.allocator, .{ .boolean = !rhs_boolean });
        },

        .runtime => |rhs_runtime| {
            try self.air_instructions.append(self.allocator, if (operand == .bool) .bool_not else .bit_not);

            try self.stack.append(self.allocator, .{ .runtime = rhs_runtime });
        },

        else => unreachable,
    }
}

const BitwiseArithmeticOperation = enum {
    bit_and,
    bit_or,
    bit_xor,
};

fn analyzeBitwiseArithmetic(self: *Sema, comptime operation: BitwiseArithmeticOperation, token_start: u32) Error!void {
    const rhs = self.stack.pop();
    const lhs = self.stack.pop();

    var lhs_type_id = try self.getTypeIdFromValue(lhs);
    var rhs_type_id = try self.getTypeIdFromValue(rhs);

    var lhs_type = self.compilation.getTypeFromId(lhs_type_id);
    var rhs_type = self.compilation.getTypeFromId(rhs_type_id);

    try self.checkIntOrBool(lhs_type, token_start);
    try self.checkIntOrBool(rhs_type, token_start);

    switch (try self.checkBinaryImplicitCast(lhs, rhs, token_start)) {
        .cast_lhs_to_rhs => {
            rhs_type_id = lhs_type_id;
            rhs_type = lhs_type;
        },

        .cast_rhs_to_lhs => {
            lhs_type_id = lhs_type_id;
            lhs_type = lhs_type;
        },

        .none => {},
    }

    switch (lhs) {
        .int => |lhs_int_id| switch (rhs) {
            .int => |rhs_int_id| {
                const lhs_int = self.compilation.getIntFromId(lhs_int_id);
                const rhs_int = self.compilation.getIntFromId(rhs_int_id);

                const result = switch (operation) {
                    .bit_and => lhs_int & rhs_int,
                    .bit_or => lhs_int | rhs_int,
                    .bit_xor => lhs_int ^ rhs_int,
                };

                const result_int_id = try self.compilation.putInt(result);

                try self.air_instructions.appendSlice(self.allocator, &.{ .pop, .pop, .{ .int = result_int_id } });

                return self.stack.append(self.allocator, .{ .int = result_int_id });
            },

            else => {},
        },

        .boolean => |lhs_boolean| switch (rhs) {
            .boolean => |rhs_boolean| {
                const result = switch (operation) {
                    .bit_and => @intFromBool(lhs_boolean) & @intFromBool(rhs_boolean),
                    .bit_or => @intFromBool(lhs_boolean) | @intFromBool(rhs_boolean),
                    .bit_xor => @intFromBool(lhs_boolean) ^ @intFromBool(rhs_boolean),
                } == 1;

                try self.air_instructions.appendSlice(self.allocator, &.{ .pop, .pop, .{ .boolean = result } });

                return self.stack.append(self.allocator, .{ .boolean = result });
            },

            else => {},
        },

        else => {},
    }

    switch (operation) {
        .bit_and => try self.air_instructions.append(self.allocator, .bit_and),
        .bit_or => try self.air_instructions.append(self.allocator, .bit_or),
        .bit_xor => try self.air_instructions.append(self.allocator, .bit_xor),
    }

    try self.stack.append(self.allocator, .{ .runtime = lhs_type_id });
}

fn analyzeWrite(self: *Sema, token_start: u32) Error!void {
    const lhs = self.stack.pop();
    const lhs_type = try self.getTypeFromValue(lhs);

    const lhs_pointer = lhs_type.getPointer() orelse try self.reportNotPointer(lhs_type, token_start);

    const rhs = self.stack.pop();

    if (self.compilation.getTypeFromId(lhs_pointer.child_type_id) == .function) {
        self.error_info = .{
            .message = "cannot write to function pointers, as they are pointing to the start of instructions in memory, which are not values",
            .source_loc = SourceLoc.find(self.module.file.buffer, token_start),
        };

        return error.WithMessage;
    }

    if (lhs_pointer.is_const) {
        self.error_info = .{ .message = "cannot mutate data pointed by this pointer, as it points to read-only data", .source_loc = SourceLoc.find(self.module.file.buffer, token_start) };

        return error.WithMessage;
    }

    try self.checkUnaryImplicitCast(rhs, self.compilation.getTypeFromId(lhs_pointer.child_type_id), token_start);

    try self.air_instructions.append(self.allocator, .write);
}

fn analyzeRead(self: *Sema, token_start: u32) Error!void {
    const rhs = self.stack.pop();
    const rhs_type = try self.getTypeFromValue(rhs);

    const rhs_pointer = rhs_type.getPointer() orelse try self.reportNotPointer(rhs_type, token_start);

    if (self.compilation.getTypeFromId(rhs_pointer.child_type_id) == .function) {
        self.error_info = .{
            .message = "cannot read from function pointers, as they are pointing to the start of instructions in memory, which are not values",
            .source_loc = SourceLoc.find(self.module.file.buffer, token_start),
        };

        return error.WithMessage;
    }

    try self.air_instructions.append(self.allocator, .read);

    try self.stack.append(self.allocator, .{ .runtime = rhs_pointer.child_type_id });
}

fn analyzeGet(self: *Sema, maybe_variable: ?*Variable, name: Name) Error!void {
    const variable = maybe_variable orelse self.scope.getPtr(name.buffer) orelse try self.reportNotDeclared(name);

    if (self.compilation.pool.lazy_units.get(variable.air_name)) |lazy_unit| {
        _ = self.compilation.pool.lazy_units.remove(variable.air_name);

        try self.analyzeLazyUnit(variable, lazy_unit);
    }

    switch (variable.value) {
        .module_id, .type_id => {},
        .function => |id| try self.air_instructions.append(self.allocator, .{ .get_variable_ptr = self.air.functions.keys()[id] }),
        .string => |range| try self.air_instructions.append(self.allocator, .{ .string = range }),
        .int => |id| try self.air_instructions.append(self.allocator, .{ .int = id }),
        .float => |float| try self.air_instructions.append(self.allocator, .{ .float = float }),
        .boolean => |boolean| try self.air_instructions.append(self.allocator, .{ .boolean = boolean }),
        .runtime => try self.air_instructions.appendSlice(self.allocator, &.{ .{ .get_variable_ptr = variable.air_name }, .read }),
    }

    try self.stack.append(self.allocator, variable.value);
}

fn analyzeSet(self: *Sema, name: Name) Error!void {
    const variable = self.scope.getPtr(name.buffer) orelse try self.reportNotDeclared(name);

    if (variable.is_const) {
        self.error_info = .{ .message = "cannot mutate the value of a constant", .source_loc = SourceLoc.find(self.module.file.buffer, name.token_start) };

        return error.WithMessage;
    }

    if (self.compilation.pool.lazy_units.get(variable.air_name)) |lazy_unit| {
        try self.analyzeLazyUnit(variable, lazy_unit);

        _ = self.compilation.pool.lazy_units.remove(variable.air_name);
    }

    const value = self.stack.pop();

    if (value == .function) {
        self.error_info = .{
            .message = "functions are only available at compile time, perhaps you meant to store a function pointer? then use `&x`",
            .source_loc = SourceLoc.find(self.module.file.buffer, name.token_start),
        };

        return error.WithMessage;
    }

    try self.checkUnaryImplicitCast(value, try self.getTypeFromValue(variable.value), name.token_start);

    try self.air_instructions.append(self.allocator, .{ .get_variable_ptr = variable.air_name });
    try self.air_instructions.append(self.allocator, .write);
}

fn analyzeGetField(self: *Sema, name: Name) Error!void {
    const rhs_type = try self.getTypeFromValue(self.stack.getLast());

    if (rhs_type == .@"struct" or (rhs_type == .pointer and rhs_type.pointer.size == .slice))
        try self.analyzeReference(name.token_start);

    const rhs = self.stack.pop();

    if (rhs == .module_id) {
        if (self.compilation.getModulePtrFromId(rhs.module_id).scope.get(name.buffer)) |module_variable| {
            var mutable_module_variable = module_variable;
            try self.analyzeGet(&mutable_module_variable, name);

            return self.compilation.getModulePtrFromId(rhs.module_id).scope.put(
                self.allocator,
                name.buffer,
                mutable_module_variable,
            );
        }
    } else if (rhs == .type_id and self.compilation.getTypeFromId(rhs.type_id) == .@"enum") {
        const rhs_enum_type = self.compilation.getTypeFromId(rhs.type_id).@"enum";

        for (rhs_enum_type.fields) |enum_field| {
            if (std.mem.eql(u8, enum_field.name, name.buffer)) {
                try self.air_instructions.append(self.allocator, .{ .int = enum_field.int_id });

                return self.stack.append(self.allocator, .{ .int = enum_field.int_id });
            }
        }
    } else if (rhs_type == .array or
        (rhs_type == .pointer and self.compilation.getTypeFromId(rhs_type.pointer.child_type_id) == .array))
    {
        if (std.mem.eql(u8, name.buffer, "len")) {
            const rhs_array = if (rhs_type.getPointer()) |pointer| self.compilation.getTypeFromId(pointer.child_type_id).array else rhs_type.array;

            try self.air_instructions.append(self.allocator, .pop);
            try self.air_instructions.append(self.allocator, .{ .int = rhs_array.len_int_id });

            return self.stack.append(self.allocator, .{ .int = rhs_array.len_int_id });
        }
    } else if (rhs_type == .pointer and (rhs_type.pointer.size == .slice or
        (self.compilation.getTypeFromId(rhs_type.pointer.child_type_id) == .pointer and
        self.compilation.getTypeFromId(rhs_type.pointer.child_type_id).pointer.size == .slice)))
    {
        if (std.mem.eql(u8, name.buffer, "len")) {
            try self.air_instructions.append(self.allocator, .{ .get_field_ptr = 0 });
            try self.air_instructions.append(self.allocator, .read);

            const usize_type: Type = .{ .int = .{ .signedness = .unsigned, .bits = self.compilation.env.target.ptrBitWidth() } };

            return self.stack.append(self.allocator, .{ .runtime = try self.compilation.putType(usize_type) });
        } else if (std.mem.eql(u8, name.buffer, "ptr")) {
            try self.air_instructions.append(self.allocator, .{ .get_field_ptr = 1 });
            try self.air_instructions.append(self.allocator, .read);

            return self.stack.append(self.allocator, .{
                .runtime = try self.compilation.putType(.{
                    .pointer = .{
                        .size = .many,
                        .is_const = rhs_type.pointer.is_const,
                        .child_type_id = rhs_type.pointer.child_type_id,
                    },
                }),
            });
        }
    } else if (rhs_type == .@"struct" or
        (rhs_type == .pointer and self.compilation.getTypeFromId(rhs_type.pointer.child_type_id) == .@"struct"))
    {
        const rhs_struct = if (rhs_type.getPointer()) |pointer| self.compilation.getTypeFromId(pointer.child_type_id).@"struct" else rhs_type.@"struct";

        for (rhs_struct.fields, 0..) |field, i| {
            if (std.mem.eql(u8, field.name, name.buffer)) {
                try self.air_instructions.append(self.allocator, .{ .get_field_ptr = @intCast(i) });
                try self.air_instructions.append(self.allocator, .read);

                return self.stack.append(self.allocator, .{ .runtime = field.type_id });
            }
        }
    }

    if (rhs == .type_id) {
        var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

        try error_message_buf.writer(self.allocator).print("'{s}' is not a field in type '", .{name.buffer});
        try self.compilation.getTypeFromId(rhs.type_id).format(self.compilation.*, error_message_buf.writer(self.allocator));
        try error_message_buf.append(self.allocator, '\'');

        self.error_info = .{ .message = error_message_buf.items, .source_loc = SourceLoc.find(self.module.file.buffer, name.token_start) };
    } else if (rhs == .module_id) {
        var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

        try error_message_buf.writer(self.allocator).print("'{s}' is not a field in module '{s}'", .{
            name.buffer,
            self.compilation.getModulePtrFromId(rhs.module_id).file.path,
        });

        self.error_info = .{ .message = error_message_buf.items, .source_loc = SourceLoc.find(self.module.file.buffer, name.token_start) };
    } else {
        var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

        try error_message_buf.writer(self.allocator).print("'{s}' is not a field in value of type '", .{name.buffer});
        try rhs_type.format(self.compilation.*, error_message_buf.writer(self.allocator));
        try error_message_buf.append(self.allocator, '\'');

        self.error_info = .{ .message = error_message_buf.items, .source_loc = SourceLoc.find(self.module.file.buffer, name.token_start) };
    }

    return error.WithMessage;
}

fn analyzePreGetElement(self: *Sema, token_start: u32) Error!void {
    const lhs = self.stack.getLast();
    const lhs_type = try self.getTypeFromValue(lhs);

    if (lhs_type == .array) {
        try self.analyzeReference(token_start);
    } else if (lhs_type == .pointer and lhs_type.pointer.size == .slice) {
        try self.analyzeReference(token_start);

        try self.air_instructions.append(self.allocator, .{ .get_field_ptr = 1 });
        try self.air_instructions.append(self.allocator, .read);

        self.stack.items[self.stack.items.len - 1] = .{
            .runtime = try self.compilation.putType(.{
                .pointer = .{
                    .size = .many,
                    .is_const = lhs_type.pointer.is_const,
                    .child_type_id = lhs_type.pointer.child_type_id,
                },
            }),
        };
    } else if (lhs_type == .pointer and lhs_type.pointer.size != .many and
        self.compilation.getTypeFromId(lhs_type.pointer.child_type_id) != .array)
    {
        try self.reportNotIndexable(lhs_type, token_start);
    }
}

fn analyzeGetElement(self: *Sema, token_start: u32) Error!void {
    const index = self.stack.pop();

    const usize_type: Type = .{ .int = .{ .signedness = .unsigned, .bits = self.compilation.env.target.ptrBitWidth() } };

    try self.checkUnaryImplicitCast(index, usize_type, token_start);

    const lhs = self.stack.pop();
    const lhs_type = try self.getTypeFromValue(lhs);
    const lhs_pointer_type = lhs_type.pointer;

    try self.checkIndexOutOfBounds(index, lhs_pointer_type, token_start);

    try self.air_instructions.append(self.allocator, .get_element_ptr);
    try self.air_instructions.append(self.allocator, .read);

    const child_type_id = if (lhs_pointer_type.size == .one)
        self.compilation.getTypeFromId(lhs_pointer_type.child_type_id).array.child_type_id
    else
        lhs_pointer_type.child_type_id;

    try self.stack.append(self.allocator, .{ .runtime = child_type_id });
}

fn analyzeMakeSlice(self: *Sema, token_start: u32) Error!void {
    const end = self.stack.pop();
    const start = self.stack.pop();

    const lhs = self.stack.pop();
    const lhs_type = try self.getTypeFromValue(lhs);
    const lhs_pointer_type = lhs_type.pointer;

    const usize_type: Type = .{ .int = .{ .signedness = .unsigned, .bits = self.compilation.env.target.ptrBitWidth() } };

    try self.checkUnaryImplicitCast(start, usize_type, token_start);
    try self.checkUnaryImplicitCast(end, usize_type, token_start);

    try self.checkRangeOutOfBounds(start, end, lhs_pointer_type, token_start);

    try self.air_instructions.append(self.allocator, .slice);

    const child_type_id = if (lhs_pointer_type.size == .one)
        self.compilation.getTypeFromId(lhs_pointer_type.child_type_id).array.child_type_id
    else
        lhs_pointer_type.child_type_id;

    try self.stack.append(self.allocator, .{
        .runtime = try self.compilation.putType(.{
            .pointer = .{
                .size = .slice,
                .is_const = lhs_pointer_type.is_const,
                .child_type_id = child_type_id,
            },
        }),
    });
}

fn analyzeReference(self: *Sema, token_start: u32) Error!void {
    const rhs = self.stack.pop();
    const rhs_type_id = try self.getTypeIdFromValue(rhs);
    const rhs_type = self.compilation.getTypeFromId(rhs_type_id);

    switch (rhs_type) {
        .void => {
            self.error_info = .{ .message = "cannot reference value with type 'void'", .source_loc = SourceLoc.find(self.module.file.buffer, token_start) };

            return error.WithMessage;
        },

        .type => {
            self.error_info = .{ .message = "cannot reference a type", .source_loc = SourceLoc.find(self.module.file.buffer, token_start) };

            return error.WithMessage;
        },

        else => {},
    }

    switch (rhs) {
        .function => {},

        else => {
            const last_instruction = self.air_instructions.items[self.air_instructions.items.len - 1];

            if (last_instruction == .read) {
                _ = self.air_instructions.pop();
            } else {
                const anon_var_name = try std.fmt.allocPrint(self.allocator, "compiler::__anon_{}", .{prng.random().int(u32)});

                if (rhs == .runtime) {
                    try self.air_instructions.appendSlice(self.allocator, &.{
                        .{ .variable = .{ anon_var_name, rhs_type_id } },
                        .{ .get_variable_ptr = anon_var_name },
                        .duplicate,
                        .{ .reverse = 3 },
                        .{ .reverse = 2 },
                        .write,
                    });
                } else {
                    try self.air.variables.put(self.allocator, anon_var_name, .{
                        .type_id = rhs_type_id,
                        .maybe_initializer = switch (rhs) {
                            .string => |string| .{ .string = string },
                            .int => |int| .{ .int = int },
                            .float => |float| .{ .float = float },
                            .boolean => |boolean| .{ .boolean = boolean },

                            else => unreachable,
                        },
                    });

                    try self.air_instructions.appendSlice(self.allocator, &.{ .pop, .{ .get_variable_ptr = anon_var_name } });
                }
            }
        },
    }

    try self.stack.append(self.allocator, .{
        .runtime = try self.compilation.putType(.{
            .pointer = .{
                .size = .one,
                .is_const = rhs == .function,
                .child_type_id = rhs_type_id,
            },
        }),
    });
}

const ArithmeticOperation = enum {
    add,
    sub,
    mul,
    div,
    rem,
};

fn analyzeArithmetic(self: *Sema, comptime operation: ArithmeticOperation, token_start: u32) Error!void {
    const rhs = self.stack.pop();
    const lhs = self.stack.pop();

    var lhs_type_id = try self.getTypeIdFromValue(lhs);
    var rhs_type_id = try self.getTypeIdFromValue(rhs);

    var lhs_type = self.compilation.getTypeFromId(lhs_type_id);
    var rhs_type = self.compilation.getTypeFromId(rhs_type_id);

    if (operation == .add or operation == .sub) {
        try self.checkIntOrFloatOrPointer(lhs_type, token_start);
        try self.checkIntOrFloatOrPointer(rhs_type, token_start);

        const usize_type: Type = .{ .int = .{ .signedness = .unsigned, .bits = self.compilation.env.target.ptrBitWidth() } };

        if (lhs_type == .pointer and rhs_type != .pointer) {
            try self.checkUnaryImplicitCast(rhs, usize_type, token_start);
        } else if (rhs_type == .pointer and lhs_type != .pointer) {
            try self.checkUnaryImplicitCast(lhs, usize_type, token_start);
        } else if (lhs_type == .pointer and rhs_type == .pointer) {
            try self.checkUnaryImplicitCast(lhs, rhs_type, token_start);
        } else {
            switch (try self.checkBinaryImplicitCast(lhs, rhs, token_start)) {
                .cast_lhs_to_rhs => {
                    rhs_type_id = lhs_type_id;
                    rhs_type = lhs_type;
                },

                .cast_rhs_to_lhs => {
                    lhs_type_id = lhs_type_id;
                    lhs_type = lhs_type;
                },

                .none => {},
            }
        }
    } else {
        try self.checkIntOrFloat(lhs_type, token_start);
        try self.checkIntOrFloat(rhs_type, token_start);

        switch (try self.checkBinaryImplicitCast(lhs, rhs, token_start)) {
            .cast_lhs_to_rhs => {
                rhs_type_id = lhs_type_id;
                rhs_type = lhs_type;
            },

            .cast_rhs_to_lhs => {
                lhs_type_id = lhs_type_id;
                lhs_type = lhs_type;
            },

            .none => {},
        }
    }

    switch (lhs) {
        .int => |lhs_int_id| switch (rhs) {
            .int => |rhs_int_id| {
                const lhs_int = self.compilation.getIntFromId(lhs_int_id);
                const rhs_int = self.compilation.getIntFromId(rhs_int_id);

                if (rhs_int == 0 and (operation == .div or operation == .rem)) {
                    self.error_info = .{ .message = "division by zero", .source_loc = SourceLoc.find(self.module.file.buffer, token_start) };

                    return error.WithMessage;
                }

                const result = switch (operation) {
                    .add => lhs_int + rhs_int,
                    .sub => lhs_int - rhs_int,
                    .mul => lhs_int * rhs_int,
                    .div => @divFloor(lhs_int, rhs_int),
                    .rem => @rem(lhs_int, rhs_int),
                };

                const result_int_id = try self.compilation.putInt(result);

                try self.air_instructions.appendSlice(self.allocator, &.{ .pop, .pop, .{ .int = result_int_id } });

                return self.stack.append(self.allocator, .{ .int = result_int_id });
            },

            else => {},
        },

        .float => |lhs_float| switch (rhs) {
            .float => |rhs_float| {
                if (rhs_float == 0 and (operation == .div or operation == .rem)) {
                    self.error_info = .{ .message = "division by zero", .source_loc = SourceLoc.find(self.module.file.buffer, token_start) };

                    return error.WithMessage;
                }

                const result = switch (operation) {
                    .add => lhs_float + rhs_float,
                    .sub => lhs_float - rhs_float,
                    .mul => lhs_float * rhs_float,
                    .div => lhs_float / rhs_float,
                    .rem => @rem(lhs_float, rhs_float),
                };

                try self.air_instructions.appendSlice(self.allocator, &.{ .pop, .pop, .{ .float = result } });

                return self.stack.append(self.allocator, .{ .float = result });
            },

            else => {},
        },

        else => {},
    }

    if (lhs_type == .pointer) {
        try self.stack.append(self.allocator, .{ .runtime = lhs_type_id });
    } else if (rhs_type == .pointer) {
        try self.stack.append(self.allocator, .{ .runtime = rhs_type_id });
    } else {
        try self.stack.append(self.allocator, .{ .runtime = lhs_type_id });
    }

    switch (operation) {
        .add => try self.air_instructions.append(self.allocator, .add),
        .sub => try self.air_instructions.append(self.allocator, .sub),
        .mul => try self.air_instructions.append(self.allocator, .mul),
        .div => try self.air_instructions.append(self.allocator, .div),
        .rem => try self.air_instructions.append(self.allocator, .rem),
    }
}

const ComparisonOperation = enum {
    lt,
    gt,
    eql,
};

fn analyzeComparison(self: *Sema, comptime operation: ComparisonOperation, token_start: u32) Error!void {
    const rhs = self.stack.pop();
    const lhs = self.stack.pop();

    var lhs_type_id = try self.getTypeIdFromValue(lhs);
    var rhs_type_id = try self.getTypeIdFromValue(rhs);

    var lhs_type = self.compilation.getTypeFromId(lhs_type_id);
    var rhs_type = self.compilation.getTypeFromId(rhs_type_id);

    if (operation == .lt or operation == .gt) {
        try self.checkIntOrFloat(lhs_type, token_start);
        try self.checkIntOrFloat(rhs_type, token_start);
    }

    try self.checkCanBeCompared(lhs_type, token_start);
    try self.checkCanBeCompared(rhs_type, token_start);

    switch (try self.checkBinaryImplicitCast(lhs, rhs, token_start)) {
        .cast_lhs_to_rhs => {
            rhs_type_id = lhs_type_id;
            rhs_type = lhs_type;
        },

        .cast_rhs_to_lhs => {
            lhs_type_id = lhs_type_id;
            lhs_type = lhs_type;
        },

        .none => {},
    }

    switch (lhs) {
        .int => |lhs_int_id| switch (rhs) {
            .int => |rhs_int_id| {
                const lhs_int = self.compilation.getIntFromId(lhs_int_id);
                const rhs_int = self.compilation.getIntFromId(rhs_int_id);

                const result = switch (operation) {
                    .lt => lhs_int < rhs_int,
                    .gt => lhs_int > rhs_int,
                    .eql => lhs_int == rhs_int,
                };

                try self.air_instructions.appendSlice(self.allocator, &.{ .pop, .pop, .{ .boolean = result } });

                return self.stack.append(self.allocator, .{ .boolean = result });
            },

            else => {},
        },

        .float => |lhs_float| switch (rhs) {
            .float => |rhs_float| {
                const result = switch (operation) {
                    .lt => lhs_float < rhs_float,
                    .gt => lhs_float > rhs_float,
                    .eql => lhs_float == rhs_float,
                };

                try self.air_instructions.appendSlice(self.allocator, &.{ .pop, .pop, .{ .boolean = result } });

                return self.stack.append(self.allocator, .{ .boolean = result });
            },

            else => {},
        },

        .boolean => |lhs_boolean| switch (rhs) {
            .boolean => |rhs_boolean| {
                const result = switch (operation) {
                    .eql => lhs_boolean == rhs_boolean,

                    else => unreachable,
                };

                try self.air_instructions.appendSlice(self.allocator, &.{ .pop, .pop, .{ .boolean = result } });

                return self.stack.append(self.allocator, .{ .boolean = result });
            },

            else => {},
        },

        else => {},
    }

    switch (operation) {
        .lt => try self.air_instructions.append(self.allocator, .lt),
        .gt => try self.air_instructions.append(self.allocator, .gt),
        .eql => try self.air_instructions.append(self.allocator, .eql),
    }

    try self.stack.append(self.allocator, .{ .runtime = try self.compilation.putType(.bool) });
}

const BitwiseShiftDirection = enum {
    left,
    right,
};

fn analyzeBitwiseShift(self: *Sema, comptime direction: BitwiseShiftDirection, token_start: u32) Error!void {
    const rhs = self.stack.pop();
    const lhs = self.stack.pop();

    const lhs_type_id = try self.getTypeIdFromValue(lhs);

    const lhs_type = self.compilation.getTypeFromId(lhs_type_id);
    const rhs_type = try self.getTypeFromValue(rhs);

    try self.checkInt(lhs_type, token_start);
    try self.checkInt(rhs_type, token_start);

    if (lhs != .runtime and rhs != .runtime) {
        const lhs_int_id = lhs.int;
        const rhs_int_id = rhs.int;

        const lhs_int = self.compilation.getIntFromId(lhs_int_id);
        const rhs_int = self.compilation.getIntFromId(rhs_int_id);

        const count_type: Type = .{ .int = .{ .signedness = .unsigned, .bits = 7 } };

        try self.checkBitShiftCount(rhs, count_type, token_start);

        const result = switch (direction) {
            .left => lhs_int << @intCast(rhs_int),
            .right => lhs_int >> @intCast(rhs_int),
        };

        const result_int_id = try self.compilation.putInt(result);

        try self.air_instructions.appendSlice(self.allocator, &.{ .pop, .pop, .{ .int = result_int_id } });

        try self.stack.append(self.allocator, .{ .int = result_int_id });
    } else {
        const count_type: Type = .{ .int = .{ .signedness = .unsigned, .bits = std.math.log2(lhs_type.int.bits) } };

        try self.checkBitShiftCount(rhs, count_type, token_start);

        switch (direction) {
            .left => try self.air_instructions.append(self.allocator, .shl),
            .right => try self.air_instructions.append(self.allocator, .shr),
        }

        try self.stack.append(self.allocator, .{ .runtime = lhs_type_id });
    }
}

fn analyzeCast(self: *Sema, token_start: u32) Error!void {
    const to_type_id = try self.popType(token_start);
    const to_type = self.compilation.getTypeFromId(to_type_id);
    const from_type = try self.getTypeFromValue(self.stack.getLast());

    if (from_type.eql(self.compilation.*, to_type)) return;

    const rhs = self.stack.pop();

    if (to_type == .void) {
        self.error_info = .{ .message = "cannot cast to 'void' as it is not possible to represent a value of this type", .source_loc = SourceLoc.find(self.module.file.buffer, token_start) };

        return error.WithMessage;
    } else if (to_type == .function) {
        self.error_info = .{ .message = "cannot cast to a function type as it should be always wrapped in a pointer", .source_loc = SourceLoc.find(self.module.file.buffer, token_start) };

        return error.WithMessage;
    } else if (from_type == .type) {
        self.error_info = .{ .message = "cannot cast a type", .source_loc = SourceLoc.find(self.module.file.buffer, token_start) };

        return error.WithMessage;
    } else if (to_type == .@"struct" or from_type == .@"struct") {
        self.error_info = .{ .message = "cannot cast from or to a struct", .source_loc = SourceLoc.find(self.module.file.buffer, token_start) };

        return error.WithMessage;
    } else if (to_type == .array or from_type == .array) {
        self.error_info = .{ .message = "cannot cast from or to an array", .source_loc = SourceLoc.find(self.module.file.buffer, token_start) };

        return error.WithMessage;
    } else if (to_type == .pointer and from_type != .pointer) {
        const usize_type: Type = .{ .int = .{ .signedness = .unsigned, .bits = self.compilation.env.target.ptrBitWidth() } };

        try self.checkUnaryImplicitCast(rhs, usize_type, token_start);
    } else if (to_type == .pointer and to_type.pointer.size == .slice) {
        self.error_info = .{ .message = "cannot cast explicitly to a slice, use slicing syntax or implicit casting", .source_loc = SourceLoc.find(self.module.file.buffer, token_start) };

        return error.WithMessage;
    } else if (from_type == .pointer and to_type != .pointer) {
        const usize_type: Type = .{ .int = .{ .signedness = .unsigned, .bits = self.compilation.env.target.ptrBitWidth() } };

        if (!to_type.eql(self.compilation.*, usize_type)) {
            var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

            try error_message_buf.appendSlice(self.allocator, "cannot cast from a pointer to '");
            try to_type.format(self.compilation.*, error_message_buf.writer(self.allocator));
            try error_message_buf.appendSlice(self.allocator, "', pointers can only cast to '");
            try usize_type.format(self.compilation.*, error_message_buf.writer(self.allocator));
            try error_message_buf.appendSlice(self.allocator, "' in this target (note: you should use 'usize' as it is an alias for '");
            try usize_type.format(self.compilation.*, error_message_buf.writer(self.allocator));
            try error_message_buf.appendSlice(self.allocator, "' in cross compilable manner as the size is different in other targets)");

            self.error_info = .{ .message = error_message_buf.items, .source_loc = SourceLoc.find(self.module.file.buffer, token_start) };

            return error.WithMessage;
        }
    } else if (to_type == .bool) {
        self.error_info = .{ .message = "cannot cast to a boolean, use comparison instead", .source_loc = SourceLoc.find(self.module.file.buffer, token_start) };

        return error.WithMessage;
    } else if (from_type == .bool) {
        try self.checkInt(to_type, token_start);
    } else if (from_type == .float) {
        try self.checkIntOrFloat(to_type, token_start);
    }

    try self.air_instructions.append(self.allocator, .{ .cast = to_type_id });

    try self.stack.append(self.allocator, .{ .runtime = to_type_id });
}

fn getImportFile(self: *Sema, file_path: []const u8, token_start: u32) Error!Compilation.File {
    if (std.mem.eql(u8, file_path, "root")) return self.compilation.root_file;

    if (std.mem.eql(u8, file_path, "std")) {
        const import_file = self.compilation.env.barq_lib.std_file;
        const import_file_path = self.compilation.env.barq_lib.std_file_path;

        if (self.compilation.pool.modules.get(import_file_path) != null) return .{ .path = import_file_path, .buffer = "" };

        const import_file_buffer = import_file.readToEndAllocOptions(
            self.allocator,
            std.math.maxInt(u32),
            null,
            @alignOf(u8),
            0,
        ) catch |err| {
            var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

            try error_message_buf.writer(self.allocator).print(
                "could not read import file: {s}",
                .{root.Cli.errorDescription(err)},
            );

            self.error_info = .{ .message = error_message_buf.items, .source_loc = SourceLoc.find(self.module.file.buffer, token_start) };

            return error.WithMessage;
        };

        return .{ .path = import_file_path, .buffer = import_file_buffer };
    }

    const parent_dir_path = std.fs.path.dirname(self.module.file.path) orelse if (self.module.file.path[0] == std.fs.path.sep)
        std.fs.path.sep_str
    else
        ".";

    var parent_dir = std.fs.cwd().openDir(parent_dir_path, .{}) catch |err| {
        var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

        try error_message_buf.writer(self.allocator).print(
            "could not open parent directory of root file: {s}",
            .{root.Cli.errorDescription(err)},
        );

        self.error_info = .{ .message = error_message_buf.items, .source_loc = SourceLoc.find(self.module.file.buffer, token_start) };

        return error.WithMessage;
    };

    defer parent_dir.close();

    const import_file = parent_dir.openFile(file_path, .{}) catch |err| {
        var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

        try error_message_buf.writer(self.allocator).print(
            "could not open import file: {s}",
            .{root.Cli.errorDescription(err)},
        );

        self.error_info = .{ .message = error_message_buf.items, .source_loc = SourceLoc.find(self.module.file.buffer, token_start) };

        return error.WithMessage;
    };

    defer import_file.close();

    const import_file_path = try std.fs.path.resolve(self.allocator, &.{ parent_dir_path, file_path });

    if (self.compilation.pool.modules.get(import_file_path) != null) return .{ .path = import_file_path, .buffer = "" };

    const import_file_buffer = import_file.readToEndAllocOptions(
        self.allocator,
        std.math.maxInt(u32),
        null,
        @alignOf(u8),
        0,
    ) catch |err| {
        var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

        try error_message_buf.writer(self.allocator).print(
            "could not read import file: {s}",
            .{root.Cli.errorDescription(err)},
        );

        self.error_info = .{ .message = error_message_buf.items, .source_loc = SourceLoc.find(self.module.file.buffer, token_start) };

        return error.WithMessage;
    };

    return .{ .path = import_file_path, .buffer = import_file_buffer };
}

fn analyzeImport(self: *Sema, token_start: u32) Error!void {
    if (self.air_instructions.pop() != .string) {
        self.error_info = .{ .message = "expected a compile time known string for import file path", .source_loc = SourceLoc.find(self.module.file.buffer, token_start) };

        return error.WithMessage;
    }

    const file_path = self.compilation.getStringFromRange(self.stack.pop().string);

    const import_file = try self.getImportFile(file_path, token_start);

    if (self.compilation.pool.modules.getIndex(import_file.path)) |module_id| {
        try self.stack.append(self.allocator, .{ .module_id = @intCast(module_id) });
    } else {
        var sir_parser = try Sir.Parser.init(self.allocator, self.compilation, import_file);
        defer sir_parser.deinit();

        sir_parser.parse() catch |err| {
            switch (err) {
                error.OutOfMemory => std.debug.print("Error: {s}\n", .{root.Cli.errorDescription(err)}),

                error.WithMessage => std.debug.print("{s}:{}:{}: {s}\n", .{
                    import_file.path,
                    sir_parser.error_info.?.source_loc.line,
                    sir_parser.error_info.?.source_loc.column,
                    sir_parser.error_info.?.message,
                }),

                error.WithoutMessage => {},
            }

            return error.WithoutMessage;
        };

        const module_scope_on_heap: *Scope = try self.allocator.create(Scope);
        module_scope_on_heap.* = .{};

        const module_id = try self.compilation.putModule(.{
            .file = import_file,
            .scope = module_scope_on_heap,
        });

        // `putModule` may invalidate our module pointer
        self.module = self.compilation.getModulePtrFromId(self.module_id);

        var sema = try Sema.init(self.allocator, self.compilation, module_id, self.air);
        try sema.hoist(sir_parser.sir);
        sema.deinit();

        try self.stack.append(self.allocator, .{ .module_id = module_id });
    }
}

fn analyzeInlineAssembly(self: *Sema, inline_assembly: Sir.Instruction.InlineAssembly) Error!void {
    self.stack.shrinkRetainingCapacity(self.stack.items.len - inline_assembly.input_constraints.len);

    if (inline_assembly.output_constraint) |output_constraint| {
        const output_constraint_type = try self.popType(inline_assembly.token_start);

        try self.air_instructions.append(self.allocator, .{
            .inline_assembly = .{
                .content = inline_assembly.content,
                .input_constraints = inline_assembly.input_constraints,
                .output_constraint = .{
                    .type_id = output_constraint_type,
                    .register = output_constraint,
                },
                .clobbers = inline_assembly.clobbers,
            },
        });

        try self.stack.append(self.allocator, .{ .runtime = output_constraint_type });
    } else {
        try self.air_instructions.append(self.allocator, .{
            .inline_assembly = .{
                .content = inline_assembly.content,
                .input_constraints = inline_assembly.input_constraints,
                .output_constraint = null,
                .clobbers = inline_assembly.clobbers,
            },
        });

        try self.stack.append(self.allocator, .{ .runtime = try self.compilation.putType(.void) });
    }
}

fn analyzeCall(self: *Sema, call: Sir.Instruction.Call) Error!void {
    const callable = self.stack.pop();
    const callable_type = try self.getTypeFromValue(callable);

    if (callable_type.getFunction(self.compilation.*)) |function| {
        if ((function.is_var_args and function.parameter_type_ids.len > call.arguments_count) or
            (!function.is_var_args and function.parameter_type_ids.len != call.arguments_count))
        {
            var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

            try error_message_buf.writer(self.allocator).print("expected {} argument(s) got {} argument(s)", .{
                function.parameter_type_ids.len,
                call.arguments_count,
            });

            self.error_info = .{ .message = error_message_buf.items, .source_loc = SourceLoc.find(self.module.file.buffer, call.token_start) };

            return error.WithMessage;
        }

        for (function.parameter_type_ids) |parameter_type_id| {
            const parameter_type = self.compilation.getTypeFromId(parameter_type_id);

            const argument = self.stack.pop();

            try self.checkUnaryImplicitCast(argument, parameter_type, call.token_start);
        }

        if (function.is_var_args) {
            self.stack.shrinkRetainingCapacity(self.stack.items.len - (call.arguments_count - function.parameter_type_ids.len));
        }

        try self.air_instructions.append(self.allocator, .{ .call = call.arguments_count });

        try self.stack.append(self.allocator, .{ .runtime = function.return_type_id });
    } else {
        var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

        try error_message_buf.appendSlice(self.allocator, "value of type '");
        try callable_type.format(self.compilation.*, error_message_buf.writer(self.allocator));
        try error_message_buf.appendSlice(self.allocator, "' does not support calling");

        self.error_info = .{ .message = error_message_buf.items, .source_loc = SourceLoc.find(self.module.file.buffer, call.token_start) };

        return error.WithMessage;
    }
}

fn analyzeParameters(self: *Sema, names: []Name) Error!void {
    var detokenized_names: std.ArrayListUnmanaged([]const u8) = .{};
    try detokenized_names.ensureTotalCapacity(self.allocator, names.len);

    try self.scope.ensureTotalCapacity(self.allocator, @intCast(names.len));

    for (names, 0..) |name, i| {
        if (self.scope.get(name.buffer) != null) try self.reportRedeclaration(name);

        detokenized_names.appendAssumeCapacity(name.buffer);

        self.scope.putAssumeCapacity(name.buffer, .{
            .air_name = name.buffer,
            .value = .{ .runtime = self.function_type.parameter_type_ids[i] },
        });
    }

    try self.air_instructions.append(self.allocator, .{ .parameters = detokenized_names.items });
}

fn analyzeConstant(self: *Sema, name: Name) Error!void {
    if (self.scope.get(name.buffer) != null) try self.reportRedeclaration(name);

    const value = self.stack.pop();

    if (value == .runtime) {
        self.error_info = .{ .message = "expected the constant value to be compile time known", .source_loc = SourceLoc.find(self.module.file.buffer, name.token_start) };

        return error.WithMessage;
    }

    const air_name = try std.fmt.allocPrint(self.allocator, "{s}.{s}", .{ self.module.file.path, name.buffer });

    try self.scope.put(self.allocator, name.buffer, .{
        .is_const = true,
        .air_name = air_name,
        .value = value,
    });
}

fn analyzeVariable(self: *Sema, comptime infer: bool, name: Name) Error!void {
    if (self.scope.get(name.buffer) != null) try self.reportRedeclaration(name);

    const type_id = if (infer)
        try self.getTypeIdFromValue(self.stack.getLast())
    else
        try self.popType(name.token_start);

    const @"type" = self.compilation.getTypeFromId(type_id);

    switch (@"type") {
        .void => {
            self.error_info = .{
                .message = "cannot declare a variable with type 'void'",
                .source_loc = SourceLoc.find(self.module.file.buffer, name.token_start),
            };

            return error.WithMessage;
        },

        .type => {
            self.error_info = .{
                .message = "cannot declare a variable with a type being the value",
                .source_loc = SourceLoc.find(self.module.file.buffer, name.token_start),
            };

            return error.WithMessage;
        },

        else => {},
    }

    try self.scope.put(self.allocator, name.buffer, .{
        .air_name = name.buffer,
        .value = .{ .runtime = type_id },
    });

    if (self.air_instructions.getLast() == .reverse) {
        _ = self.air_instructions.pop();
    }

    try self.air_instructions.append(self.allocator, .{ .variable = .{ name.buffer, type_id } });
}

fn analyzeBlock(self: *Sema, id: u32) Error!void {
    try self.air_instructions.append(self.allocator, .{ .block = id });
}

fn analyzeBr(self: *Sema, id: u32) Error!void {
    try self.air_instructions.append(self.allocator, .{ .br = id });
}

fn analyzeCondBr(self: *Sema, cond_br: Sir.Instruction.CondBr) Error!void {
    const condition = self.stack.pop();

    try self.checkUnaryImplicitCast(condition, .bool, cond_br.token_start);

    switch (condition) {
        .boolean => |condition_boolean| {
            try self.air_instructions.append(self.allocator, .pop);

            try self.air_instructions.append(
                self.allocator,
                .{
                    .br = if (condition_boolean)
                        cond_br.true_id
                    else
                        cond_br.false_id,
                },
            );
        },

        else => {
            try self.air_instructions.append(
                self.allocator,
                .{
                    .cond_br = .{
                        .true_id = cond_br.true_id,
                        .false_id = cond_br.false_id,
                    },
                },
            );
        },
    }
}

fn analyzeSwitch(self: *Sema, @"switch": Sir.Instruction.Switch) Error!void {
    const switched_value = self.stack.pop();
    const switched_value_type = try self.getTypeFromValue(switched_value);

    try self.checkIntOrBool(switched_value_type, @"switch".token_start);

    var case_values: std.AutoHashMapUnmanaged(i128, void) = .{};

    for (@"switch".case_token_starts) |case_token_start| {
        const case_value = self.stack.pop();

        try self.checkUnaryImplicitCast(case_value, switched_value_type, case_token_start);

        if (case_value == .runtime) {
            self.error_info = .{ .message = "expected switch case value to be compile time known", .source_loc = SourceLoc.find(self.module.file.buffer, case_token_start) };

            return error.WithMessage;
        }

        const case_value_int = switch (case_value) {
            .int => |int| int,
            .boolean => |boolean| @as(i128, @intFromBool(boolean)),

            else => unreachable,
        };

        if (case_values.get(case_value_int) != null) {
            self.error_info = .{ .message = "duplcicte switch case", .source_loc = SourceLoc.find(self.module.file.buffer, case_token_start) };

            return error.WithMessage;
        }

        try case_values.put(self.allocator, case_value_int, {});
    }

    case_values.deinit(self.allocator);

    try self.air_instructions.append(self.allocator, .{
        .@"switch" = .{
            .case_block_ids = @"switch".case_block_ids,
            .else_block_id = @"switch".else_block_id,
        },
    });
}

fn analyzeReturn(self: *Sema, comptime with_value: bool, token_start: u32) Error!void {
    const return_type = self.compilation.getTypeFromId(self.function_type.return_type_id);

    if (with_value) {
        try self.checkUnaryImplicitCast(self.stack.pop(), return_type, token_start);
    } else {
        if (return_type != .void) {
            self.error_info = .{ .message = "function with non void return type returns void", .source_loc = SourceLoc.find(self.module.file.buffer, token_start) };

            return error.WithMessage;
        }
    }

    try self.air_instructions.append(self.allocator, if (with_value) .ret else .ret_void);
}

fn canUnaryImplicitCast(self: Sema, lhs: Value, to_type: Type) std.mem.Allocator.Error!bool {
    const lhs_type = try self.getTypeFromValue(lhs);

    return (lhs_type.eql(self.compilation.*, to_type) or
        (lhs == .int and to_type == .int and self.compilation.getIntFromId(lhs.int) >= to_type.minInt() and
        lhs == .int and to_type == .int and self.compilation.getIntFromId(lhs.int) <= to_type.maxInt()) or
        (lhs == .float and to_type == .float and lhs.float >= -to_type.maxFloat() and
        lhs == .float and to_type == .float and lhs.float <= to_type.maxFloat()) or
        (lhs_type == .int and to_type == .int and
        lhs_type.maxInt() <= to_type.maxInt() and lhs_type.minInt() >= to_type.minInt() and
        lhs_type.canBeNegative() == to_type.canBeNegative()) or
        (lhs_type == .float and to_type == .float and
        lhs_type.maxFloat() <= to_type.maxFloat()) or
        (lhs_type == .pointer and to_type == .pointer and self.compilation.getTypeFromId(lhs_type.pointer.child_type_id) == .array and
        (to_type.pointer.size == .many or to_type.pointer.size == .slice) and
        self.compilation.getTypeFromId(self.compilation.getTypeFromId(lhs_type.pointer.child_type_id).array.child_type_id)
        .eql(self.compilation.*, self.compilation.getTypeFromId(to_type.pointer.child_type_id))));
}

fn checkUnaryImplicitCast(self: *Sema, lhs: Value, to_type: Type, token_start: u32) Error!void {
    if (!try self.canUnaryImplicitCast(lhs, to_type)) {
        const lhs_type = try self.getTypeFromValue(lhs);

        var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

        try error_message_buf.appendSlice(self.allocator, "value of type '");
        try lhs_type.format(self.compilation.*, error_message_buf.writer(self.allocator));
        try error_message_buf.appendSlice(self.allocator, "' cannot be implicitly casted to  '");
        try to_type.format(self.compilation.*, error_message_buf.writer(self.allocator));
        try error_message_buf.append(self.allocator, '\'');

        self.error_info = .{ .message = error_message_buf.items, .source_loc = SourceLoc.find(self.module.file.buffer, token_start) };

        return error.WithMessage;
    }
}

fn checkIndexOutOfBounds(self: *Sema, index: Value, lhs_pointer: Type.Pointer, token_start: u32) Error!void {
    if (index == .int and self.compilation.getTypeFromId(lhs_pointer.child_type_id) == .array and
        self.compilation.getIntFromId(index.int) >=
        self.compilation.getIntFromId(self.compilation.getTypeFromId(lhs_pointer.child_type_id).array.len_int_id))
    {
        self.error_info = .{ .message = "index out of bounds", .source_loc = SourceLoc.find(self.module.file.buffer, token_start) };

        return error.WithMessage;
    }
}

fn checkRangeOutOfBounds(self: *Sema, start: Value, end: Value, lhs_pointer: Type.Pointer, token_start: u32) Error!void {
    if (start == .int) {
        if (end == .int and self.compilation.getIntFromId(start.int) > self.compilation.getIntFromId(end.int)) {
            self.error_info = .{ .message = "range start is greater than range end", .source_loc = SourceLoc.find(self.module.file.buffer, token_start) };

            return error.WithMessage;
        }

        if (self.compilation.getTypeFromId(lhs_pointer.child_type_id) == .array and
            self.compilation.getIntFromId(start.int) >=
            self.compilation.getIntFromId(self.compilation.getTypeFromId(lhs_pointer.child_type_id).array.len_int_id))
        {
            self.error_info = .{ .message = "range start out of bounds", .source_loc = SourceLoc.find(self.module.file.buffer, token_start) };

            return error.WithMessage;
        }
    }

    if (end == .int and self.compilation.getTypeFromId(lhs_pointer.child_type_id) == .array and
        self.compilation.getIntFromId(end.int) >
        self.compilation.getIntFromId(self.compilation.getTypeFromId(lhs_pointer.child_type_id).array.len_int_id))
    {
        self.error_info = .{ .message = "range end out of bounds", .source_loc = SourceLoc.find(self.module.file.buffer, token_start) };

        return error.WithMessage;
    }
}

fn checkBitShiftCount(self: *Sema, rhs: Value, count_type: Type, token_start: u32) Error!void {
    if (!try self.canUnaryImplicitCast(rhs, count_type)) {
        const rhs_type = try self.getTypeFromValue(rhs);

        var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

        try error_message_buf.appendSlice(self.allocator, "value of type '");
        try rhs_type.format(self.compilation.*, error_message_buf.writer(self.allocator));
        try error_message_buf.appendSlice(self.allocator, "' cannot be used as a bit shift count, as it cannot be implicitly casted to '");
        try count_type.format(self.compilation.*, error_message_buf.writer(self.allocator));
        try error_message_buf.append(self.allocator, '\'');

        self.error_info = .{ .message = error_message_buf.items, .source_loc = SourceLoc.find(self.module.file.buffer, token_start) };

        return error.WithMessage;
    }
}

const BinaryImplicitCastResult = enum {
    none,
    cast_lhs_to_rhs,
    cast_rhs_to_lhs,
};

fn checkBinaryImplicitCast(self: *Sema, lhs: Value, rhs: Value, token_start: u32) Error!BinaryImplicitCastResult {
    const lhs_type = try self.getTypeFromValue(lhs);
    const rhs_type = try self.getTypeFromValue(rhs);

    if (std.meta.activeTag(lhs_type) == std.meta.activeTag(rhs_type)) {
        if (lhs == .runtime and rhs == .runtime and
            lhs_type.canBeNegative() != rhs_type.canBeNegative())
        {
            try self.reportIncompatibleTypes(lhs_type, rhs_type, token_start);
        }

        if (lhs_type == .int and lhs_type.int.bits > rhs_type.int.bits or
            lhs_type == .float and lhs_type.float.bits > rhs_type.float.bits)
        {
            // lhs as u64 > rhs as u16
            // lhs as f64 > rhs as f32
            // lhs as f64 > rhs as f16
            try self.checkUnaryImplicitCast(rhs, lhs_type, token_start);

            return .cast_rhs_to_lhs;
        } else if (lhs_type == .int and lhs_type.int.bits < rhs_type.int.bits or
            lhs_type == .float and lhs_type.float.bits < rhs_type.float.bits)
        {
            // lhs as u16 > rhs as u64
            // lhs as f32 > rhs as f64
            // lhs as f16 > rhs as f64
            try self.checkUnaryImplicitCast(lhs, rhs_type, token_start);

            return .cast_lhs_to_rhs;
        }
    } else {
        try self.reportIncompatibleTypes(lhs_type, rhs_type, token_start);
    }

    return .none;
}

fn checkIntOrBool(self: *Sema, provided_type: Type, token_start: u32) Error!void {
    if (provided_type != .int and provided_type != .bool) {
        var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

        try error_message_buf.appendSlice(self.allocator, "value of type '");
        try provided_type.format(self.compilation.*, error_message_buf.writer(self.allocator));
        try error_message_buf.appendSlice(self.allocator, "' is provided while expected an integer or boolean");

        self.error_info = .{ .message = error_message_buf.items, .source_loc = SourceLoc.find(self.module.file.buffer, token_start) };

        return error.WithMessage;
    }
}

fn checkIntOrFloat(self: *Sema, provided_type: Type, token_start: u32) Error!void {
    if (provided_type != .int and provided_type != .float) {
        var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

        try error_message_buf.appendSlice(self.allocator, "value of type '");
        try provided_type.format(self.compilation.*, error_message_buf.writer(self.allocator));
        try error_message_buf.appendSlice(self.allocator, "' is provided while expected an integer or float");

        self.error_info = .{ .message = error_message_buf.items, .source_loc = SourceLoc.find(self.module.file.buffer, token_start) };

        return error.WithMessage;
    }
}

fn checkIntOrFloatOrPointer(self: *Sema, provided_type: Type, token_start: u32) Error!void {
    if (provided_type != .int and provided_type != .float and provided_type != .pointer) {
        var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

        try error_message_buf.appendSlice(self.allocator, "value of type '");
        try provided_type.format(self.compilation.*, error_message_buf.writer(self.allocator));
        try error_message_buf.appendSlice(self.allocator, "' is provided while expected an integer or float or pointer");

        self.error_info = .{ .message = error_message_buf.items, .source_loc = SourceLoc.find(self.module.file.buffer, token_start) };

        return error.WithMessage;
    }
}

fn checkInt(self: *Sema, provided_type: Type, token_start: u32) Error!void {
    if (provided_type != .int) {
        var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

        try error_message_buf.appendSlice(self.allocator, "value of type '");
        try provided_type.format(self.compilation.*, error_message_buf.writer(self.allocator));
        try error_message_buf.appendSlice(self.allocator, "' is provided while expected an integer");

        self.error_info = .{ .message = error_message_buf.items, .source_loc = SourceLoc.find(self.module.file.buffer, token_start) };

        return error.WithMessage;
    }
}

fn checkIntType(self: *Sema, provided_type: Type, token_start: u32) Error!void {
    if (provided_type != .int) {
        var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

        try error_message_buf.appendSlice(self.allocator, "type '");
        try provided_type.format(self.compilation.*, error_message_buf.writer(self.allocator));
        try error_message_buf.appendSlice(self.allocator, "' is provided while expected an integer type");

        self.error_info = .{ .message = error_message_buf.items, .source_loc = SourceLoc.find(self.module.file.buffer, token_start) };

        return error.WithMessage;
    }
}

fn checkCanBeCompared(self: *Sema, provided_type: Type, token_start: u32) Error!void {
    if (provided_type == .@"struct" or provided_type == .void or provided_type == .function) {
        try self.reportNotComparable(provided_type, token_start);
    }
}

fn reportIncompatibleTypes(self: *Sema, lhs: Type, rhs: Type, token_start: u32) Error!noreturn {
    var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

    try error_message_buf.append(self.allocator, '\'');
    try lhs.format(self.compilation.*, error_message_buf.writer(self.allocator));
    try error_message_buf.appendSlice(self.allocator, "' is not compatible with '");
    try rhs.format(self.compilation.*, error_message_buf.writer(self.allocator));
    try error_message_buf.append(self.allocator, '\'');

    self.error_info = .{ .message = error_message_buf.items, .source_loc = SourceLoc.find(self.module.file.buffer, token_start) };

    return error.WithMessage;
}

fn reportNotDeclared(self: *Sema, name: Name) Error!noreturn {
    var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

    try error_message_buf.writer(self.allocator).print("'{s}' is not declared", .{name.buffer});

    self.error_info = .{ .message = error_message_buf.items, .source_loc = SourceLoc.find(self.module.file.buffer, name.token_start) };

    return error.WithMessage;
}

fn reportRedeclaration(self: *Sema, name: Name) Error!noreturn {
    var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

    try error_message_buf.writer(self.allocator).print("redeclaration of '{s}'", .{name.buffer});

    self.error_info = .{ .message = error_message_buf.items, .source_loc = SourceLoc.find(self.module.file.buffer, name.token_start) };

    return error.WithMessage;
}

fn reportNotPointer(self: *Sema, provided_type: Type, token_start: u32) Error!noreturn {
    var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

    try error_message_buf.appendSlice(self.allocator, "value of type '");
    try provided_type.format(self.compilation.*, error_message_buf.writer(self.allocator));
    try error_message_buf.appendSlice(self.allocator, "' is provided while expected a pointer");

    self.error_info = .{ .message = error_message_buf.items, .source_loc = SourceLoc.find(self.module.file.buffer, token_start) };

    return error.WithMessage;
}

fn reportNotIndexable(self: *Sema, provided_type: Type, token_start: u32) Error!noreturn {
    var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

    try error_message_buf.appendSlice(self.allocator, "value of type '");
    try provided_type.format(self.compilation.*, error_message_buf.writer(self.allocator));
    try error_message_buf.appendSlice(self.allocator, "' does not support indexing");

    self.error_info = .{ .message = error_message_buf.items, .source_loc = SourceLoc.find(self.module.file.buffer, token_start) };

    return error.WithMessage;
}

fn reportNotComparable(self: *Sema, provided_type: Type, token_start: u32) Error!noreturn {
    var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

    try error_message_buf.appendSlice(self.allocator, "value of type '");
    try provided_type.format(self.compilation.*, error_message_buf.writer(self.allocator));
    try error_message_buf.appendSlice(self.allocator, "' does not support comparison");

    self.error_info = .{ .message = error_message_buf.items, .source_loc = SourceLoc.find(self.module.file.buffer, token_start) };

    return error.WithMessage;
}

fn reportCircularDependency(self: *Sema, name: Name) Error!noreturn {
    var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

    try error_message_buf.writer(self.allocator).print("'{s}' is circularly dependent on itself", .{name.buffer});

    self.error_info = .{ .message = error_message_buf.items, .source_loc = SourceLoc.find(self.module.file.buffer, name.token_start) };

    return error.WithMessage;
}
