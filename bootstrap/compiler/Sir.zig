//! Syntax Intermediate Representation.
//!
//! An unchecked stack-based intermediate representation lowered down from `Token`s.
//! Passed to `Sema` which checks all the instructions and types to be valid and lowers it down to `Air`.
//! And then `Air` gets lowered down to machine code.

const std = @import("std");

const Compilation = @import("Compilation.zig");
const Lexer = @import("Lexer.zig");
const Symbol = @import("Symbol.zig");
const Scope = Symbol.Scope;
const Type = Symbol.Type;
const Range = @import("Range.zig");
const Token = @import("Token.zig");

const Sir = @This();

global_assembly: std.ArrayListUnmanaged(u8) = .{},

instructions: std.ArrayListUnmanaged(Instruction) = .{},

variables: std.StringArrayHashMapUnmanaged(Variable) = .{},
constants: std.StringArrayHashMapUnmanaged(Constant) = .{},

pub const Variable = struct {
    /// The start of where this variable has been defined at, inside of the source code
    token_start: u32,
    /// A range of instructions that when evaluated by the semantic analyzer, it would get this variable's type, if this is empty then semantic
    /// analyzer should try inferring the type
    type_instructions: Range,
    /// A range of instructions that when evaluated by the semantic analyzer, it would get this variable's value
    value_instructions: Range,
};

pub const Constant = struct {
    /// The start of where this constant has been defined at, inside of the source code
    token_start: u32,
    /// A range of instructions that when evaluated by the semantic analyzer, it would get this constant's value
    value_instructions: Range,
};

pub const Instruction = union(enum) {
    /// Duplicate the top of the stack
    duplicate,
    /// Reverse the stack into nth depth
    reverse: u32,
    /// Same as `reverse` but don't emit air instruction
    comptime_reverse: u32,
    /// Pop the top of the stack
    pop,
    /// Push a string onto the stack
    string: Range,
    /// Push an integer onto the stack
    int: u32,
    /// Push a float onto the stack
    float: f64,
    /// Push a function value onto stack, function type should be on the stack
    function: Function,
    /// Push an array type onto stack, array length and child type should be on the stack
    array_type: u32,
    /// Push a pointer type onto stack, child type should be on the stack
    pointer_type: PointerType,
    /// Push a struct type onto stack, provides the name of each field and the type of each field should be on the stack
    struct_type: StructType,
    /// Push an enum type onto stack, provides the fields and the type of its fields should be on the stack, if it isn't on the stack
    /// then the semantic analyzer should try inferring the type
    enum_type: EnumType,
    /// Push a function type onto stack, return type and parameter types should be on the stack
    function_type: FunctionType,
    /// Declare function parameters, type of these parameters should already be known in the function type
    parameters: []Name,
    /// Negate an integer or float on the top of the stack
    negate: u32,
    /// Reverse a boolean from true to false and from false to true
    bool_not: u32,
    /// Perform bitwise NOT operation on the bits of rhs (Which is to reverse its bits representation)
    bit_not: u32,
    /// Perform bitwise AND operation on the bits of lhs and rhs
    bit_and: u32,
    /// Perform bitwise OR operation on the bits of lhs and rhs
    bit_or: u32,
    /// Perform bitwise XOR operation on the bits of lhs and rhs
    bit_xor: u32,
    /// Reference a value on the stack
    /// 1. If the value is a comptime value, it will be hoisted into a global variable and then the pointer of the global variable
    /// will be on the stack
    /// 2. If the value is a runtime value, it will be in a local variable and then the pointer of the local variable will be on the stack
    /// 3. If the previous instruction is a `read` air instruction, it will be removed and the pointer of the value will be on the stack
    reference: u32,
    /// Override the data that the pointer is pointing to
    write: u32,
    /// Read the data that the pointer is pointing to
    read: u32,
    /// Add two integers or floats or pointers on the top of the stack
    add: u32,
    /// Subtract two integers or floats or pointers on the top of the stack
    sub: u32,
    /// Multiply two integers or floats on the top of the stack
    mul: u32,
    /// Divide two integers or floats on the top of the stack
    div: u32,
    /// Remainder of two integers or floats on the top of the stack
    rem: u32,
    /// Compare between two integers or floats on the stack and check for order (in this case, lhs less than rhs)
    lt: u32,
    /// Compare between two integers or floats on the stack and check for order (in this case, lhs greater than rhs)
    gt: u32,
    /// Compare between two values on the stack and check for equality
    eql: u32,
    /// Shift to left the bits of lhs using rhs offset
    shl: u32,
    /// Shift to right the bits of lhs using rhs offset
    shr: u32,
    /// Cast a value to a different type, both the value and its `to` type should already be on the stack
    cast: u32,
    /// Import a module and push it on the stack, also pops the module file path from the stack
    import: u32,
    /// Place a machine-specific inline assembly in the output
    inline_assembly: InlineAssembly,
    /// Call a function pointer on the stack
    call: Call,
    /// Declare a variable that is only known at runtime and doesn't get replaced by the compiler, type of this variable should already be
    /// on top of the stack which have to be popped before `set` instruction is executed
    variable: Name,
    /// Same as `variable` but the type is not on the top of the stack, and should be inferred by the semantic analyzer
    variable_infer: Name,
    /// Define a constant that is replaced at compile time and acts as a placeholder for a value
    /// Pops the value from the stack and doesn't need `set` instruction after it unlike `variable` which doesn't pop the value
    /// and needs `set` instruction after it
    constant: Name,
    /// Set a variable with a value on top of the stack
    set: Name,
    /// Get a value of a variable
    get: Name,
    /// Should be used before parsing the index of element access (i.e array[index]) or slicing (i.e array[start..end])
    pre_get_element: u32,
    /// Get an element in a "size many" pointer
    get_element: u32,
    /// Get a field in a struct
    get_field: Name,
    /// Make a new slice out of a "size many" pointer
    make_slice: u32,
    /// Start a new block
    block: u32,
    /// Unconditionally branch to a block
    br: u32,
    /// Conditionally branch to a block, condition is on the stack
    cond_br: CondBr,
    /// Switch on value to branch to a block
    @"switch": Switch,
    /// Start a new scope
    start_scope,
    /// End a scope
    end_scope,
    /// Return out of the function with a value on the stack
    ret: u32,
    /// Return out of the function without a value
    ret_void: u32,

    pub const PointerType = struct {
        size: Type.Pointer.Size,
        is_const: bool,
        token_start: u32,
    };

    pub const StructType = struct {
        fields: []Name,
        token_start: u32,
    };

    pub const EnumType = struct {
        fields: []Type.Enum.Field,
        token_start: u32,
    };

    pub const FunctionType = struct {
        parameters_count: usize,
        is_var_args: bool,
        token_start: u32,
    };

    pub const Function = struct {
        maybe_named: ?Name = null,
        maybe_foreign: ?Range = null,
        instructions: []Instruction,
        token_start: u32,
    };

    pub const InlineAssembly = struct {
        content: []const u8,
        /// If output_constraint is not null, its type would be on the stack before this instruction
        output_constraint: ?Range,
        input_constraints: []Range,
        clobbers: []Range,
        token_start: u32,
    };

    pub const Call = struct {
        arguments_count: usize,
        token_start: u32,
    };

    pub const CondBr = struct {
        true_id: u32,
        false_id: u32,
        token_start: u32,
    };

    pub const Switch = struct {
        case_block_ids: []u32,
        case_token_starts: []u32,
        else_block_id: u32,
        token_start: u32,
    };
};

pub const SourceLoc = struct {
    line: u32 = 1,
    column: u32 = 1,

    pub fn find(buffer: []const u8, start: u32) SourceLoc {
        var source_loc: SourceLoc = .{};

        var line_start: usize = 0;

        while (std.mem.indexOfScalarPos(u8, buffer, line_start, '\n')) |i| {
            if (i >= start) break;
            source_loc.line += 1;
            line_start = i + 1;
        }

        for (buffer[line_start..], line_start..) |c, i| {
            if (i >= start) break;
            source_loc.line += @intFromBool(c == '\n');
            source_loc.column += 1;
        }

        return source_loc;
    }
};

pub const Name = struct {
    buffer: []const u8,
    token_start: u32,
};

pub const Parser = struct {
    allocator: std.mem.Allocator,

    compilation: *Compilation,

    file: Compilation.File,

    tokens: std.MultiArrayList(Token).Slice,
    token_index: u32 = 0,

    sir: Sir = .{},

    target_os_int_id: u32,
    target_arch_int_id: u32,
    target_abi_int_id: u32,

    block_id: u32 = 0,

    defer_stack: std.ArrayListUnmanaged([]Instruction) = .{},
    scope_defer_count: u32 = 0,

    error_info: ?ErrorInfo = null,

    pub const ErrorInfo = struct {
        message: []const u8,
        source_loc: SourceLoc,
    };

    pub const Error = error{ WithMessage, WithoutMessage } || std.mem.Allocator.Error;

    pub fn init(allocator: std.mem.Allocator, compilation: *Compilation, file: Compilation.File) std.mem.Allocator.Error!Parser {
        var tokens: std.MultiArrayList(Token) = .{};

        // Tokens should have a lowering rate of 2 to 1, we use that estimate to avoid reallocation
        try tokens.ensureTotalCapacity(allocator, file.buffer.len / 2);

        var lexer = Lexer.init(file.buffer);

        while (true) {
            const token = lexer.next();

            try tokens.append(allocator, token);

            if (token.tag == .eof) break;
        }

        return Parser{
            .allocator = allocator,
            .compilation = compilation,
            .file = file,
            .tokens = tokens.toOwnedSlice(),
            .target_os_int_id = try compilation.putInt(@intCast(@intFromEnum(compilation.env.target.os.tag))),
            .target_arch_int_id = try compilation.putInt(@intCast(@intFromEnum(compilation.env.target.cpu.arch))),
            .target_abi_int_id = try compilation.putInt(@intCast(@intFromEnum(compilation.env.target.abi))),
        };
    }

    pub fn deinit(self: *Parser) void {
        self.tokens.deinit(self.allocator);
    }

    fn advance(self: *Parser) void {
        self.token_index += 1;
    }

    fn tokenTag(self: Parser) Token.Tag {
        return self.tokens.items(.tag)[self.token_index];
    }

    fn tokenRange(self: Parser) Range {
        return self.tokens.items(.range)[self.token_index];
    }

    fn tokenValue(self: Parser) []const u8 {
        const range = self.tokenRange();
        return self.file.buffer[range.start..range.end];
    }

    fn eat(self: *Parser, tag: Token.Tag) bool {
        if (self.tokenTag() == tag) {
            self.advance();

            return true;
        } else {
            return false;
        }
    }

    pub fn parse(self: *Parser) Error!void {
        while (self.tokenTag() != .eof) {
            switch (self.tokenTag()) {
                .identifier => try self.parseUnit(true),
                .keyword_asm => try self.parseGlobalAssembly(),

                else => {
                    self.error_info = .{ .message = "expected a name or 'asm' keyword", .source_loc = SourceLoc.find(self.file.buffer, self.tokenRange().start) };

                    return error.WithMessage;
                },
            }
        }
    }

    fn parseName(self: *Parser) Error!Name {
        if (self.tokenTag() != .identifier) {
            self.error_info = .{ .message = "expected a name", .source_loc = SourceLoc.find(self.file.buffer, self.tokenRange().start) };

            return error.WithMessage;
        }

        const token_range = self.tokenRange();
        const token_value = self.tokenValue();

        self.advance();

        return Name{ .buffer = token_value, .token_start = token_range.start };
    }

    fn parseUnit(self: *Parser, comptime top_level: bool) Error!void {
        const name = try self.parseName();

        if (self.sir.constants.get(name.buffer) != null) try self.reportRedeclaration(name);
        if (self.sir.variables.get(name.buffer) != null) try self.reportRedeclaration(name);

        switch (self.tokenTag()) {
            .colon => {
                self.advance();

                var type_instructions: Range = .{ .start = @intCast(self.sir.instructions.items.len), .end = 0 };

                try self.parseExprA(.lowest);

                type_instructions.end = @intCast(self.sir.instructions.items.len);

                if (self.eat(.semicolon)) {
                    if (top_level) {
                        return self.sir.variables.put(self.allocator, name.buffer, .{
                            .token_start = name.token_start,
                            .type_instructions = type_instructions,
                            .value_instructions = .{ .start = 0, .end = 0 },
                        });
                    } else {
                        return self.sir.instructions.append(self.allocator, .{ .variable = name });
                    }
                }

                if (!self.eat(.assign)) {
                    self.error_info = .{ .message = "expected '=' or ';'", .source_loc = SourceLoc.find(self.file.buffer, self.tokenRange().start) };

                    return error.WithMessage;
                }

                var value_instructions: Range = .{ .start = @intCast(self.sir.instructions.items.len), .end = 0 };

                try self.parseExpr(.lowest);

                if (self.sir.instructions.getLast() == .function) {
                    self.sir.instructions.items[self.sir.instructions.items.len - 1].function.maybe_named = name;
                }

                value_instructions.end = @intCast(self.sir.instructions.items.len);

                if (!self.eat(.semicolon)) {
                    self.error_info = .{ .message = "expected ';'", .source_loc = SourceLoc.find(self.file.buffer, self.tokenRange().start) };

                    return error.WithMessage;
                }

                if (top_level) {
                    try self.sir.variables.put(self.allocator, name.buffer, .{
                        .token_start = name.token_start,
                        .type_instructions = type_instructions,
                        .value_instructions = value_instructions,
                    });
                } else {
                    try self.sir.instructions.appendSlice(self.allocator, &.{
                        .{ .reverse = 2 },
                        .{ .variable = name },
                        .{ .set = name },
                    });
                }
            },

            .double_colon => {
                self.advance();

                var value_instructions: Range = .{ .start = @intCast(self.sir.instructions.items.len), .end = 0 };

                try self.parseExpr(.lowest);

                if (self.sir.instructions.getLast() == .function) {
                    self.sir.instructions.items[self.sir.instructions.items.len - 1].function.maybe_named = name;
                }

                value_instructions.end = @intCast(self.sir.instructions.items.len);

                if (!self.eat(.semicolon) and self.tokens.items(.tag)[self.token_index - 1] != .close_brace) {
                    self.error_info = .{ .message = "expected ';'", .source_loc = SourceLoc.find(self.file.buffer, self.tokenRange().start) };

                    return error.WithMessage;
                }

                if (top_level) {
                    try self.sir.constants.put(self.allocator, name.buffer, .{
                        .token_start = name.token_start,
                        .value_instructions = value_instructions,
                    });
                } else {
                    try self.sir.instructions.append(self.allocator, .{ .constant = name });
                }
            },

            .colon_assign => {
                self.advance();

                var value_instructions: Range = .{ .start = @intCast(self.sir.instructions.items.len), .end = 0 };

                try self.parseExpr(.lowest);

                if (self.sir.instructions.getLast() == .function) {
                    self.sir.instructions.items[self.sir.instructions.items.len - 1].function.maybe_named = name;
                }

                value_instructions.end = @intCast(self.sir.instructions.items.len);

                if (!self.eat(.semicolon)) {
                    self.error_info = .{ .message = "expected ';'", .source_loc = SourceLoc.find(self.file.buffer, self.tokenRange().start) };

                    return error.WithMessage;
                }

                if (top_level) {
                    try self.sir.variables.put(self.allocator, name.buffer, .{
                        .token_start = name.token_start,
                        .type_instructions = .{ .start = 0, .end = 0 },
                        .value_instructions = value_instructions,
                    });
                } else {
                    try self.sir.instructions.appendSlice(self.allocator, &.{
                        .{ .variable_infer = name },
                        .{ .set = name },
                    });
                }
            },

            else => {
                self.error_info = .{ .message = "expected '::' or ':=' or ':'", .source_loc = SourceLoc.find(self.file.buffer, self.tokenRange().start) };

                return error.WithMessage;
            },
        }
    }

    fn parseGlobalAssembly(self: *Parser) Error!void {
        self.advance();

        if (!self.eat(.open_brace)) {
            self.error_info = .{ .message = "expected a '{'", .source_loc = SourceLoc.find(self.file.buffer, self.tokenRange().start) };

            return error.WithMessage;
        }

        while (!self.eat(.close_brace)) {
            if (self.tokenTag() != .string_literal) {
                self.error_info = .{ .message = "expected a valid string", .source_loc = SourceLoc.find(self.file.buffer, self.tokenRange().start) };

                return error.WithMessage;
            }

            const content = self.tokenValue();
            const string_start = self.tokenRange().start;

            self.advance();

            unescape(self.allocator, &self.sir.global_assembly, content) catch |err| switch (err) {
                error.InvalidEscapeCharacter => {
                    self.error_info = .{ .message = "invalid escape character", .source_loc = SourceLoc.find(self.file.buffer, string_start) };

                    return error.WithMessage;
                },

                inline else => |other_err| return other_err,
            };

            try self.sir.global_assembly.append(self.allocator, '\n');
        }
    }

    fn parseStmt(self: *Parser, expect_semicolon: bool) Error!void {
        switch (self.tokenTag()) {
            .identifier => switch (self.tokens.items(.tag)[self.token_index + 1]) {
                .colon, .double_colon, .colon_assign => return self.parseUnit(false),

                else => {
                    try self.parseExpr(.lowest);

                    try self.sir.instructions.append(self.allocator, .pop);
                },
            },

            .keyword_switch => return self.parseSwitch(),

            .keyword_if => return self.parseConditional(),

            .keyword_while => return self.parseWhileLoop(),

            .keyword_break => try self.parseBreak(),

            .keyword_continue => try self.parseContinue(),

            .keyword_defer => return self.parseDefer(),

            .keyword_return => try self.parseReturn(),

            .open_brace => {
                try self.sir.instructions.append(self.allocator, .start_scope);

                try self.parseBody();

                try self.sir.instructions.append(self.allocator, .end_scope);

                return;
            },

            else => {
                try self.parseExpr(.lowest);

                try self.sir.instructions.append(self.allocator, .pop);
            },
        }

        if (expect_semicolon and !self.eat(.semicolon)) {
            self.error_info = .{ .message = "expected a ';'", .source_loc = SourceLoc.find(self.file.buffer, self.tokenRange().start) };

            return error.WithMessage;
        }
    }

    fn popScopeDefers(self: *Parser) Error!void {
        while (self.scope_defer_count > 0) : (self.scope_defer_count -= 1) {
            const defer_instructions = self.defer_stack.pop();
            try self.sir.instructions.appendSlice(self.allocator, defer_instructions);
            self.allocator.free(defer_instructions);
        }
    }

    fn parseSwitch(self: *Parser) Error!void {
        const switch_keyword_start = self.tokenRange().start;

        self.advance();

        try self.parseExpr(.lowest);

        if (!self.eat(.open_brace)) {
            self.error_info = .{ .message = "expected a '{'", .source_loc = SourceLoc.find(self.file.buffer, self.tokenRange().start) };

            return error.WithMessage;
        }

        var case_block_ids: std.ArrayListUnmanaged(u32) = .{};
        var case_token_starts: std.ArrayListUnmanaged(u32) = .{};
        var case_instructions: std.ArrayListUnmanaged(Instruction) = .{};

        var maybe_else_block_id: ?u32 = null;

        const end_block_id = self.block_id;
        self.block_id += 1;

        while (self.tokenTag() != .eof and self.tokenTag() != .close_brace) {
            const case_block_id = self.block_id;
            self.block_id += 1;

            if (self.tokenTag() == .keyword_else) {
                const else_keyword_start = self.tokenRange().start;

                self.advance();

                if (maybe_else_block_id != null) {
                    self.error_info = .{ .message = "duplicate switch case", .source_loc = SourceLoc.find(self.file.buffer, else_keyword_start) };

                    return error.WithMessage;
                }

                maybe_else_block_id = case_block_id;
            } else {
                try case_block_ids.append(self.allocator, case_block_id);
                try case_token_starts.append(self.allocator, self.tokenRange().start);

                try self.parseExpr(.lowest);

                if (self.eat(.comma)) {
                    while (self.tokenTag() != .eof and self.tokenTag() != .fat_arrow) {
                        try case_block_ids.append(self.allocator, case_block_id);
                        try case_token_starts.append(self.allocator, self.tokenRange().start);

                        try self.parseExpr(.lowest);

                        if (!self.eat(.comma) and self.tokenTag() != .fat_arrow) {
                            self.error_info = .{ .message = "expected a ','", .source_loc = SourceLoc.find(self.file.buffer, self.tokenRange().start) };

                            return error.WithMessage;
                        }
                    }
                }
            }

            if (!self.eat(.fat_arrow)) {
                self.error_info = .{ .message = "expected a '=>'", .source_loc = SourceLoc.find(self.file.buffer, self.tokenRange().start) };

                return error.WithMessage;
            }

            try case_instructions.append(self.allocator, .{ .block = case_block_id });

            const previous_sir_instructions = self.sir.instructions;
            self.sir.instructions = case_instructions;

            if (self.tokenTag() == .open_brace) {
                try self.sir.instructions.append(self.allocator, .start_scope);

                try self.parseBody();

                try self.sir.instructions.append(self.allocator, .end_scope);
            } else {
                const previous_scope_defer_count = self.scope_defer_count;
                self.scope_defer_count = 0;

                try self.parseStmt(false);

                try self.popScopeDefers();
                self.scope_defer_count = previous_scope_defer_count;
            }

            try self.sir.instructions.append(self.allocator, .{ .br = end_block_id });

            case_instructions = self.sir.instructions;
            self.sir.instructions = previous_sir_instructions;

            if (!self.eat(.comma) and self.tokenTag() != .close_brace) {
                self.error_info = .{ .message = "expected a ','", .source_loc = SourceLoc.find(self.file.buffer, self.tokenRange().start) };

                return error.WithMessage;
            }
        }

        if (!self.eat(.close_brace)) {
            self.error_info = .{ .message = "expected a '}'", .source_loc = SourceLoc.find(self.file.buffer, self.tokenRange().start) };

            return error.WithMessage;
        }

        const owned_case_block_ids = try case_block_ids.toOwnedSlice(self.allocator);
        const owned_case_token_starts = try case_token_starts.toOwnedSlice(self.allocator);

        try self.sir.instructions.append(self.allocator, .{ .reverse = @intCast(owned_case_block_ids.len + 1) });

        if (maybe_else_block_id) |else_block_id| {
            try self.sir.instructions.append(self.allocator, .{
                .@"switch" = .{
                    .case_block_ids = owned_case_block_ids,
                    .case_token_starts = owned_case_token_starts,
                    .else_block_id = else_block_id,
                    .token_start = switch_keyword_start,
                },
            });
        } else {
            self.error_info = .{ .message = "unhandled switch cases (note: missing an 'else' case)", .source_loc = SourceLoc.find(self.file.buffer, switch_keyword_start) };

            return error.WithMessage;
        }

        try self.sir.instructions.appendSlice(self.allocator, case_instructions.items);

        case_instructions.deinit(self.allocator);

        try self.sir.instructions.append(self.allocator, .{ .block = end_block_id });
    }

    fn parseConditional(self: *Parser) Error!void {
        const end_block_id = self.block_id;
        self.block_id += 1;

        try self.sir.instructions.append(self.allocator, .{ .br = self.block_id });

        while (true) {
            const if_keyword_start = self.tokenRange().start;

            self.advance();

            try self.sir.instructions.append(self.allocator, .{ .block = self.block_id });
            self.block_id += 1;

            try self.parseExpr(.lowest);

            try self.sir.instructions.append(self.allocator, .{
                .cond_br = .{
                    .true_id = self.block_id,
                    .false_id = undefined,
                    .token_start = if_keyword_start,
                },
            });

            const cond_br_index = self.sir.instructions.items.len - 1;

            try self.sir.instructions.append(self.allocator, .{ .block = self.block_id });
            self.block_id += 1;

            try self.sir.instructions.append(self.allocator, .start_scope);

            try self.parseBody();

            self.sir.instructions.items[cond_br_index].cond_br.false_id = self.block_id;

            try self.sir.instructions.append(self.allocator, .{ .br = end_block_id });

            try self.sir.instructions.append(self.allocator, .end_scope);

            if (self.eat(.keyword_else)) {
                if (self.tokenTag() == .keyword_if) continue;

                try self.sir.instructions.append(self.allocator, .{ .block = self.block_id });
                self.block_id += 1;

                try self.sir.instructions.append(self.allocator, .start_scope);

                try self.parseBody();

                try self.sir.instructions.append(self.allocator, .{ .br = end_block_id });

                try self.sir.instructions.append(self.allocator, .end_scope);
            } else {
                try self.sir.instructions.append(self.allocator, .{ .block = self.block_id });
                self.block_id += 1;

                try self.sir.instructions.append(self.allocator, .{ .br = end_block_id });
            }

            break;
        }

        try self.sir.instructions.append(self.allocator, .{ .block = end_block_id });
    }

    var maybe_header_block_id: ?u32 = null;
    var maybe_end_block_id: ?u32 = null;

    fn parseWhileLoop(self: *Parser) Error!void {
        const while_keyword_start = self.tokenRange().start;

        self.advance();

        const header_block_id = self.block_id;
        self.block_id += 1;

        const previous_header_block_id = maybe_header_block_id;
        maybe_header_block_id = header_block_id;
        defer maybe_header_block_id = previous_header_block_id;

        const body_block_id = self.block_id;
        self.block_id += 1;

        const end_block_id = self.block_id;
        self.block_id += 1;

        const previous_end_block_id = maybe_end_block_id;
        maybe_end_block_id = end_block_id;
        defer maybe_end_block_id = previous_end_block_id;

        try self.sir.instructions.append(self.allocator, .{ .br = header_block_id });

        try self.sir.instructions.append(self.allocator, .{ .block = header_block_id });

        try self.parseExpr(.lowest);

        try self.sir.instructions.append(self.allocator, .{ .cond_br = .{
            .true_id = body_block_id,
            .false_id = end_block_id,
            .token_start = while_keyword_start,
        } });

        try self.sir.instructions.append(self.allocator, .{ .block = body_block_id });

        try self.sir.instructions.append(self.allocator, .start_scope);

        try self.parseBody();

        try self.sir.instructions.append(self.allocator, .{ .br = header_block_id });

        try self.sir.instructions.append(self.allocator, .end_scope);

        try self.sir.instructions.append(self.allocator, .{ .block = end_block_id });
    }

    fn parseContinue(self: *Parser) Error!void {
        const continue_keyword_start = self.tokenRange().start;

        self.advance();

        if (maybe_header_block_id) |header_block_id| {
            return self.sir.instructions.append(self.allocator, .{ .br = header_block_id });
        }

        self.error_info = .{ .message = "expected the continue statement to be inside a loop", .source_loc = SourceLoc.find(self.file.buffer, continue_keyword_start) };

        return error.WithMessage;
    }

    fn parseBreak(self: *Parser) Error!void {
        const break_keyword_start = self.tokenRange().start;

        self.advance();

        if (maybe_end_block_id) |end_block_id| {
            return self.sir.instructions.append(self.allocator, .{ .br = end_block_id });
        }

        self.error_info = .{ .message = "expected the break statement to be inside a loop", .source_loc = SourceLoc.find(self.file.buffer, break_keyword_start) };

        return error.WithMessage;
    }

    fn parseDefer(self: *Parser) Error!void {
        self.advance();

        const defer_instructions_start = self.sir.instructions.items.len;

        if (self.tokenTag() == .open_brace) {
            try self.sir.instructions.append(self.allocator, .start_scope);

            try self.parseBody();

            try self.sir.instructions.append(self.allocator, .end_scope);
        } else {
            const previous_scope_defer_count = self.scope_defer_count;
            self.scope_defer_count = 0;

            try self.parseStmt(true);

            try self.popScopeDefers();
            self.scope_defer_count = previous_scope_defer_count;
        }

        if (defer_instructions_start == self.sir.instructions.items.len) return;

        const defer_instructions = self.sir.instructions.items[defer_instructions_start..];
        const defer_instructions_on_heap = try self.allocator.alloc(Instruction, defer_instructions.len);
        @memcpy(defer_instructions_on_heap, defer_instructions);

        try self.defer_stack.append(self.allocator, defer_instructions_on_heap);
        self.scope_defer_count += 1;

        self.sir.instructions.shrinkRetainingCapacity(defer_instructions_start);
    }

    fn parseReturn(self: *Parser) Error!void {
        const return_keyword_start = self.tokenRange().start;

        self.advance();

        const returns_value = self.tokenTag() != .semicolon;

        if (returns_value) {
            try self.parseExpr(.lowest);
        }

        if (self.defer_stack.items.len > 0) {
            var i = self.defer_stack.items.len - 1;

            while (true) : (i -= 1) {
                const defer_instructions = self.defer_stack.items[i];
                try self.sir.instructions.appendSlice(self.allocator, defer_instructions);
                if (i == 0) break;
            }
        }

        self.defer_stack.shrinkRetainingCapacity(self.defer_stack.items.len - self.scope_defer_count);
        self.scope_defer_count = 0;

        if (returns_value) {
            try self.sir.instructions.append(self.allocator, .{ .ret = return_keyword_start });
        } else {
            try self.sir.instructions.append(self.allocator, .{ .ret_void = return_keyword_start });
        }
    }

    const Precedence = enum {
        lowest,
        assign,
        comparison,
        sum,
        product,
        bit_or,
        bit_xor,
        bit_and,
        shift,
        cast,
        prefix,
        call,
        subscript,
        field,

        fn fromTokenTag(tag: Token.Tag) Precedence {
            return switch (tag) {
                .plus_assign,
                .minus_assign,
                .star_assign,
                .divide_assign,
                .modulo_assign,
                .bit_and_assign,
                .bit_or_assign,
                .bit_xor_assign,
                .left_shift_assign,
                .right_shift_assign,
                .assign,
                => .assign,
                .less_than, .greater_than, .less_or_eql, .greater_or_eql, .eql, .not_eql => .comparison,
                .plus, .minus => .sum,
                .star, .divide, .modulo => .product,
                .bit_and => .bit_and,
                .bit_or => .bit_or,
                .bit_xor => .bit_xor,
                .left_shift, .right_shift => .shift,
                .keyword_as => .cast,
                .open_paren => .call,
                .open_bracket => .subscript,
                .period => .field,

                else => .lowest,
            };
        }
    };

    fn parseExprA(self: *Parser, precedence: Precedence) Error!void {
        try self.parseUnaryExpr();

        while (@intFromEnum(Precedence.fromTokenTag(self.tokenTag())) > @intFromEnum(precedence) and
            self.tokenTag() != .semicolon and self.tokenTag() != .assign)
        {
            try self.parseBinaryExpr();
        }
    }

    fn parseExpr(self: *Parser, precedence: Precedence) Error!void {
        try self.parseUnaryExpr();

        while (@intFromEnum(Precedence.fromTokenTag(self.tokenTag())) > @intFromEnum(precedence) and
            self.tokenTag() != .semicolon)
        {
            try self.parseBinaryExpr();
        }
    }

    fn parseUnaryExpr(self: *Parser) Error!void {
        switch (self.tokenTag()) {
            .identifier => try self.parseIdentifier(),

            .special_identifier => try self.parseSpecialIdentifier(),

            .string_literal => try self.parseString(),

            .char_literal => try self.parseChar(),

            .int => try self.parseInt(),

            .float => try self.parseFloat(),

            .keyword_struct => try self.parseStructType(),

            .keyword_enum => try self.parseEnumType(),

            .keyword_fn => try self.parseFunction(),

            .open_bracket => try self.parsePointerType(true),
            .star => try self.parsePointerType(false),

            .open_paren => try self.parseParentheses(),

            .keyword_asm => return self.parseInlineAssembly(),

            .minus, .bool_not, .bit_not, .bit_and => try self.parseUnaryOperation(),

            else => {
                self.error_info = .{ .message = "unknown expression", .source_loc = SourceLoc.find(self.file.buffer, self.tokenRange().start) };

                return error.WithMessage;
            },
        }
    }

    fn parseIdentifier(self: *Parser) Error!void {
        try self.sir.instructions.append(self.allocator, .{ .get = try self.parseName() });
    }

    fn parseSpecialIdentifier(self: *Parser) Error!void {
        const token_value = self.tokenValue();
        const token_start = self.tokenRange().start;

        self.advance();

        if (std.mem.eql(u8, token_value, "import")) {
            if (!self.eat(.open_paren)) {
                self.error_info = .{ .message = "expected a '('", .source_loc = SourceLoc.find(self.file.buffer, self.tokenRange().start) };

                return error.WithMessage;
            }

            try self.parseExpr(.lowest);

            if (!self.eat(.close_paren)) {
                self.error_info = .{ .message = "expected a ')'", .source_loc = SourceLoc.find(self.file.buffer, self.tokenRange().start) };

                return error.WithMessage;
            }

            try self.sir.instructions.append(self.allocator, .{ .import = token_start });
        } else if (std.mem.eql(u8, token_value, "target_os")) {
            try self.sir.instructions.append(self.allocator, .{ .int = self.target_os_int_id });
        } else if (std.mem.eql(u8, token_value, "target_arch")) {
            try self.sir.instructions.append(self.allocator, .{ .int = self.target_arch_int_id });
        } else if (std.mem.eql(u8, token_value, "target_abi")) {
            try self.sir.instructions.append(self.allocator, .{ .int = self.target_abi_int_id });
        } else {
            self.error_info = .{ .message = "unknown special identifier", .source_loc = SourceLoc.find(self.file.buffer, token_start) };

            return error.WithMessage;
        }
    }

    const UnescapeError = error{InvalidEscapeCharacter} || std.mem.Allocator.Error;

    fn unescape(allocator: std.mem.Allocator, output: *std.ArrayListUnmanaged(u8), input: []const u8) UnescapeError!void {
        try output.ensureUnusedCapacity(allocator, input.len);

        var unescaping = false;

        for (input) |input_char| {
            switch (unescaping) {
                false => switch (input_char) {
                    '\\' => unescaping = true,

                    else => output.appendAssumeCapacity(input_char),
                },

                true => {
                    unescaping = false;

                    switch (input_char) {
                        '\\' => {
                            output.appendAssumeCapacity('\\');
                        },

                        'n' => {
                            output.appendAssumeCapacity('\n');
                        },

                        'r' => {
                            output.appendAssumeCapacity('\r');
                        },

                        't' => {
                            output.appendAssumeCapacity('\t');
                        },

                        'e' => {
                            output.appendAssumeCapacity(27);
                        },

                        'v' => {
                            output.appendAssumeCapacity(11);
                        },

                        'b' => {
                            output.appendAssumeCapacity(8);
                        },

                        'f' => {
                            output.appendAssumeCapacity(20);
                        },

                        '"' => {
                            output.appendAssumeCapacity('"');
                        },

                        '\'' => {
                            output.appendAssumeCapacity('\'');
                        },

                        else => return error.InvalidEscapeCharacter,
                    }
                },
            }
        }
    }

    fn parseString(self: *Parser) Error!void {
        const content = self.tokenValue();
        const string_start = self.tokenRange().start;

        self.advance();

        const string_bytes_start: u32 = @intCast(self.compilation.pool.bytes.items.len);

        unescape(self.allocator, &self.compilation.pool.bytes, content) catch |err| switch (err) {
            error.InvalidEscapeCharacter => {
                self.error_info = .{ .message = "invalid escape character", .source_loc = SourceLoc.find(self.file.buffer, string_start) };

                return error.WithMessage;
            },

            inline else => |other_err| return other_err,
        };

        const string_bytes_end: u32 = @intCast(self.compilation.pool.bytes.items.len);

        try self.sir.instructions.append(self.allocator, .{ .string = .{ .start = string_bytes_start, .end = string_bytes_end } });
    }

    fn parseChar(self: *Parser) Error!void {
        const content = self.tokenValue();
        const char_start = self.tokenRange().start;

        self.advance();

        var unescaped: std.ArrayListUnmanaged(u8) = .{};
        defer unescaped.deinit(self.allocator);

        unescape(self.allocator, &unescaped, content) catch |err| switch (err) {
            error.InvalidEscapeCharacter => {
                self.error_info = .{ .message = "invalid escape character", .source_loc = SourceLoc.find(self.file.buffer, char_start) };

                return error.WithMessage;
            },

            inline else => |other_err| return other_err,
        };

        const decoded = switch (unescaped.items.len) {
            1 => unescaped.items[0],
            2 => std.unicode.utf8Decode2(unescaped.items[0..2].*),
            3 => std.unicode.utf8Decode3(unescaped.items[0..3].*),
            4 => std.unicode.utf8Decode4(unescaped.items[0..4].*),
            else => error.TooMuchCodes,
        } catch {
            self.error_info = .{ .message = "invalid character literal", .source_loc = SourceLoc.find(self.file.buffer, char_start) };

            return error.WithMessage;
        };

        try self.sir.instructions.append(self.allocator, .{ .int = try self.compilation.putInt(decoded) });
    }

    fn parseInt(self: *Parser) Error!void {
        const value = std.fmt.parseInt(i128, self.tokenValue(), 0) catch {
            self.error_info = .{ .message = "invalid number", .source_loc = SourceLoc.find(self.file.buffer, self.tokenRange().start) };

            return error.WithMessage;
        };

        self.advance();

        try self.sir.instructions.append(self.allocator, .{ .int = try self.compilation.putInt(value) });
    }

    fn parseFloat(self: *Parser) Error!void {
        const value = std.fmt.parseFloat(f64, self.tokenValue()) catch {
            self.error_info = .{ .message = "invalid number", .source_loc = SourceLoc.find(self.file.buffer, self.tokenRange().start) };

            return error.WithMessage;
        };

        self.advance();

        try self.sir.instructions.append(self.allocator, .{ .float = value });
    }

    fn parseStructType(self: *Parser) Error!void {
        const struct_keyword_start = self.tokenRange().start;

        self.advance();

        if (!self.eat(.open_brace)) {
            self.error_info = .{ .message = "expected a '{'", .source_loc = SourceLoc.find(self.file.buffer, self.tokenRange().start) };

            return error.WithMessage;
        }

        var fields: std.StringArrayHashMapUnmanaged(Name) = .{};

        while (!self.eat(.close_brace)) {
            const field_name = try self.parseName();

            if (fields.get(field_name.buffer) != null) {
                var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

                try error_message_buf.writer(self.allocator).print("redeclaration of '{s}' in struct fields", .{field_name.buffer});

                self.error_info = .{ .message = error_message_buf.items, .source_loc = SourceLoc.find(self.file.buffer, field_name.token_start) };
            }

            if (!self.eat(.colon)) {
                self.error_info = .{ .message = "expected a ':'", .source_loc = SourceLoc.find(self.file.buffer, self.tokenRange().start) };

                return error.WithMessage;
            }

            try self.parseExpr(.lowest);

            if (!self.eat(.comma) and self.tokenTag() != .close_brace) {
                self.error_info = .{ .message = "expected a ','", .source_loc = SourceLoc.find(self.file.buffer, self.tokenRange().start) };

                return error.WithMessage;
            }

            try fields.put(self.allocator, field_name.buffer, field_name);
        }

        try self.sir.instructions.append(self.allocator, .{ .comptime_reverse = @intCast(fields.count()) });

        try self.sir.instructions.append(self.allocator, .{
            .struct_type = .{
                .fields = fields.values(),
                .token_start = struct_keyword_start,
            },
        });
    }

    fn parseEnumType(self: *Parser) Error!void {
        const enum_keyword_start = self.tokenRange().start;

        self.advance();

        if (!self.eat(.open_brace)) {
            try self.parseExpr(.lowest);

            if (!self.eat(.open_brace)) {
                self.error_info = .{ .message = "expected a '{'", .source_loc = SourceLoc.find(self.file.buffer, self.tokenRange().start) };

                return error.WithMessage;
            }
        }

        var fields: std.StringArrayHashMapUnmanaged(Type.Enum.Field) = .{};

        while (!self.eat(.close_brace)) {
            const field_name = try self.parseName();

            if (fields.get(field_name.buffer) != null) {
                var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

                try error_message_buf.writer(self.allocator).print("redeclaration of '{s}' in enum fields", .{field_name.buffer});

                self.error_info = .{ .message = error_message_buf.items, .source_loc = SourceLoc.find(self.file.buffer, field_name.token_start) };
            }

            var field_int_id = if (self.tokenTag() == .assign)
                0
            else if (fields.count() == 0)
                try self.compilation.putInt(0)
            else
                try self.compilation.putInt(self.compilation.getIntFromId(fields.values()[fields.count() - 1].int_id) + 1);

            if (self.eat(.assign)) {
                if (self.tokenTag() != .int) {
                    self.error_info = .{ .message = "expected a valid integer", .source_loc = SourceLoc.find(self.file.buffer, self.tokenRange().start) };

                    return error.WithMessage;
                }

                try self.parseInt();

                field_int_id = self.sir.instructions.pop().int;
            }

            if (!self.eat(.comma) and self.tokenTag() != .close_brace) {
                self.error_info = .{ .message = "expected a ','", .source_loc = SourceLoc.find(self.file.buffer, self.tokenRange().start) };

                return error.WithMessage;
            }

            try fields.put(self.allocator, field_name.buffer, .{
                .name = field_name.buffer,
                .int_id = field_int_id,
            });
        }

        try self.sir.instructions.append(self.allocator, .{
            .enum_type = .{
                .fields = fields.values(),
                .token_start = enum_keyword_start,
            },
        });
    }

    fn parseFunction(self: *Parser) Error!void {
        const fn_keyword_start = self.tokenRange().start;

        self.advance();

        if (!self.eat(.open_paren)) {
            self.error_info = .{ .message = "expected a '('", .source_loc = SourceLoc.find(self.file.buffer, self.tokenRange().start) };

            return error.WithMessage;
        }

        var parameters: std.ArrayListUnmanaged(Name) = .{};

        var is_var_args = false;

        while (!self.eat(.close_paren)) {
            try parameters.append(self.allocator, try self.parseName());

            if (!self.eat(.colon)) {
                self.error_info = .{ .message = "expected a ':'", .source_loc = SourceLoc.find(self.file.buffer, self.tokenRange().start) };

                return error.WithMessage;
            }

            try self.parseExpr(.lowest);

            if (self.tokenTag() != .close_paren and !self.eat(.comma)) {
                self.error_info = .{ .message = "expected a ','", .source_loc = SourceLoc.find(self.file.buffer, self.tokenRange().start) };

                return error.WithMessage;
            }

            if (self.eat(.triple_period)) {
                is_var_args = true;

                if (!self.eat(.close_paren)) {
                    if (!self.eat(.colon)) {
                        self.error_info = .{ .message = "expected a ','", .source_loc = SourceLoc.find(self.file.buffer, self.tokenRange().start) };

                        return error.WithMessage;
                    }

                    if (!self.eat(.close_paren)) {
                        self.error_info = .{ .message = "expected a ')'", .source_loc = SourceLoc.find(self.file.buffer, self.tokenRange().start) };

                        return error.WithMessage;
                    }
                }

                break;
            }
        }

        parameters.shrinkAndFree(self.allocator, parameters.items.len);

        try self.sir.instructions.append(self.allocator, .{ .comptime_reverse = @intCast(parameters.items.len) });

        if (self.tokenTag() != .special_identifier and self.tokenTag() != .open_brace) {
            try self.parseExpr(.lowest);
        } else {
            try self.sir.instructions.append(self.allocator, .{ .get = .{ .buffer = "void", .token_start = 0 } });
        }

        try self.sir.instructions.append(self.allocator, .{
            .function_type = .{
                .parameters_count = parameters.items.len,
                .is_var_args = is_var_args,
                .token_start = fn_keyword_start,
            },
        });

        var maybe_foreign: ?Range = null;

        if (self.tokenTag() == .special_identifier) {
            if (std.mem.eql(u8, self.tokenValue(), "foreign")) {
                self.advance();

                if (!self.eat(.open_paren)) {
                    self.error_info = .{ .message = "expected a '('", .source_loc = SourceLoc.find(self.file.buffer, self.tokenRange().start) };

                    return error.WithMessage;
                }

                if (self.tokenTag() != .string_literal) {
                    self.error_info = .{ .message = "expected a valid string", .source_loc = SourceLoc.find(self.file.buffer, self.tokenRange().start) };

                    return error.WithMessage;
                }

                try self.parseString();

                if (!self.eat(.close_paren)) {
                    self.error_info = .{ .message = "expected a ')'", .source_loc = SourceLoc.find(self.file.buffer, self.tokenRange().start) };

                    return error.WithMessage;
                }

                maybe_foreign = self.sir.instructions.pop().string;
            } else {
                self.error_info = .{ .message = "unknown special identifier", .source_loc = SourceLoc.find(self.file.buffer, self.tokenRange().start) };

                return error.WithMessage;
            }
        }

        if (self.tokenTag() == .open_brace) {
            try self.sir.instructions.append(self.allocator, .{
                .function = .{
                    .maybe_foreign = maybe_foreign,
                    .instructions = &.{},
                    .token_start = fn_keyword_start,
                },
            });

            const instructions_start = self.sir.instructions.items.len;

            try self.sir.instructions.appendSlice(self.allocator, &.{
                .{ .block = 0 },
                .start_scope,
                .{ .parameters = parameters.items },
            });

            self.block_id = 1;

            try self.parseBody();

            const last_instruction = self.sir.instructions.getLast();

            if (last_instruction != .ret and last_instruction != .ret_void)
                try self.sir.instructions.append(self.allocator, .{ .ret_void = fn_keyword_start });

            try self.sir.instructions.append(self.allocator, .end_scope);

            self.sir.instructions.items[instructions_start - 1].function.instructions =
                try self.allocator.dupe(Instruction, self.sir.instructions.items[instructions_start..]);

            self.sir.instructions.shrinkRetainingCapacity(instructions_start);
        } else if (maybe_foreign) |foreign| {
            try self.sir.instructions.append(self.allocator, .{
                .function = .{
                    .maybe_foreign = foreign,
                    .instructions = &.{},
                    .token_start = fn_keyword_start,
                },
            });
        }
    }

    fn parsePointerType(self: *Parser, comptime open_bracket: bool) Error!void {
        const token_start = self.tokenRange().start;

        self.advance();

        var size: Type.Pointer.Size = .one;

        var is_array = false;

        if (open_bracket) {
            if (self.eat(.close_bracket)) {
                size = .slice;
            } else if (self.eat(.star)) {
                size = .many;

                if (!self.eat(.close_bracket)) {
                    self.error_info = .{ .message = "expected a ']'", .source_loc = SourceLoc.find(self.file.buffer, self.tokenRange().start) };

                    return error.WithMessage;
                }
            } else {
                is_array = true;

                try self.parseExpr(.lowest);

                if (!self.eat(.close_bracket)) {
                    self.error_info = .{ .message = "expected a ']'", .source_loc = SourceLoc.find(self.file.buffer, self.tokenRange().start) };

                    return error.WithMessage;
                }
            }
        }

        if (is_array) {
            try self.parseExpr(.lowest);

            try self.sir.instructions.append(self.allocator, .{ .array_type = token_start });
        } else {
            const is_const = self.eat(.keyword_const);

            try self.parseExpr(.lowest);

            try self.sir.instructions.append(self.allocator, .{
                .pointer_type = .{
                    .is_const = is_const,
                    .size = size,
                    .token_start = token_start,
                },
            });
        }
    }

    fn parseParentheses(self: *Parser) Error!void {
        self.advance();

        try self.parseExpr(.lowest);

        if (!self.eat(.close_paren)) {
            self.error_info = .{ .message = "expected a ')'", .source_loc = SourceLoc.find(self.file.buffer, self.tokenRange().start) };

            return error.WithMessage;
        }
    }

    fn parseInlineAssembly(self: *Parser) Error!void {
        const asm_keyword_start = self.tokenRange().start;

        self.advance();

        var content: std.ArrayListUnmanaged(u8) = .{};

        if (!self.eat(.open_brace)) {
            self.error_info = .{ .message = "expected a '{'", .source_loc = SourceLoc.find(self.file.buffer, self.tokenRange().start) };

            return error.WithMessage;
        }

        var input_constraints: []Range = &.{};
        var output_constraint: ?Range = null;
        var clobbers: []Range = &.{};

        while (!self.eat(.close_brace)) {
            if (self.tokenTag() != .string_literal) {
                self.error_info = .{ .message = "expected a valid string", .source_loc = SourceLoc.find(self.file.buffer, self.tokenRange().start) };

                return error.WithMessage;
            }

            try self.parseString();

            const string = self.sir.instructions.pop().string;

            try content.appendSlice(self.allocator, self.compilation.getStringFromRange(string));

            self.compilation.pool.bytes.shrinkRetainingCapacity(string.start);

            try content.append(self.allocator, '\n');

            if (self.eat(.colon)) {
                if (self.tokenTag() != .colon) {
                    output_constraint = try self.parseInlineAssemblyConstraint();
                }
            }

            if (self.eat(.colon)) {
                if (self.tokenTag() != .colon) {
                    input_constraints = try self.parseInlineAssemblyConstraints();
                }
            }

            if (self.eat(.colon)) {
                if (self.tokenTag() != .close_brace) {
                    clobbers = try self.parseInlineAssemblyClobbers();
                }
            }

            if (self.tokenTag() != .close_brace and (self.tokenTag() != .colon or self.tokenTag() != .string_literal)) {
                self.error_info = .{ .message = "expected a '}'", .source_loc = SourceLoc.find(self.file.buffer, self.tokenRange().start) };

                return error.WithMessage;
            }
        }

        content.shrinkAndFree(self.allocator, content.items.len);

        try self.sir.instructions.append(self.allocator, .{
            .inline_assembly = .{
                .content = content.items,
                .input_constraints = input_constraints,
                .output_constraint = output_constraint,
                .clobbers = clobbers,
                .token_start = asm_keyword_start,
            },
        });
    }

    fn parseInlineAssemblyConstraints(self: *Parser) Error![]Range {
        var constraints = std.ArrayList(Range).init(self.allocator);

        while (self.tokenTag() != .eof and self.tokenTag() != .colon and self.tokenTag() != .close_brace) {
            try constraints.append(try self.parseInlineAssemblyConstraint());

            if (!self.eat(.comma) and self.tokenTag() != .colon and self.tokenTag() != .close_brace) {
                self.error_info = .{ .message = "expected a ','", .source_loc = SourceLoc.find(self.file.buffer, self.tokenRange().start) };

                return error.WithMessage;
            }
        }

        try self.sir.instructions.append(self.allocator, .{ .reverse = @intCast(constraints.items.len) });

        return constraints.toOwnedSlice();
    }

    fn parseInlineAssemblyConstraint(self: *Parser) Error!Range {
        if (self.tokenTag() != .string_literal) {
            self.error_info = .{ .message = "expected a valid string", .source_loc = SourceLoc.find(self.file.buffer, self.tokenRange().start) };

            return error.WithMessage;
        }

        try self.parseString();

        const register = self.sir.instructions.pop().string;

        if (!self.eat(.open_paren)) {
            self.error_info = .{ .message = "expected a '('", .source_loc = SourceLoc.find(self.file.buffer, self.tokenRange().start) };

            return error.WithMessage;
        }

        try self.parseExpr(.lowest);

        if (!self.eat(.close_paren)) {
            self.error_info = .{ .message = "expected a ')'", .source_loc = SourceLoc.find(self.file.buffer, self.tokenRange().start) };

            return error.WithMessage;
        }

        return register;
    }

    fn parseInlineAssemblyClobbers(self: *Parser) Error![]Range {
        var clobbers = std.ArrayList(Range).init(self.allocator);

        while (self.tokenTag() != .eof and self.tokenTag() != .close_brace) {
            if (self.tokenTag() != .string_literal) {
                self.error_info = .{ .message = "expected a valid string", .source_loc = SourceLoc.find(self.file.buffer, self.tokenRange().start) };

                return error.WithMessage;
            }

            try self.parseString();

            const clobber = self.sir.instructions.pop().string;

            try clobbers.append(clobber);

            if (!self.eat(.comma) and self.tokenTag() != .close_brace) {
                self.error_info = .{ .message = "expected a ','", .source_loc = SourceLoc.find(self.file.buffer, self.tokenRange().start) };

                return error.WithMessage;
            }
        }

        return clobbers.toOwnedSlice();
    }

    fn parseUnaryOperation(self: *Parser) Error!void {
        const operator_tag = self.tokenTag();
        const operator_start = self.tokenRange().start;

        self.advance();

        try self.parseExpr(.prefix);

        switch (operator_tag) {
            .minus => {
                try self.sir.instructions.append(self.allocator, .{ .negate = operator_start });
            },

            .bool_not => {
                try self.sir.instructions.append(self.allocator, .{ .bool_not = operator_start });
            },

            .bit_not => {
                try self.sir.instructions.append(self.allocator, .{ .bit_not = operator_start });
            },

            .bit_and => {
                try self.sir.instructions.append(self.allocator, .{ .reference = operator_start });
            },

            else => unreachable,
        }
    }

    fn parseBinaryExpr(self: *Parser) Error!void {
        switch (self.tokenTag()) {
            .plus,
            .minus,
            .star,
            .divide,
            .modulo,
            .less_than,
            .greater_than,
            .less_or_eql,
            .greater_or_eql,
            .left_shift,
            .right_shift,
            .bit_and,
            .bit_or,
            .bit_xor,
            .plus_assign,
            .minus_assign,
            .star_assign,
            .divide_assign,
            .modulo_assign,
            .bit_and_assign,
            .bit_or_assign,
            .bit_xor_assign,
            .left_shift_assign,
            .right_shift_assign,
            .assign,
            .eql,
            .not_eql,
            => try self.parseBinaryOperation(),

            .keyword_as => try self.parseCast(),

            .open_paren => try self.parseCall(),

            .open_bracket => try self.parseSubscript(),

            .period => try self.parseFieldAccess(),

            else => {
                self.error_info = .{ .message = "unknown expression", .source_loc = SourceLoc.find(self.file.buffer, self.tokenRange().start) };

                return error.WithMessage;
            },
        }
    }

    fn parseBinaryOperation(self: *Parser) Error!void {
        const operator_tag = self.tokenTag();
        const operator_start = self.tokenRange().start;

        self.advance();

        const precedence = Precedence.fromTokenTag(operator_tag);

        if (precedence != .assign) {
            try self.parseExpr(precedence);
        }

        switch (operator_tag) {
            .plus_assign,
            .minus_assign,
            .star_assign,
            .divide_assign,
            .modulo_assign,
            .bit_and_assign,
            .bit_or_assign,
            .bit_xor_assign,
            .left_shift_assign,
            .right_shift_assign,
            .assign,
            => {
                const maybe_additional_operator: ?Token.Tag = if (operator_tag != .assign)
                    switch (operator_tag) {
                        .plus_assign => .plus,
                        .minus_assign => .minus,
                        .star_assign => .star,
                        .divide_assign => .divide,
                        .modulo_assign => .modulo,
                        .bit_and_assign => .bit_and,
                        .bit_or_assign => .bit_or,
                        .bit_xor_assign => .bit_xor,
                        .left_shift_assign => .left_shift,
                        .right_shift_assign => .right_shift,
                        else => unreachable,
                    }
                else
                    null;

                const last_instruction = self.sir.instructions.pop();

                if (last_instruction == .get_element or last_instruction == .get_field) {
                    try self.sir.instructions.appendSlice(self.allocator, &.{
                        last_instruction,
                        .{ .reference = operator_start },
                    });

                    try self.parseExpr(precedence);

                    if (maybe_additional_operator) |additional_operator| {
                        try self.sir.instructions.appendSlice(
                            self.allocator,
                            &.{
                                .{ .reverse = 2 },
                                .duplicate,
                                .{ .read = operator_start },
                                .{ .reverse = 2 },
                                .{ .reverse = 3 },
                            },
                        );

                        try self.emitBinaryOperation(additional_operator, operator_start);
                    }

                    try self.sir.instructions.appendSlice(
                        self.allocator,
                        &.{
                            .duplicate,
                            .{ .reverse = 3 },
                            .{ .write = operator_start },
                        },
                    );
                } else if (last_instruction == .read) {
                    try self.parseExpr(precedence);

                    if (maybe_additional_operator) |additional_operator_token| {
                        try self.sir.instructions.appendSlice(
                            self.allocator,
                            &.{
                                .{ .reverse = 2 },
                                .duplicate,
                                .{ .read = operator_start },
                                .{ .reverse = 2 },
                                .{ .reverse = 3 },
                            },
                        );

                        try self.emitBinaryOperation(additional_operator_token, operator_start);
                    }

                    try self.sir.instructions.appendSlice(
                        self.allocator,
                        &.{
                            .duplicate,
                            .{ .reverse = 3 },
                            .{ .write = operator_start },
                        },
                    );
                } else if (last_instruction == .get) {
                    try self.parseExpr(precedence);

                    if (maybe_additional_operator) |additional_operator_token| {
                        try self.sir.instructions.appendSlice(
                            self.allocator,
                            &.{
                                last_instruction,
                                .{ .reverse = 2 },
                            },
                        );

                        try self.emitBinaryOperation(additional_operator_token, operator_start);
                    }

                    try self.sir.instructions.appendSlice(
                        self.allocator,
                        &.{
                            .duplicate,
                            .{ .set = last_instruction.get },
                        },
                    );
                } else {
                    self.error_info = .{ .message = "cannot assign to value", .source_loc = SourceLoc.find(self.file.buffer, operator_start) };

                    return error.WithMessage;
                }
            },

            else => try self.emitBinaryOperation(operator_tag, operator_start),
        }
    }

    fn emitBinaryOperation(self: *Parser, operator_tag: Token.Tag, operator_start: u32) Error!void {
        switch (operator_tag) {
            .plus => try self.sir.instructions.append(self.allocator, .{ .add = operator_start }),
            .minus => try self.sir.instructions.append(self.allocator, .{ .sub = operator_start }),
            .star => try self.sir.instructions.append(self.allocator, .{ .mul = operator_start }),
            .divide => try self.sir.instructions.append(self.allocator, .{ .div = operator_start }),
            .modulo => try self.sir.instructions.append(self.allocator, .{ .rem = operator_start }),
            .less_than => try self.sir.instructions.append(self.allocator, .{ .lt = operator_start }),
            .greater_than => try self.sir.instructions.append(self.allocator, .{ .gt = operator_start }),
            .eql => try self.sir.instructions.append(self.allocator, .{ .eql = operator_start }),
            .left_shift => try self.sir.instructions.append(self.allocator, .{ .shl = operator_start }),
            .right_shift => try self.sir.instructions.append(self.allocator, .{ .shr = operator_start }),
            .bit_and => try self.sir.instructions.append(self.allocator, .{ .bit_and = operator_start }),
            .bit_or => try self.sir.instructions.append(self.allocator, .{ .bit_or = operator_start }),
            .bit_xor => try self.sir.instructions.append(self.allocator, .{ .bit_xor = operator_start }),

            .not_eql => {
                try self.sir.instructions.append(self.allocator, .{ .eql = operator_start });
                try self.sir.instructions.append(self.allocator, .{ .bool_not = operator_start });
            },

            .less_or_eql => {
                try self.sir.instructions.append(self.allocator, .{ .gt = operator_start });
                try self.sir.instructions.append(self.allocator, .{ .bool_not = operator_start });
            },

            .greater_or_eql => {
                try self.sir.instructions.append(self.allocator, .{ .lt = operator_start });
                try self.sir.instructions.append(self.allocator, .{ .bool_not = operator_start });
            },

            else => unreachable,
        }
    }

    fn parseCast(self: *Parser) Error!void {
        const as_keyword_start = self.tokenRange().start;

        self.advance();

        try self.parseExpr(.cast);

        try self.sir.instructions.append(self.allocator, .{ .cast = as_keyword_start });
    }

    fn parseCall(self: *Parser) Error!void {
        const open_paren_start = self.tokenRange().start;

        self.advance();

        const arguments_count = try self.parseCallArguments();

        try self.sir.instructions.append(self.allocator, .{ .reverse = arguments_count + 1 });

        try self.sir.instructions.append(self.allocator, .{ .call = .{ .arguments_count = arguments_count, .token_start = open_paren_start } });
    }

    fn parseCallArguments(self: *Parser) Error!u32 {
        var count: u32 = 0;

        while (self.tokenTag() != .eof and self.tokenTag() != .close_paren) {
            try self.parseExpr(.lowest);

            count += 1;

            if (!self.eat(.comma) and self.tokenTag() != .close_paren) {
                self.error_info = .{ .message = "expected a ','", .source_loc = SourceLoc.find(self.file.buffer, self.tokenRange().start) };

                return error.WithMessage;
            }
        }

        if (!self.eat(.close_paren)) {
            self.error_info = .{ .message = "expected a ')'", .source_loc = SourceLoc.find(self.file.buffer, self.tokenRange().start) };

            return error.WithMessage;
        }

        return count;
    }

    fn parseSubscript(self: *Parser) Error!void {
        const open_bracket_start = self.tokenRange().start;

        self.advance();

        try self.sir.instructions.append(self.allocator, .{ .pre_get_element = open_bracket_start });

        try self.parseExpr(.lowest);

        const slicing = self.eat(.double_period);

        if (slicing) {
            try self.parseExpr(.lowest);

            try self.sir.instructions.append(self.allocator, .{ .make_slice = open_bracket_start });
        }

        if (!self.eat(.close_bracket)) {
            self.error_info = .{ .message = "expected a ']'", .source_loc = SourceLoc.find(self.file.buffer, self.tokenRange().start) };

            return error.WithMessage;
        }

        if (!slicing) {
            try self.sir.instructions.append(self.allocator, .{ .get_element = open_bracket_start });
        }
    }

    fn parseFieldAccess(self: *Parser) Error!void {
        const period_start = self.tokenRange().start;

        self.advance();

        if (self.eat(.star)) {
            try self.sir.instructions.append(self.allocator, .{ .read = period_start });
        } else {
            const name = try self.parseName();

            try self.sir.instructions.append(self.allocator, .{ .get_field = name });
        }
    }

    fn parseBody(self: *Parser) Error!void {
        const previous_scope_defer_count = self.scope_defer_count;
        self.scope_defer_count = 0;

        if (!self.eat(.open_brace)) {
            self.error_info = .{ .message = "expected a '{'", .source_loc = SourceLoc.find(self.file.buffer, self.tokenRange().start) };

            return error.WithMessage;
        }

        while (!self.eat(.close_brace)) {
            try self.parseStmt(true);

            if (self.tokenTag() == .eof) {
                self.error_info = .{ .message = "expected a '}'", .source_loc = SourceLoc.find(self.file.buffer, self.tokenRange().start) };

                return error.WithMessage;
            }
        }

        try self.popScopeDefers();

        self.scope_defer_count = previous_scope_defer_count;
    }

    fn reportRedeclaration(self: *Parser, name: Name) Error!noreturn {
        var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

        try error_message_buf.writer(self.allocator).print("redeclaration of '{s}'", .{name.buffer});

        self.error_info = .{ .message = error_message_buf.items, .source_loc = SourceLoc.find(self.file.buffer, name.token_start) };

        return error.WithMessage;
    }
};
