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

blocks: std.ArrayListUnmanaged(std.ArrayListUnmanaged(Instruction)) = .{},

variables: std.StringArrayHashMapUnmanaged(Variable) = .{},
constants: std.StringArrayHashMapUnmanaged(Constant) = .{},

pub const Variable = struct {
    /// The start of where this variable has been defined at, inside of the source code
    token_start: u32,
    /// A block that when evaluated by the semantic analyzer, it would get this variable's type, if this is null then semantic
    /// analyzer should try inferring the type
    type_block: ?u32 = null,
    /// A block that when evaluated by the semantic analyzer, it would get this variable's value
    value_block: u32,
};

pub const Constant = struct {
    /// The start of where this constant has been defined at, inside of the source code
    token_start: u32,
    /// A block that when evaluated by the semantic analyzer, it would get this constant's value
    value_block: u32,
};

pub const Instruction = union(enum) {
    /// Push a string onto the stack
    string: Range,
    /// Push an integer onto the stack
    int: u32,
    /// Push a float onto the stack
    float: f64,
    /// Push a boolean onto the stack
    boolean: bool,
    /// Push a function value onto the stack, function type should be on the stack
    function: Function,
    /// Push a value that is the representation of uninitialization, basically could be anything, the user doesn't care
    /// Type of this value should be on the stack
    uninitialized: u32,
    /// Push a void type onto the stack
    void_type,
    /// Push a bool type onto the stack
    bool_type,
    /// Push an int type onto the stack
    int_type: Type.Int,
    /// Push a float type onto the stack
    float_type: Type.Float,
    /// Push an array type onto the stack, array length and child type should be on the stack
    array_type: u32,
    /// Push a pointer type onto the stack, child type should be on the stack
    pointer_type: PointerType,
    /// Push a struct type onto the stack, provides the name of each field and the type of each field should be on the stack
    struct_type: StructType,
    /// Push an enum type onto the stack, provides the fields and the type of its fields should be on the stack
    enum_type: EnumType,
    /// Same as `enum_type` but the type is not on the top of the stack, and should be inferred by the semantic analyzer
    enum_type_infer: EnumType,
    /// Push a function type onto the stack, return type and parameter types should be on the stack
    function_type: FunctionType,
    /// Push the value of a function parameter onto the stack
    parameter: u32,
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
    /// Override the data that the pointer is pointing to
    store: u32,
    /// load the data that the pointer is pointing to
    load: u32,
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
    /// Allocate a local variable with the type provided, and push its pointer on stack
    alloca: u32,
    /// Same as `alloca` but the type is inferred by the value on top of the stack
    alloca_infer: u32,
    /// Duplicate a value on the stack using an index, or duplicate the top of the stack
    duplicate: ?u32,
    /// Reverse the stack into nth depth
    reverse: u32,
    /// Same as `reverse` but don't emit air instruction
    comptime_reverse: u32,
    /// Pop the stack into nth depth
    pop: u32,
    /// Get the value of a global variable
    get_global_val: Name,
    // Set the value of a global variable
    set_global_val: Name,
    /// Reference a value on the stack
    /// 1. If the value is a comptime value, it will be hoisted into a global variable and then the pointer of the global variable
    /// will be on the stack
    /// 2. If the value is a runtime value, it will be in a local variable and then the pointer of the local variable will be on the stack
    /// 3. If the previous instruction is a `load` air instruction, it will be removed and the pointer of the value will be on the stack
    reference: u32,
    /// Should be used before parsing the index of element access (i.e array[index]) or slicing (i.e array[start..end])
    pre_get_element: u32,
    /// Get an element in a "size many" pointer
    get_element: u32,
    /// Get a field in a struct
    get_field: Name,
    /// Check if a container has a specific field
    has_field: u32,
    /// Make a new slice out of a "size many" pointer
    make_slice: u32,
    /// Nest blocks inside each other
    block: u32,
    /// Loop while the value in `condition_block` is true
    loop: Loop,
    /// Skip this iteration and continue the loop, this is only emitted if we are in a loop block
    @"continue",
    /// Break this loop, this is only emitted if we are in a loop block
    @"break",
    /// If the value on top of the stack is true, go to the `then_block` else go to `else_block`, get a value if there is any
    conditional: Conditional,
    /// Switch on value to branch to a block, get a value if there is any
    @"switch": Switch,
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
        calling_convention: Type.Function.CallingConvention,
    };

    pub const Function = struct {
        name: ?Name = null,
        is_foreign: bool = false,
        foreign_name: ?Range = null,
        body_block: ?u32 = null,
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

    pub const Loop = struct {
        condition_block: u32,
        body_block: u32,
        token_start: u32,
    };

    pub const Conditional = struct {
        then_block: u32,
        else_block: ?u32,
        token_start: u32,
    };

    pub const Switch = struct {
        case_blocks: []u32,
        else_block: u32,
        case_token_starts: []u32,
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
    sir_instructions: *std.ArrayListUnmanaged(Instruction) = undefined,

    target_os_int_id: u32,
    target_arch_int_id: u32,
    target_abi_int_id: u32,

    scope: *Scope(Local) = undefined,

    alloca_count: u32 = 0,

    defer_blocks_stack: std.ArrayListUnmanaged(u32) = .{},
    scope_defers_count: u32 = 0,

    error_info: ?ErrorInfo = null,

    pub const ErrorInfo = struct {
        message: []const u8,
        source_loc: SourceLoc,
    };

    pub const Error = error{ WithMessage, WithoutMessage } || std.mem.Allocator.Error;

    pub const Local = struct {
        is_const: bool,
        stack_index: u32,
    };

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
        var scope: Scope(Local) = .{};
        self.scope = &scope;

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

        if (self.sir.constants.contains(name.buffer)) try self.reportRedeclaration(name);
        if (self.sir.variables.contains(name.buffer)) try self.reportRedeclaration(name);

        const previous_sir_instructions = self.sir_instructions;
        defer self.sir_instructions = previous_sir_instructions;

        switch (self.tokenTag()) {
            .colon => {
                self.advance();

                var type_instructions: std.ArrayListUnmanaged(Instruction) = .{};
                if (top_level) self.sir_instructions = &type_instructions;

                try self.parseExprA(.lowest);

                const type_block: u32 = @intCast(self.sir.blocks.items.len);
                if (top_level) try self.sir.blocks.append(self.allocator, type_instructions);

                if (!self.eat(.assign)) {
                    self.error_info = .{ .message = "expected a '='", .source_loc = SourceLoc.find(self.file.buffer, self.tokenRange().start) };

                    return error.WithMessage;
                }

                var value_instructions: std.ArrayListUnmanaged(Instruction) = .{};
                if (top_level) self.sir_instructions = &value_instructions;

                try self.parseExpr(.lowest);

                const value_block: u32 = @intCast(self.sir.blocks.items.len);
                if (top_level) try self.sir.blocks.append(self.allocator, value_instructions);

                if (self.tokens.items(.tag)[self.token_index - 1] != .close_brace and !self.eat(.semicolon)) {
                    self.error_info = .{ .message = "expected a ';'", .source_loc = SourceLoc.find(self.file.buffer, self.tokenRange().start) };

                    return error.WithMessage;
                }

                if (top_level) {
                    try self.sir.variables.put(self.allocator, name.buffer, .{
                        .token_start = name.token_start,
                        .type_block = type_block,
                        .value_block = value_block,
                    });
                } else {
                    try self.sir_instructions.appendSlice(self.allocator, &.{
                        .{ .reverse = 2 },
                        .{ .alloca = name.token_start },
                        .{ .duplicate = null },
                        .{ .reverse = 3 },
                        .{ .reverse = 2 },
                        .{ .store = name.token_start },
                    });

                    self.alloca_count += 1;

                    try self.scope.put(self.allocator, name.buffer, .{
                        .is_const = false,
                        .stack_index = self.alloca_count - 1,
                    });
                }
            },

            .double_colon => {
                self.advance();

                var value_instructions: std.ArrayListUnmanaged(Instruction) = .{};
                if (top_level) self.sir_instructions = &value_instructions;

                try self.parseExpr(.lowest);

                const value_block: u32 = @intCast(self.sir.blocks.items.len);
                if (top_level) try self.sir.blocks.append(self.allocator, value_instructions);

                if (self.tokens.items(.tag)[self.token_index - 1] != .close_brace and !self.eat(.semicolon)) {
                    self.error_info = .{ .message = "expected a ';'", .source_loc = SourceLoc.find(self.file.buffer, self.tokenRange().start) };

                    return error.WithMessage;
                }

                if (top_level) {
                    if (self.sir_instructions.getLast() == .function) {
                        self.sir_instructions.items[self.sir_instructions.items.len - 1].function.name = name;
                    }

                    try self.sir.constants.put(self.allocator, name.buffer, .{
                        .token_start = name.token_start,
                        .value_block = value_block,
                    });
                } else {
                    self.alloca_count += 1;

                    try self.scope.put(self.allocator, name.buffer, .{
                        .is_const = true,
                        .stack_index = self.alloca_count - 1,
                    });
                }
            },

            .colon_assign => {
                self.advance();

                var value_instructions: std.ArrayListUnmanaged(Instruction) = .{};
                if (top_level) self.sir_instructions = &value_instructions;

                try self.parseExpr(.lowest);

                const value_block: u32 = @intCast(self.sir.blocks.items.len);
                if (top_level) try self.sir.blocks.append(self.allocator, value_instructions);

                if (self.tokens.items(.tag)[self.token_index - 1] != .close_brace and !self.eat(.semicolon)) {
                    self.error_info = .{ .message = "expected a ';'", .source_loc = SourceLoc.find(self.file.buffer, self.tokenRange().start) };

                    return error.WithMessage;
                }

                if (top_level) {
                    try self.sir.variables.put(self.allocator, name.buffer, .{
                        .token_start = name.token_start,
                        .value_block = value_block,
                    });
                } else {
                    try self.sir_instructions.appendSlice(self.allocator, &.{
                        .{ .alloca_infer = name.token_start },
                        .{ .duplicate = null },
                        .{ .reverse = 3 },
                        .{ .reverse = 2 },
                        .{ .store = name.token_start },
                    });

                    self.alloca_count += 1;

                    try self.scope.put(self.allocator, name.buffer, .{
                        .is_const = false,
                        .stack_index = self.alloca_count - 1,
                    });
                }
            },

            else => {
                self.error_info = .{ .message = "expected either '::' or ':=' or ':'", .source_loc = SourceLoc.find(self.file.buffer, self.tokenRange().start) };

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
        const stmt_token_tag = self.tokenTag();

        switch (stmt_token_tag) {
            .identifier => switch (self.tokens.items(.tag)[self.token_index + 1]) {
                .colon, .double_colon, .colon_assign => return self.parseUnit(false),

                else => {
                    try self.parseExpr(.lowest);

                    try self.sir_instructions.append(self.allocator, .{ .pop = 1 });
                },
            },

            .keyword_while => try self.parseWhileLoop(),

            .keyword_break => try self.parseBreak(),

            .keyword_continue => try self.parseContinue(),

            .keyword_defer => try self.parseDefer(),

            .keyword_return => try self.parseReturn(),

            .open_brace => try self.sir_instructions.append(self.allocator, .{ .block = try self.parseStmtsBlock() }),

            else => {
                try self.parseExpr(.lowest);

                try self.sir_instructions.append(self.allocator, .{ .pop = 1 });
            },
        }

        if (expect_semicolon and
            (stmt_token_tag == .keyword_return or self.tokens.items(.tag)[self.token_index - 1] != .close_brace) and
            !self.eat(.semicolon))
        {
            self.error_info = .{ .message = "expected a ';'", .source_loc = SourceLoc.find(self.file.buffer, self.tokenRange().start) };

            return error.WithMessage;
        }
    }

    fn popScopeDefers(self: *Parser) Error!void {
        while (self.scope_defers_count > 0) : (self.scope_defers_count -= 1) {
            const defer_block = self.defer_blocks_stack.pop().?;

            try self.sir_instructions.append(self.allocator, .{ .block = defer_block });
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

        var case_blocks: std.ArrayListUnmanaged(u32) = .{};
        var case_token_starts: std.ArrayListUnmanaged(u32) = .{};

        var maybe_else_block: ?u32 = null;

        while (self.tokenTag() != .eof and self.tokenTag() != .close_brace) {
            if (self.tokenTag() == .keyword_else) {
                const else_keyword_start = self.tokenRange().start;

                self.advance();

                if (maybe_else_block != null) {
                    self.error_info = .{ .message = "duplicate switch case", .source_loc = SourceLoc.find(self.file.buffer, else_keyword_start) };

                    return error.WithMessage;
                }

                if (!self.eat(.fat_arrow)) {
                    self.error_info = .{ .message = "expected a '=>'", .source_loc = SourceLoc.find(self.file.buffer, self.tokenRange().start) };

                    return error.WithMessage;
                }

                maybe_else_block = if (self.tokenTag() == .open_brace)
                    try self.parseStmtsBlock()
                else
                    try self.parseExprBlock(.lowest);
            } else {
                try case_token_starts.append(self.allocator, self.tokenRange().start);

                try self.parseExpr(.lowest);

                var append_case_block_count: usize = 1;

                if (self.eat(.comma)) {
                    while (self.tokenTag() != .eof and self.tokenTag() != .fat_arrow) {
                        append_case_block_count += 1;

                        try case_token_starts.append(self.allocator, self.tokenRange().start);

                        try self.parseExpr(.lowest);

                        if (!self.eat(.comma) and self.tokenTag() != .fat_arrow) {
                            self.error_info = .{ .message = "expected a ','", .source_loc = SourceLoc.find(self.file.buffer, self.tokenRange().start) };

                            return error.WithMessage;
                        }
                    }
                }

                if (!self.eat(.fat_arrow)) {
                    self.error_info = .{ .message = "expected a '=>'", .source_loc = SourceLoc.find(self.file.buffer, self.tokenRange().start) };

                    return error.WithMessage;
                }

                const case_block = if (self.tokenTag() == .open_brace)
                    try self.parseStmtsBlock()
                else
                    try self.parseExprBlock(.lowest);

                try case_blocks.appendNTimes(self.allocator, case_block, append_case_block_count);
            }

            if (!self.eat(.comma) and self.tokenTag() != .close_brace) {
                self.error_info = .{ .message = "expected a ','", .source_loc = SourceLoc.find(self.file.buffer, self.tokenRange().start) };

                return error.WithMessage;
            }
        }

        if (!self.eat(.close_brace)) {
            self.error_info = .{ .message = "expected a '}'", .source_loc = SourceLoc.find(self.file.buffer, self.tokenRange().start) };

            return error.WithMessage;
        }

        const owned_case_blocks = try case_blocks.toOwnedSlice(self.allocator);
        const owned_case_token_starts = try case_token_starts.toOwnedSlice(self.allocator);

        try self.sir_instructions.append(self.allocator, .{ .reverse = @intCast(owned_case_blocks.len + 1) });

        if (maybe_else_block) |else_block| {
            try self.sir_instructions.append(self.allocator, .{
                .@"switch" = .{
                    .case_blocks = owned_case_blocks,
                    .else_block = else_block,
                    .case_token_starts = owned_case_token_starts,
                    .token_start = switch_keyword_start,
                },
            });
        } else {
            self.error_info = .{ .message = "unhandled switch cases (note: missing an 'else' case)", .source_loc = SourceLoc.find(self.file.buffer, switch_keyword_start) };

            return error.WithMessage;
        }
    }

    fn parseConditional(self: *Parser) Error!void {
        const if_keyword_start = self.tokenRange().start;

        self.advance();

        try self.parseExpr(.lowest);

        const then_block = if (self.eat(.keyword_then))
            try self.parseExprBlock(.lowest)
        else
            try self.parseStmtsBlock();

        const else_block = if (!self.eat(.keyword_else))
            null
        else if (self.tokenTag() == .open_brace)
            try self.parseStmtsBlock()
        else
            try self.parseExprBlock(.lowest);

        try self.sir_instructions.append(self.allocator, .{
            .conditional = .{
                .then_block = then_block,
                .else_block = else_block,
                .token_start = if_keyword_start,
            },
        });
    }

    var in_loop: bool = false;

    fn parseWhileLoop(self: *Parser) Error!void {
        const while_keyword_start = self.tokenRange().start;

        self.advance();

        const condition_block = try self.parseExprBlock(.lowest);

        const previous_in_loop = in_loop;
        in_loop = true;
        const body_block = try self.parseStmtsBlock();
        in_loop = previous_in_loop;

        try self.sir_instructions.append(self.allocator, .{
            .loop = .{
                .condition_block = condition_block,
                .body_block = body_block,
                .token_start = while_keyword_start,
            },
        });
    }

    fn parseContinue(self: *Parser) Error!void {
        const continue_keyword_start = self.tokenRange().start;

        self.advance();

        if (!in_loop) {
            self.error_info = .{ .message = "expected the continue statement to be inside a loop", .source_loc = SourceLoc.find(self.file.buffer, continue_keyword_start) };

            return error.WithMessage;
        }

        try self.sir_instructions.append(self.allocator, .@"continue");
    }

    fn parseBreak(self: *Parser) Error!void {
        const break_keyword_start = self.tokenRange().start;

        self.advance();

        if (!in_loop) {
            self.error_info = .{ .message = "expected the break statement to be inside a loop", .source_loc = SourceLoc.find(self.file.buffer, break_keyword_start) };

            return error.WithMessage;
        }

        try self.sir_instructions.append(self.allocator, .@"break");
    }

    fn parseDefer(self: *Parser) Error!void {
        self.advance();

        const defer_block = if (self.tokenTag() == .open_brace)
            try self.parseStmtsBlock()
        else
            try self.parseStmtBlock(false);

        try self.defer_blocks_stack.append(self.allocator, defer_block);
        self.scope_defers_count += 1;
    }

    fn parseReturn(self: *Parser) Error!void {
        const return_keyword_start = self.tokenRange().start;

        self.advance();

        const returns_value = self.tokenTag() != .semicolon;

        if (returns_value) {
            try self.parseExpr(.lowest);
        }

        try self.sir_instructions.ensureUnusedCapacity(self.allocator, self.defer_blocks_stack.items.len);

        if (self.defer_blocks_stack.items.len > 0) {
            var i = self.defer_blocks_stack.items.len - 1;

            while (true) : (i -= 1) {
                const defer_block = self.defer_blocks_stack.items[i];

                self.sir_instructions.appendAssumeCapacity(.{ .block = defer_block });

                if (i == 0) break;
            }
        }

        self.defer_blocks_stack.shrinkRetainingCapacity(self.defer_blocks_stack.items.len - self.scope_defers_count);
        self.scope_defers_count = 0;

        if (returns_value) {
            try self.sir_instructions.append(self.allocator, .{ .ret = return_keyword_start });
        } else {
            try self.sir_instructions.append(self.allocator, .{ .ret_void = return_keyword_start });
        }
    }

    const Precedence = enum {
        lowest,
        assign,
        bit_or,
        bit_xor,
        bit_and,
        comparison,
        shift,
        sum,
        product,
        cast,
        prefix,
        field,
        subscript,
        call,

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

            .keyword_switch => try self.parseSwitch(),

            .keyword_if => try self.parseConditional(),

            .keyword_struct => try self.parseStructType(),

            .keyword_enum => try self.parseEnumType(),

            .keyword_fn => try self.parseFunction(),

            .open_bracket => try self.parsePointerType(true),
            .star => try self.parsePointerType(false),

            .open_paren => try self.parseParentheses(),

            .keyword_asm => try self.parseInlineAssembly(),

            .minus, .bool_not, .bit_not, .bit_and => try self.parseUnaryOperation(),

            else => {
                self.error_info = .{ .message = "unknown expression", .source_loc = SourceLoc.find(self.file.buffer, self.tokenRange().start) };

                return error.WithMessage;
            },
        }
    }

    fn parseIdentifier(self: *Parser) Error!void {
        const name = try self.parseName();

        if (self.scope.get(name.buffer)) |local| {
            try self.sir_instructions.append(self.allocator, .{ .duplicate = local.stack_index });

            if (!local.is_const) {
                try self.sir_instructions.append(self.allocator, .{ .load = name.token_start });
            }
        } else {
            const c_char_bits = self.compilation.env.target.cTypeBitSize(.char);

            {
                const c_char_type_instruction: Instruction = .{
                    .int_type = .{
                        .signedness = if (self.compilation.env.target.charSignedness() == .signed) .signed else .unsigned,
                        .bits = c_char_bits,
                    },
                };

                inline for (.{ "void", "bool", "c_char" }, .{ .void_type, .bool_type, c_char_type_instruction }) |type_name, instruction| {
                    if (std.mem.eql(u8, name.buffer, type_name)) {
                        return self.sir_instructions.append(self.allocator, instruction);
                    }
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

                inline for (.{ "c_uchar", "c_ushort", "c_uint", "c_ulong", "c_ulonglong", "usize" }, .{ c_char_bits, c_ushort_bits, c_uint_bits, c_ulong_bits, c_ulonglong_bits, ptr_bits }) |type_name, bits| {
                    if (std.mem.eql(u8, name.buffer, type_name)) {
                        return self.sir_instructions.append(self.allocator, .{
                            .int_type = .{
                                .signedness = .unsigned,
                                .bits = @intCast(bits),
                            },
                        });
                    }
                }

                inline for (.{ "c_schar", "c_short", "c_int", "c_long", "c_longlong", "ssize" }, .{ c_char_bits, c_short_bits, c_int_bits, c_long_bits, c_longlong_bits, ptr_bits }) |type_name, bits| {
                    if (std.mem.eql(u8, name.buffer, type_name)) {
                        return self.sir_instructions.append(self.allocator, .{
                            .int_type = .{
                                .signedness = .signed,
                                .bits = @intCast(bits),
                            },
                        });
                    }
                }

                {
                    const c_float_bits = self.compilation.env.target.cTypeBitSize(.float);
                    const c_double_bits = self.compilation.env.target.cTypeBitSize(.double);
                    // TODO: Type `c_longdouble` requires `f80` and `f128` to be supported.

                    inline for (.{ "f16", "f32", "f64", "c_float", "c_double" }, .{ 16, 32, 64, c_float_bits, c_double_bits }) |type_name, bits| {
                        if (std.mem.eql(u8, name.buffer, type_name)) {
                            return self.sir_instructions.append(self.allocator, .{
                                .float_type = .{
                                    .bits = @intCast(bits),
                                },
                            });
                        }
                    }
                }

                {
                    inline for (.{ "true", "false" }, .{ true, false }) |boolean_name, boolean_value| {
                        if (std.mem.eql(u8, name.buffer, boolean_name)) {
                            return self.sir_instructions.append(self.allocator, .{
                                .boolean = boolean_value,
                            });
                        }
                    }
                }

                if (name.buffer.len > 1) {
                    if (std.mem.startsWith(u8, name.buffer, "u")) {
                        if (std.fmt.parseInt(u16, name.buffer[1..], 10) catch null) |bits| {
                            return self.sir_instructions.append(self.allocator, .{
                                .int_type = .{
                                    .signedness = .unsigned,
                                    .bits = @intCast(bits),
                                },
                            });
                        }
                    } else if (std.mem.startsWith(u8, name.buffer, "s")) {
                        if (std.fmt.parseInt(u16, name.buffer[1..], 10) catch null) |bits| {
                            return self.sir_instructions.append(self.allocator, .{
                                .int_type = .{
                                    .signedness = .signed,
                                    .bits = @intCast(bits),
                                },
                            });
                        }
                    }
                }

                try self.sir_instructions.append(self.allocator, .{ .get_global_val = name });
            }
        }
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

            try self.sir_instructions.append(self.allocator, .{ .import = token_start });
        } else if (std.mem.eql(u8, token_value, "uninitialized")) {
            if (!self.eat(.open_paren)) {
                self.error_info = .{ .message = "expected a '('", .source_loc = SourceLoc.find(self.file.buffer, self.tokenRange().start) };

                return error.WithMessage;
            }

            try self.parseExpr(.lowest);

            if (!self.eat(.close_paren)) {
                self.error_info = .{ .message = "expected a ')'", .source_loc = SourceLoc.find(self.file.buffer, self.tokenRange().start) };

                return error.WithMessage;
            }

            try self.sir_instructions.append(self.allocator, .{ .uninitialized = token_start });
        } else if (std.mem.eql(u8, token_value, "has_field")) {
            if (!self.eat(.open_paren)) {
                self.error_info = .{ .message = "expected a '('", .source_loc = SourceLoc.find(self.file.buffer, self.tokenRange().start) };

                return error.WithMessage;
            }

            try self.parseExpr(.lowest);

            if (!self.eat(.comma)) {
                self.error_info = .{ .message = "expected a ','", .source_loc = SourceLoc.find(self.file.buffer, self.tokenRange().start) };

                return error.WithMessage;
            }

            try self.parseExpr(.lowest);

            if (!self.eat(.close_paren)) {
                self.error_info = .{ .message = "expected a ')'", .source_loc = SourceLoc.find(self.file.buffer, self.tokenRange().start) };

                return error.WithMessage;
            }

            try self.sir_instructions.append(self.allocator, .{ .has_field = token_start });
        } else if (std.mem.eql(u8, token_value, "target_os")) {
            try self.sir_instructions.append(self.allocator, .{ .int = self.target_os_int_id });
        } else if (std.mem.eql(u8, token_value, "target_arch")) {
            try self.sir_instructions.append(self.allocator, .{ .int = self.target_arch_int_id });
        } else if (std.mem.eql(u8, token_value, "target_abi")) {
            try self.sir_instructions.append(self.allocator, .{ .int = self.target_abi_int_id });
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

        try self.sir_instructions.append(self.allocator, .{ .string = .{ .start = string_bytes_start, .end = string_bytes_end } });
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

        try self.sir_instructions.append(self.allocator, .{ .int = try self.compilation.putInt(decoded) });
    }

    fn parseInt(self: *Parser) Error!void {
        const value = std.fmt.parseInt(i128, self.tokenValue(), 0) catch {
            self.error_info = .{ .message = "invalid number", .source_loc = SourceLoc.find(self.file.buffer, self.tokenRange().start) };

            return error.WithMessage;
        };

        self.advance();

        try self.sir_instructions.append(self.allocator, .{ .int = try self.compilation.putInt(value) });
    }

    fn parseFloat(self: *Parser) Error!void {
        const value = std.fmt.parseFloat(f64, self.tokenValue()) catch {
            self.error_info = .{ .message = "invalid number", .source_loc = SourceLoc.find(self.file.buffer, self.tokenRange().start) };

            return error.WithMessage;
        };

        self.advance();

        try self.sir_instructions.append(self.allocator, .{ .float = value });
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

            if (fields.contains(field_name.buffer)) {
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

        try self.sir_instructions.append(self.allocator, .{ .comptime_reverse = @intCast(fields.count()) });

        try self.sir_instructions.append(self.allocator, .{
            .struct_type = .{
                .fields = fields.values(),
                .token_start = struct_keyword_start,
            },
        });
    }

    fn parseEnumType(self: *Parser) Error!void {
        const enum_keyword_start = self.tokenRange().start;

        self.advance();

        const infer_backing_type = self.eat(.open_brace);

        if (!infer_backing_type) {
            try self.parseExpr(.lowest);

            if (!self.eat(.open_brace)) {
                self.error_info = .{ .message = "expected a '{'", .source_loc = SourceLoc.find(self.file.buffer, self.tokenRange().start) };

                return error.WithMessage;
            }
        }

        var fields: std.StringArrayHashMapUnmanaged(Type.Enum.Field) = .{};

        while (!self.eat(.close_brace)) {
            const field_name = try self.parseName();

            if (fields.contains(field_name.buffer)) {
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

                field_int_id = self.sir_instructions.pop().?.int;
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

        try self.sir_instructions.append(self.allocator, if (infer_backing_type)
            .{
                .enum_type_infer = .{
                    .fields = fields.values(),
                    .token_start = enum_keyword_start,
                },
            }
        else
            .{
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

        try self.sir_instructions.append(self.allocator, .{ .comptime_reverse = @intCast(parameters.items.len) });

        if (self.tokenTag() != .special_identifier and self.tokenTag() != .open_brace) {
            try self.parseExpr(.lowest);
        } else {
            try self.sir_instructions.append(self.allocator, .void_type);
        }

        var is_foreign: bool = false;
        var maybe_foreign_name: ?Range = null;
        var maybe_calling_convention: ?Type.Function.CallingConvention = null;

        while (self.tokenTag() == .special_identifier) {
            const special_identifier = self.tokenValue();

            if (std.mem.eql(u8, special_identifier, "foreign")) {
                self.advance();

                is_foreign = true;

                if (!self.eat(.open_paren)) {
                    continue;
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

                maybe_foreign_name = self.sir_instructions.pop().?.string;
            } else if (std.mem.eql(u8, special_identifier, "callconv")) {
                self.advance();

                if (!self.eat(.open_paren)) {
                    self.error_info = .{ .message = "expected a '('", .source_loc = SourceLoc.find(self.file.buffer, self.tokenRange().start) };

                    return error.WithMessage;
                }

                if (self.tokenTag() != .identifier) {
                    self.error_info = .{ .message = "expected a valid identifier", .source_loc = SourceLoc.find(self.file.buffer, self.tokenRange().start) };

                    return error.WithMessage;
                }

                const identifier = self.tokenValue();

                maybe_calling_convention = if (std.mem.eql(u8, identifier, "auto"))
                    .auto
                else if (std.mem.eql(u8, identifier, "c"))
                    .c
                else if (std.mem.eql(u8, identifier, "inline"))
                    .@"inline"
                else if (std.mem.eql(u8, identifier, "naked"))
                    .naked
                else {
                    self.error_info = .{ .message = "unknown calling convention", .source_loc = SourceLoc.find(self.file.buffer, self.tokenRange().start) };

                    return error.WithMessage;
                };

                self.advance();

                if (!self.eat(.close_paren)) {
                    self.error_info = .{ .message = "expected a ')'", .source_loc = SourceLoc.find(self.file.buffer, self.tokenRange().start) };

                    return error.WithMessage;
                }
            } else {
                self.error_info = .{ .message = "unexpected special identifier", .source_loc = SourceLoc.find(self.file.buffer, self.tokenRange().start) };

                return error.WithMessage;
            }
        }

        const calling_convention: Type.Function.CallingConvention = maybe_calling_convention orelse if (!is_foreign) .auto else .c;

        try self.sir_instructions.append(self.allocator, .{
            .function_type = .{
                .parameters_count = parameters.items.len,
                .is_var_args = is_var_args,
                .token_start = fn_keyword_start,
                .calling_convention = calling_convention,
            },
        });

        if (self.tokenTag() == .open_brace) {
            const previous_sir_instructions = self.sir_instructions;

            var body_instructions: std.ArrayListUnmanaged(Instruction) = .{};
            self.sir_instructions = &body_instructions;

            const previous_scope = self.scope;

            var scope: Scope(Local) = .{};
            self.scope = &scope;

            const previous_alloca_count = self.alloca_count;
            self.alloca_count = 0;

            try body_instructions.ensureUnusedCapacity(self.allocator, parameters.items.len);
            try self.scope.ensureUnusedCapacity(self.allocator, @intCast(parameters.items.len));

            for (parameters.items, 0..) |name, i| {
                body_instructions.appendAssumeCapacity(.{ .parameter = @intCast(i) });

                self.scope.putAssumeCapacity(name.buffer, .{
                    .is_const = true,
                    .stack_index = @intCast(i),
                });
            }

            self.alloca_count += @intCast(parameters.items.len);

            const returned = try self.parseStmtsBlockInstructions();

            self.alloca_count = previous_alloca_count;

            if (!returned)
                try body_instructions.append(self.allocator, .{ .ret_void = fn_keyword_start });

            self.scope = previous_scope;

            try self.sir.blocks.append(self.allocator, body_instructions);

            self.sir_instructions = previous_sir_instructions;

            try self.sir_instructions.append(self.allocator, .{
                .function = .{
                    .is_foreign = is_foreign,
                    .foreign_name = maybe_foreign_name,
                    .body_block = @intCast(self.sir.blocks.items.len - 1),
                    .token_start = fn_keyword_start,
                },
            });
        } else if (is_foreign) {
            try self.sir_instructions.append(self.allocator, .{
                .function = .{
                    .is_foreign = is_foreign,
                    .foreign_name = maybe_foreign_name,
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

            try self.sir_instructions.append(self.allocator, .{ .array_type = token_start });
        } else {
            const is_const = self.eat(.keyword_const);

            try self.parseExpr(.lowest);

            try self.sir_instructions.append(self.allocator, .{
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

        while (self.tokenTag() == .string_literal) {
            try self.parseString();

            const string = self.sir_instructions.pop().?.string;

            try content.appendSlice(self.allocator, self.compilation.getStringFromRange(string));

            self.compilation.pool.bytes.shrinkRetainingCapacity(string.start);

            try content.append(self.allocator, '\n');
        }

        var input_constraints: []Range = &.{};
        var output_constraint: ?Range = null;
        var clobbers: []Range = &.{};

        if (content.items.len > 0) {
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
        }

        if (!self.eat(.close_brace)) {
            self.error_info = .{ .message = "expected a '}'", .source_loc = SourceLoc.find(self.file.buffer, self.tokenRange().start) };

            return error.WithMessage;
        }

        content.shrinkAndFree(self.allocator, content.items.len);

        try self.sir_instructions.append(self.allocator, .{
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

        try self.sir_instructions.append(self.allocator, .{ .reverse = @intCast(constraints.items.len) });

        return constraints.toOwnedSlice();
    }

    fn parseInlineAssemblyConstraint(self: *Parser) Error!Range {
        if (self.tokenTag() != .string_literal) {
            self.error_info = .{ .message = "expected a valid string", .source_loc = SourceLoc.find(self.file.buffer, self.tokenRange().start) };

            return error.WithMessage;
        }

        try self.parseString();

        const register = self.sir_instructions.pop().?.string;

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

            const clobber = self.sir_instructions.pop().?.string;

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
                try self.sir_instructions.append(self.allocator, .{ .negate = operator_start });
            },

            .bool_not => {
                try self.sir_instructions.append(self.allocator, .{ .bool_not = operator_start });
            },

            .bit_not => {
                try self.sir_instructions.append(self.allocator, .{ .bit_not = operator_start });
            },

            .bit_and => {
                try self.sir_instructions.append(self.allocator, .{ .reference = operator_start });
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

                const last_instruction = self.sir_instructions.pop().?;

                if (last_instruction == .get_element or last_instruction == .get_field) {
                    try self.sir_instructions.appendSlice(self.allocator, &.{
                        last_instruction,
                        .{ .reference = operator_start },
                    });

                    try self.parseExpr(precedence);

                    if (maybe_additional_operator) |additional_operator| {
                        try self.sir_instructions.appendSlice(
                            self.allocator,
                            &.{
                                .{ .reverse = 2 },
                                .{ .duplicate = null },
                                .{ .load = operator_start },
                                .{ .reverse = 2 },
                                .{ .reverse = 3 },
                            },
                        );

                        try self.emitBinaryOperation(additional_operator, operator_start);
                    }

                    try self.sir_instructions.appendSlice(
                        self.allocator,
                        &.{
                            .{ .duplicate = null },
                            .{ .reverse = 3 },
                            .{ .store = operator_start },
                        },
                    );
                } else if (last_instruction == .load) {
                    try self.parseExpr(precedence);

                    if (maybe_additional_operator) |additional_operator_token| {
                        try self.sir_instructions.appendSlice(
                            self.allocator,
                            &.{
                                .{ .reverse = 2 },
                                .{ .duplicate = null },
                                .{ .load = operator_start },
                                .{ .reverse = 2 },
                                .{ .reverse = 3 },
                            },
                        );

                        try self.emitBinaryOperation(additional_operator_token, operator_start);
                    }

                    try self.sir_instructions.appendSlice(
                        self.allocator,
                        &.{
                            .{ .duplicate = null },
                            .{ .reverse = 3 },
                            .{ .store = operator_start },
                        },
                    );
                } else if (last_instruction == .get_global_val) {
                    try self.parseExpr(precedence);

                    if (maybe_additional_operator) |additional_operator_token| {
                        try self.sir_instructions.appendSlice(
                            self.allocator,
                            &.{
                                last_instruction,
                                .{ .reverse = 2 },
                            },
                        );

                        try self.emitBinaryOperation(additional_operator_token, operator_start);
                    }

                    try self.sir_instructions.appendSlice(
                        self.allocator,
                        &.{
                            .{ .duplicate = null },
                            .{ .set_global_val = last_instruction.get_global_val },
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
            .plus => try self.sir_instructions.append(self.allocator, .{ .add = operator_start }),
            .minus => try self.sir_instructions.append(self.allocator, .{ .sub = operator_start }),
            .star => try self.sir_instructions.append(self.allocator, .{ .mul = operator_start }),
            .divide => try self.sir_instructions.append(self.allocator, .{ .div = operator_start }),
            .modulo => try self.sir_instructions.append(self.allocator, .{ .rem = operator_start }),
            .less_than => try self.sir_instructions.append(self.allocator, .{ .lt = operator_start }),
            .greater_than => try self.sir_instructions.append(self.allocator, .{ .gt = operator_start }),
            .eql => try self.sir_instructions.append(self.allocator, .{ .eql = operator_start }),
            .left_shift => try self.sir_instructions.append(self.allocator, .{ .shl = operator_start }),
            .right_shift => try self.sir_instructions.append(self.allocator, .{ .shr = operator_start }),
            .bit_and => try self.sir_instructions.append(self.allocator, .{ .bit_and = operator_start }),
            .bit_or => try self.sir_instructions.append(self.allocator, .{ .bit_or = operator_start }),
            .bit_xor => try self.sir_instructions.append(self.allocator, .{ .bit_xor = operator_start }),

            .not_eql => {
                try self.sir_instructions.append(self.allocator, .{ .eql = operator_start });
                try self.sir_instructions.append(self.allocator, .{ .bool_not = operator_start });
            },

            .less_or_eql => {
                try self.sir_instructions.append(self.allocator, .{ .gt = operator_start });
                try self.sir_instructions.append(self.allocator, .{ .bool_not = operator_start });
            },

            .greater_or_eql => {
                try self.sir_instructions.append(self.allocator, .{ .lt = operator_start });
                try self.sir_instructions.append(self.allocator, .{ .bool_not = operator_start });
            },

            else => unreachable,
        }
    }

    fn parseCast(self: *Parser) Error!void {
        const as_keyword_start = self.tokenRange().start;

        self.advance();

        try self.parseExpr(.cast);

        try self.sir_instructions.append(self.allocator, .{ .cast = as_keyword_start });
    }

    fn parseCall(self: *Parser) Error!void {
        const open_paren_start = self.tokenRange().start;

        self.advance();

        const arguments_count = try self.parseCallArguments();

        try self.sir_instructions.append(self.allocator, .{ .reverse = arguments_count + 1 });

        try self.sir_instructions.append(self.allocator, .{ .call = .{ .arguments_count = arguments_count, .token_start = open_paren_start } });
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

        try self.sir_instructions.append(self.allocator, .{ .pre_get_element = open_bracket_start });

        try self.parseExpr(.lowest);

        const slicing = self.eat(.double_period);

        if (slicing) {
            try self.parseExpr(.lowest);

            try self.sir_instructions.append(self.allocator, .{ .make_slice = open_bracket_start });
        }

        if (!self.eat(.close_bracket)) {
            self.error_info = .{ .message = "expected a ']'", .source_loc = SourceLoc.find(self.file.buffer, self.tokenRange().start) };

            return error.WithMessage;
        }

        if (!slicing) {
            try self.sir_instructions.append(self.allocator, .{ .get_element = open_bracket_start });
        }
    }

    fn parseFieldAccess(self: *Parser) Error!void {
        const period_start = self.tokenRange().start;

        self.advance();

        if (self.eat(.star)) {
            try self.sir_instructions.append(self.allocator, .{ .load = period_start });
        } else {
            const name = try self.parseName();

            try self.sir_instructions.append(self.allocator, .{ .get_field = name });
        }
    }

    fn parseExprBlock(self: *Parser, precdence: Precedence) Error!u32 {
        const previous_sir_instructions = self.sir_instructions;
        defer self.sir_instructions = previous_sir_instructions;

        var block_instructions: std.ArrayListUnmanaged(Instruction) = .{};
        self.sir_instructions = &block_instructions;

        try self.parseExpr(precdence);

        try self.sir.blocks.append(self.allocator, block_instructions);

        return @intCast(self.sir.blocks.items.len - 1);
    }

    fn parseStmtBlock(self: *Parser, comptime expect_semicolon: bool) Error!u32 {
        const previous_sir_instructions = self.sir_instructions;
        defer self.sir_instructions = previous_sir_instructions;

        var block_instructions: std.ArrayListUnmanaged(Instruction) = .{};
        self.sir_instructions = &block_instructions;

        try self.parseStmt(expect_semicolon);

        try self.sir.blocks.append(self.allocator, block_instructions);

        return @intCast(self.sir.blocks.items.len - 1);
    }

    fn parseStmtsBlock(self: *Parser) Error!u32 {
        const previous_sir_instructions = self.sir_instructions;
        defer self.sir_instructions = previous_sir_instructions;

        var block_instructions: std.ArrayListUnmanaged(Instruction) = .{};
        self.sir_instructions = &block_instructions;

        _ = try self.parseStmtsBlockInstructions();

        try self.sir.blocks.append(self.allocator, block_instructions);

        return @intCast(self.sir.blocks.items.len - 1);
    }

    fn parseStmtsBlockInstructions(self: *Parser) Error!bool {
        var scope: Scope(Local) = .{ .maybe_parent = self.scope };
        self.scope = &scope;

        const previous_scope_defers_count = self.scope_defers_count;
        self.scope_defers_count = 0;

        const previous_alloca_count = self.alloca_count;

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

        const returned = self.sir_instructions.items.len > 0 and
            (self.sir_instructions.getLast() == .ret or self.sir_instructions.getLast() == .ret_void);

        try self.popScopeDefers();

        self.scope_defers_count = previous_scope_defers_count;

        try self.sir_instructions.append(self.allocator, .{ .pop = self.alloca_count - previous_alloca_count });

        self.alloca_count = previous_alloca_count;

        self.scope = self.scope.maybe_parent.?;

        return returned;
    }

    fn reportRedeclaration(self: *Parser, name: Name) Error!noreturn {
        var error_message_buf: std.ArrayListUnmanaged(u8) = .{};

        try error_message_buf.writer(self.allocator).print("redeclaration of '{s}'", .{name.buffer});

        self.error_info = .{ .message = error_message_buf.items, .source_loc = SourceLoc.find(self.file.buffer, name.token_start) };

        return error.WithMessage;
    }
};
