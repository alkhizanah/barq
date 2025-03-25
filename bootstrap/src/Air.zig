//! Analyzed Intermediate Representation.
//!
//! An analyzed and checked stack-based intermediate representation lowered from `Sir`.
//! Last intermediate representation to be used before lowering to machine code.

const std = @import("std");

const Range = @import("Range.zig");
const Symbol = @import("Symbol.zig");
const Type = Symbol.Type;

const Air = @This();

blocks: std.ArrayListUnmanaged(std.ArrayListUnmanaged(Instruction)) = .{},

variables: std.StringArrayHashMapUnmanaged(Variable) = .{},
functions: std.StringArrayHashMapUnmanaged(Function) = .{},

pub const Variable = struct {
    type_id: u32,
    initializer: Instruction,
};

pub const Function = struct {
    type_id: u32,
    body_block: ?u32,
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
    /// Push a value that is the representation of uninitialization, basically could be anything, the user doesn't care
    uninitialized: u32,
    /// Negate an integer or float on the top of the stack
    negate,
    /// Reverse a boolean from true to false and from false to true
    bool_not,
    /// Perform bitwise NOT operation on the bits of rhs (Which is to reverse its bits representation)
    bit_not,
    /// Perform bitwise AND operation on the bits of lhs and rhs
    bit_and,
    /// Perform bitwise OR operation on the bits of lhs and rhs
    bit_or,
    /// Perform bitwise XOR operation on the bits of lhs and rhs
    bit_xor,
    /// Override the data that the pointer is pointing to
    store,
    /// Read the data that the pointer is pointing to
    load,
    /// Add two integers or floats on the top of the stack
    add,
    /// Subtract two integers or floats on the top of the stack
    sub,
    /// Multiply two integers or floats on the top of the stack
    mul,
    /// Divide two integers or floats on the top of the stack
    div,
    /// Remainder of two integers or floats on the top of the stack
    rem,
    /// Compare between two integers or floats on the stack and check for order (in this case, lhs less than rhs)
    lt,
    /// Compare between two integers or floats on the stack and check for order (in this case, lhs greater than rhs)
    gt,
    /// Compare between two values on the stack and check for equality
    eql,
    /// Shift to left the bits of lhs using rhs offset
    shl,
    /// Shift to right the bits of lhs using rhs offset
    shr,
    /// Cast a value to a different type
    cast: u32,
    /// Place a machine-specific inline assembly in the output
    inline_assembly: InlineAssembly,
    /// Call a function pointer on top of the stack
    call: usize,
    /// Push the value of a function parameter onto the stack
    parameter: u32,
    /// Allocate a local variable with the type provided, and push its pointer on stack
    alloca: u32,
    /// Duplicate a value on the stack using an index, or duplicate the top of the stack
    duplicate: ?u32,
    /// Reverse the stack into nth depth
    reverse: u32,
    /// Pop the stack into nth depth
    pop: u32,
    /// Get a pointer to global variable
    get_global_ptr: []const u8,
    /// Calculate the pointer of an element in a "size many" pointer
    get_element_ptr,
    /// Extract a field from a struct value
    extract_field: u32,
    /// Calculate the pointer of a field in a struct pointer
    get_field_ptr: u32,
    /// Make a slice out of a "size many" pointer
    slice,
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
    ret,
    /// Return out of the function without a value
    ret_void,

    pub const InlineAssembly = struct {
        content: []const u8,
        output_constraint: ?OutputConstraint,
        input_constraints: []Range,
        clobbers: []Range,

        pub const OutputConstraint = struct {
            register: Range,
            type_id: u32,
        };
    };

    pub const Loop = struct {
        condition_block: u32,
        body_block: u32,
    };

    pub const Conditional = struct {
        then_block: u32,
        else_block: ?u32,
    };

    pub const Switch = struct {
        case_blocks: []u32,
        else_block: u32,
    };
};
