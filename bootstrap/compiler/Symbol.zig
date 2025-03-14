const std = @import("std");

const Name = @import("Sir.zig").Name;
const Compilation = @import("Compilation.zig");
const Range = @import("Range.zig");

const Symbol = @This();

name: Name,
type: Type,

pub const Type = union(enum) {
    void,
    bool,
    type,
    int: Int,
    float: Float,
    pointer: Pointer,
    function: Function,
    @"struct": Struct,
    @"enum": Enum,
    array: Array,

    pub const Int = struct {
        signedness: Signedness,
        bits: u16,

        pub const Signedness = enum {
            unsigned,
            signed,
        };
    };

    pub const Float = struct {
        bits: u16,
    };

    pub const Pointer = struct {
        size: Size,
        is_const: bool,
        child_type_id: u32,

        pub const Size = enum {
            one,
            many,
            slice,
        };
    };

    pub const Function = struct {
        parameter_type_ids: []u32,
        is_var_args: bool,
        return_type_id: u32,
    };

    pub const Struct = struct {
        fields: []Field,

        pub const Field = struct {
            name: []const u8,
            type_id: u32,
        };
    };

    pub const Enum = struct {
        backing_type_id: u32,
        fields: []Field,

        pub const Field = struct {
            name: []const u8,
            int_id: u32,
        };
    };

    pub const Array = struct {
        len_int_id: u32,
        child_type_id: u32,
    };

    pub const HashContext = struct {
        pub fn hash(_: HashContext, key: Type) u32 {
            var hasher = std.hash.Wyhash.init(0);
            std.hash.autoHashStrat(&hasher, key, .DeepRecursive);
            return @truncate(hasher.final());
        }

        pub fn eql(_: HashContext, key: Type, other_key: Type, _: usize) bool {
            return std.meta.eql(key, other_key);
        }
    };

    pub fn minInt(self: Type) i128 {
        return switch (self) {
            .int => |int| {
                if (int.signedness == .unsigned or int.bits == 0) return 0;
                return -(@as(i128, 1) << @intCast(int.bits - 1));
            },

            else => unreachable,
        };
    }

    pub fn maxInt(self: Type) i128 {
        return switch (self) {
            .int => |int| {
                if (int.bits == 0) return 0;
                return (@as(i128, 1) << @intCast(int.bits - @intFromBool(int.signedness == .signed))) - 1;
            },

            else => unreachable,
        };
    }

    pub fn maxFloat(self: Type) f64 {
        return switch (self) {
            .float => |float| if (float.bits == 16)
                std.math.floatMax(f16)
            else if (float.bits == 32)
                std.math.floatMax(f32)
            else if (float.bits == 64)
                std.math.floatMax(f64)
            else
                unreachable,

            else => unreachable,
        };
    }

    pub fn intFittingRange(from: i128, to: i128) Type {
        const signedness: Int.Signedness = @enumFromInt(@intFromBool(from < 0));

        const largest_positive_value = @max(if (from < 0) (-from) - 1 else from, to);

        const base: u7 = @intFromFloat(@ceil(@log2(@as(f64, @floatFromInt(largest_positive_value + 1)))));
        const upper = (@as(i128, 1) << base) - 1;

        var magnitude_bits = if (upper >= largest_positive_value) base else base + 1;
        magnitude_bits += @intFromEnum(signedness);

        return Type{
            .int = .{
                .signedness = signedness,
                .bits = @intCast(magnitude_bits),
            },
        };
    }

    pub fn floatFittingRange(from: f64, to: f64) Type {
        const largest_positive_value = @max(if (from < 0) (-from) - 1 else from, to);

        const base = @ceil(@log2(largest_positive_value + 1));

        return if (base <= 16)
            Type{ .float = .{ .bits = 16 } }
        else if (base <= 32)
            Type{ .float = .{ .bits = 32 } }
        else
            Type{ .float = .{ .bits = 64 } };
    }

    pub fn canBeNegative(self: Type) bool {
        return switch (self) {
            .float => true,
            .int => |int| int.signedness == .signed,

            else => false,
        };
    }

    pub fn hasField(self: Type, compilation: Compilation, field_name: []const u8) bool {
        return switch (self) {
            .array => std.mem.eql(u8, field_name, "len"),

            .pointer => |container_pointer_type| switch (container_pointer_type.size) {
                .slice => std.mem.eql(u8, field_name, "len") or std.mem.eql(u8, field_name, "ptr"),
                .many => false,
                .one => compilation.getTypeFromId(container_pointer_type.child_type_id).hasField(compilation, field_name),
            },

            .@"enum" => |container_enum_type| blk: {
                for (container_enum_type.fields) |enum_field| {
                    if (std.mem.eql(u8, enum_field.name, field_name)) {
                        break :blk true;
                    }
                }

                break :blk false;
            },

            .@"struct" => |container_struct_type| blk: {
                for (container_struct_type.fields) |struct_field| {
                    if (std.mem.eql(u8, struct_field.name, field_name)) {
                        break :blk true;
                    }
                }

                break :blk false;
            },

            else => false,
        };
    }

    pub fn getPointer(self: Type) ?Type.Pointer {
        if (self != .pointer) return null;
        return self.pointer;
    }

    pub fn getArray(self: Type) ?Type.Array {
        if (self != .array) return null;
        return self.array;
    }

    pub fn getStruct(self: Type) ?Type.Struct {
        if (self != .@"struct") return null;
        return self.@"struct";
    }

    pub fn getFunction(self: Type, compilation: Compilation) ?Type.Function {
        if (self.getPointer()) |pointer| {
            const child_type = compilation.getTypeFromId(pointer.child_type_id);

            if (child_type == .function) {
                return child_type.function;
            } else {
                return null;
            }
        }

        if (self != .function) {
            return null;
        }

        return self.function;
    }

    pub fn format(self: Type, compilation: Compilation, writer: anytype) !void {
        switch (self) {
            .void => try writer.writeAll("void"),
            .bool => try writer.writeAll("bool"),
            .type => try writer.writeAll("type"),

            .int => |int| try writer.print("{c}{}", .{ if (int.signedness == .unsigned) @as(u8, 'u') else @as(u8, 's'), int.bits }),
            .float => |float| try writer.print("f{}", .{float.bits}),

            .array => |array| {
                try writer.print("[{}]", .{compilation.getIntFromId(array.len_int_id)});

                const child_type = compilation.getTypeFromId(array.child_type_id);

                try child_type.format(compilation, writer);
            },

            .pointer => |pointer| {
                if (pointer.size == .one) {
                    try writer.writeAll("*");
                } else if (pointer.size == .many) {
                    try writer.writeAll("[*]");
                } else if (pointer.size == .slice) {
                    try writer.writeAll("[]");
                }

                if (pointer.is_const) {
                    try writer.writeAll("const ");
                }

                const child_type = compilation.getTypeFromId(pointer.child_type_id);

                try child_type.format(compilation, writer);
            },

            .function => |function| {
                try writer.writeAll("fn (");

                for (function.parameter_type_ids, 0..) |parameter_type_id, i| {
                    const parameter_type = compilation.getTypeFromId(parameter_type_id);

                    try parameter_type.format(compilation, writer);

                    if (i < function.parameter_type_ids.len - 1) {
                        try writer.writeAll(", ");
                    }
                }

                try writer.writeAll(") ");

                const return_type = compilation.getTypeFromId(function.return_type_id);

                try return_type.format(compilation, writer);
            },

            .@"struct" => |@"struct"| {
                try writer.writeAll("struct { ");

                for (@"struct".fields, 0..) |field, i| {
                    try writer.print("{s} ", .{field.name});

                    const field_type = compilation.getTypeFromId(field.type_id);

                    try field_type.format(compilation, writer);

                    if (i < @"struct".fields.len - 1) {
                        try writer.writeAll(", ");
                    }
                }

                try writer.writeAll(" }");
            },

            .@"enum" => |@"enum"| {
                try writer.writeAll("enum ");

                const backing_type = compilation.getTypeFromId(@"enum".backing_type_id);

                try backing_type.format(compilation, writer);

                try writer.writeAll(" { ");

                for (@"enum".fields, 0..) |field, i| {
                    try writer.print("{s}", .{field.name});

                    if (i < @"enum".fields.len - 1) {
                        try writer.writeAll(", ");
                    }
                }

                try writer.writeAll(" }");
            },
        }
    }

    pub fn eql(self: Type, compilation: Compilation, other: Type) bool {
        if (self.getPointer()) |pointer| {
            const other_pointer = other.getPointer() orelse return false;

            const my_child_type = compilation.getTypeFromId(pointer.child_type_id);
            const other_child_type = compilation.getTypeFromId(other_pointer.child_type_id);

            return ((!pointer.is_const and other_pointer.is_const) or pointer.is_const == other_pointer.is_const) and
                pointer.size == other_pointer.size and
                my_child_type.eql(compilation, other_child_type);
        } else if (self.getFunction(compilation)) |function| {
            const other_function = other.getFunction(compilation) orelse return false;

            if (function.parameter_type_ids.len != other_function.parameter_type_ids.len) return false;

            for (function.parameter_type_ids, other_function.parameter_type_ids) |my_parameter_type_id, other_parameter_type_id| {
                const my_parameter_type = compilation.getTypeFromId(my_parameter_type_id);
                const other_parameter_type = compilation.getTypeFromId(other_parameter_type_id);

                if (!my_parameter_type.eql(compilation, other_parameter_type)) return false;
            }

            const my_return_type = compilation.getTypeFromId(function.return_type_id);
            const other_return_type = compilation.getTypeFromId(other_function.return_type_id);

            return my_return_type.eql(compilation, other_return_type);
        } else if (self.getArray()) |array| {
            const other_array = other.getArray() orelse return false;

            const my_child_type = compilation.getTypeFromId(array.child_type_id);
            const other_child_type = compilation.getTypeFromId(other_array.child_type_id);

            return compilation.getIntFromId(array.len_int_id) == compilation.getIntFromId(other_array.len_int_id) and
                my_child_type.eql(compilation, other_child_type);
        } else if (self.getStruct()) |@"struct"| {
            const other_struct = other.getStruct() orelse return false;

            if (@"struct".fields.len != other_struct.fields.len) return false;

            for (@"struct".fields, other_struct.fields) |field, other_field| {
                const my_field_type = compilation.getTypeFromId(field.type_id);
                const other_field_type = compilation.getTypeFromId(other_field.type_id);

                if (!my_field_type.eql(compilation, other_field_type)) return false;
            }

            return true;
        } else {
            return std.meta.eql(self, other);
        }
    }
};

pub fn Scope(comptime V: type) type {
    return struct {
        const Self = @This();

        maybe_parent: ?*Self = null,

        items: std.StringHashMapUnmanaged(V) = .{},

        pub fn clearRetainingCapacity(self: *Self) void {
            self.items.clearRetainingCapacity();
        }

        pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
            self.items.deinit(allocator);
        }

        pub fn iterator(self: *const Self) @FieldType(Self, "items").Iterator {
            return self.items.iterator();
        }

        pub fn contains(self: Self, name: []const u8) bool {
            return self.items.contains(name);
        }

        pub fn put(self: *Self, allocator: std.mem.Allocator, name: []const u8, value: V) std.mem.Allocator.Error!void {
            try self.items.put(allocator, name, value);
        }

        pub fn putAssumeCapacity(self: *Self, name: []const u8, value: V) void {
            self.items.putAssumeCapacity(name, value);
        }

        pub fn get(self: Self, name: []const u8) ?V {
            if (self.items.get(name)) |value| {
                return value;
            }

            if (self.maybe_parent) |parent| {
                return parent.get(name);
            }

            return null;
        }

        pub fn getPtr(self: *Self, name: []const u8) ?*V {
            if (self.items.getPtr(name)) |value| {
                return value;
            }

            if (self.maybe_parent) |parent| {
                return parent.getPtr(name);
            }

            return null;
        }

        pub fn remove(self: *Self, name: []const u8) bool {
            return self.items.remove(name);
        }

        pub fn ensureTotalCapacity(self: *Self, allocator: std.mem.Allocator, new_capacity: u32) std.mem.Allocator.Error!void {
            try self.items.ensureTotalCapacity(allocator, new_capacity);
        }

        pub fn ensureUnusedCapacity(self: *Self, allocator: std.mem.Allocator, additional_capacity: u32) std.mem.Allocator.Error!void {
            try self.items.ensureUnusedCapacity(allocator, additional_capacity);
        }
    };
}
