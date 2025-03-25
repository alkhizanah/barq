//! Compilation.
//!
//! A wrapper around the compilation pipeline of Barq.

const std = @import("std");
const root = @import("root");

const Air = @import("Air.zig");
const Range = @import("Range.zig");
const Symbol = @import("Symbol.zig");
const Scope = Symbol.Scope;
const Type = Symbol.Type;
const Sir = @import("Sir.zig");
const Sema = @import("Sema.zig");

const LlvmBackend = @import("backend/LlvmBackend.zig");

const Compilation = @This();

allocator: std.mem.Allocator,

target: std.Target,

barq_lib: BarqLib,

root_file: File,

optimization_mode: OptimizationMode,

code_model: CodeModel,

global_assembly: std.ArrayListUnmanaged(u8) = .{},

lazy_units: std.StringHashMapUnmanaged(LazyUnit) = .{},
modules: std.StringArrayHashMapUnmanaged(Module) = .{},
bytes: std.ArrayListUnmanaged(u8) = .{},
types: std.ArrayHashMapUnmanaged(Type, void, TypeHashContext, false) = .{},
ints: std.AutoArrayHashMapUnmanaged(i128, void) = .{},

pub const OptimizationMode = enum {
    debug,
    release,
};

pub const CodeModel = enum {
    default,
    tiny,
    small,
    kernel,
    medium,
    large,
};

pub const BarqLib = struct {
    dir: std.fs.Dir,
    std_file: std.fs.File,
    std_file_path: []const u8,

    pub fn openDir() !std.fs.Dir {
        var self_exe_dir_path_buf: [std.fs.max_path_bytes]u8 = undefined;

        const self_exe_dir_path = try std.fs.selfExeDirPath(&self_exe_dir_path_buf);

        const self_exe_dir = try std.fs.openDirAbsolute(self_exe_dir_path, .{});

        // We start from the executable directory, and iterate upwards
        var dir = self_exe_dir;

        var opened = false;

        while (!opened) {
            opened = true;

            // We first try to open `lib/barq` directory so we differentiate between
            // `/usr/lib` and `/usr/lib/barq` if the executable is in `/usr/bin`
            dir = dir.openDir("lib" ++ std.fs.path.sep_str ++ "barq", .{}) catch |err| switch (err) {
                error.FileNotFound => blk: {
                    // Ok so we didn't find `lib/barq` let's now try the more generic `lib`
                    break :blk dir.openDir("lib", .{}) catch |another_err| switch (another_err) {
                        error.FileNotFound => {
                            opened = false;

                            // We still didn't find any of those, so we need to go up one directory
                            break :blk try dir.openDir("..", .{});
                        },

                        else => return err,
                    };
                },

                else => return err,
            };
        }

        return dir;
    }
};

pub const Module = struct {
    file: File,
    sir: Sir,
    globals: *Sema.Globals,
};

pub const LazyUnit = struct {
    owner_id: u32,
    token_start: u32,
    type_block: ?u32 = null,
    value_block: ?u32 = null,
};

pub inline fn getModulePtrFromId(self: Compilation, id: u32) *Module {
    return &self.modules.values()[id];
}

pub inline fn putModule(self: *Compilation, module: Module) std.mem.Allocator.Error!u32 {
    return @intCast((try self.modules.getOrPutValue(self.allocator, module.file.path, module)).index);
}

const TypeHashContext = struct {
    pub fn hash(_: TypeHashContext, @"type": Type) u32 {
        var hasher = std.hash.Wyhash.init(0);
        std.hash.autoHashStrat(&hasher, @"type", .DeepRecursive);
        return @truncate(hasher.final());
    }

    pub fn eql(_: TypeHashContext, lhs: Type, rhs: Type, _: usize) bool {
        return std.meta.eql(lhs, rhs);
    }
};

pub fn putType(self: *Compilation, @"type": Type) std.mem.Allocator.Error!u32 {
    return @intCast((try self.types.getOrPutContext(self.allocator, @"type", .{})).index);
}

pub inline fn getTypeFromId(self: Compilation, id: u32) Type {
    return self.types.keys()[id];
}

pub inline fn putInt(self: *Compilation, int: i128) std.mem.Allocator.Error!u32 {
    return @intCast((try self.ints.getOrPut(self.allocator, int)).index);
}

pub inline fn getIntFromId(self: Compilation, id: u32) i128 {
    return self.ints.keys()[id];
}

pub fn makeStringType(self: *Compilation, len: usize) std.mem.Allocator.Error!Type {
    return Type{
        .pointer = .{
            .is_const = true,
            .size = .one,
            .child_type_id = try self.putType(.{
                .array = .{
                    .len_int_id = try self.putInt(len),
                    .child_type_id = try self.putType(.{ .int = .{ .signedness = .unsigned, .bits = 8 } }),
                },
            }),
        },
    };
}

pub fn getStringFromRange(self: Compilation, range: Range) []const u8 {
    return self.bytes.items[range.start..range.end];
}

pub const File = struct {
    path: []const u8,
    buffer: [:0]const u8,
};

pub fn emit(
    self: *Compilation,
    air: Air,
    output_file_path: [:0]const u8,
    output_kind: root.OutputKind,
) std.mem.Allocator.Error!void {
    var backend = try LlvmBackend.init(self.allocator, self, air);
    defer backend.deinit();
    try backend.emit(output_file_path, output_kind);
}

/// Link an object file into an executable file
pub fn link(self: Compilation, object_file_path: []const u8, output_file_path: []const u8) !u8 {
    const lld = switch (self.target.ofmt) {
        .coff => "lld-link",
        .elf => "ld.lld",
        .macho => "ld64.lld",
        .wasm => "wasm-ld",

        else => return error.UnknownObjectFormat,
    };

    const lld_argv: [4][]const u8 = .{
        lld,
        object_file_path,
        "-o",
        output_file_path,
    };

    if (!std.process.can_spawn) {
        @compileError("TODO: use lld library if spawning is not supported");
    }

    var lld_process = std.process.Child.init(&lld_argv, self.allocator);
    lld_process.stdin_behavior = .Inherit;
    lld_process.stdout_behavior = .Inherit;
    lld_process.stderr_behavior = .Inherit;

    const termination = try lld_process.spawnAndWait();

    switch (termination) {
        .Exited => |code| return code,

        else => return 1,
    }
}
