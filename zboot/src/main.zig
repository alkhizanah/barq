const std = @import("std");
const builtin = @import("builtin");

const Compilation = @import("Compilation.zig");
const Air = @import("Air.zig");
const Sema = @import("Sema.zig");
const Sir = @import("Sir.zig");

pub const OutputKind = enum {
    assembly,
    object,
    executable,
    none,
};

pub const RunnerKind = enum {
    executable,
    none,
};

const CodeModel = Compilation.CodeModel;
const OptimizationMode = Compilation.OptimizationMode;

pub const Cli = struct {
    allocator: std.mem.Allocator,

    program: []const u8,
    command: ?Command = null,

    const Command = union(enum) {
        compile: Compile,
        run: Run,
        help,

        const Compile = struct {
            root_file_path: []const u8,
            maybe_output_file_path: ?[]const u8,
            output_kind: OutputKind,
            runner_kind: RunnerKind,
            target: std.Target,
            code_model: CodeModel,
            optimization_mode: OptimizationMode,

            const usage =
                \\Usage:
                \\  {s} compile <root-file-path> [options]
                \\
                \\Options:
                \\  --output <output-file-path>         -- specify the output file path
                \\
                \\  --emit <output-kind>                -- specify the output kind
                \\                                          [assembly, object, executable (default), none]
                \\
                \\  --runner <runner-kind>              -- specify the runner kind
                \\                                          [executable (default), none]
                \\
                \\  --target <arch-os-abi>              -- specify the target query
                \\
                \\  --cpu-model <cpu-model>             -- specify the target cpu model
                \\
                \\  --add-cpu-features <features..>     -- add to the target cpu features (separated by commas)
                \\  --remove-cpu-features <features..>  -- remove from the target cpu features (separated by commas)
                \\
                \\  --code-model <code-model>           -- specify the code model
                \\                                          [default, tiny, small, kernel, medium, large]
                \\
                \\  --optimize <optimization-mode>      -- specify the optimization mode
                \\                                          [debug, release]
                \\
            ;
        };

        const Run = struct {
            root_file_path: []const u8,
            runner_kind: RunnerKind,
            target: std.Target,
            code_model: CodeModel,
            optimization_mode: OptimizationMode,
            arguments: []const []const u8,

            const usage =
                \\Usage:
                \\  {s} run <root-file-path> [options] [-- [arguments]]
                \\
                \\Options:
                \\  --runner <runner-kind>              -- specify the runner kind
                \\                                          [executable (default), none]
                \\
                \\  --target <arch-os-abi>              -- specify the target query
                \\
                \\  --cpu-model <cpu-model>             -- specify the target cpu model
                \\
                \\  --add-cpu-features <features..>     -- add to the target cpu features (separated by commas)
                \\  --remove-cpu-features <features..>  -- remove from the target cpu features (separated by commas)
                \\
                \\  --code-model <code-model>           -- specify the code model
                \\                                          [default, tiny, small, kernel, medium, large]
                \\
                \\  --optimize <optimization-mode>      -- specify the optimization mode
                \\                                          [debug, release]
                \\
                \\
            ;
        };
    };

    const usage =
        \\Usage:
        \\  {s} <command> [options]
        \\
        \\Commands:
        \\  compile                       -- compile certain file
        \\  run                           -- compile certain file into an executable and run it
        \\  help                          -- print this help message
        \\
        \\
    ;

    pub fn errorDescription(e: anyerror) []const u8 {
        return switch (e) {
            error.OutOfMemory => "ran out of memory",
            error.FileNotFound => "no such file or directory",
            error.IsDir => "is a directory",
            error.NotDir => "is not a directory",
            error.NotOpenForReading => "is not open for reading",
            error.NotOpenForWriting => "is not open for writing",
            error.InvalidUtf8 => "invalid UTF-8",
            error.FileBusy => "file is busy",
            error.NameTooLong => "name is too long",
            error.AccessDenied => "access denied",
            error.FileTooBig, error.StreamTooLong => "file is too big",
            error.ProcessFdQuotaExceeded, error.SystemFdQuotaExceeded => "ran out of file descriptors",
            error.ThreadQuotaExceeded => "ran out of threads",
            error.LockedMemoryLimitExceeded => "ran out of locked memory",
            error.SystemResources => "ran out of system resources",
            error.FatalError => "a fatal error occurred",
            error.Unexpected => "an unexpected error occurred",
            error.UnexpectedExtraField => "unexpected extra field",
            error.UnknownArchitecture => "unrecognized architecture",
            error.UnknownOperatingSystem, error.MissingOperatingSystem => "unrecognized operating system",
            error.UnknownObjectFormat => "unrecognized object format",
            error.UnknownCpuFeature => "unrecognized cpu feature",
            error.UnknownCpuModel => "unrecognized cpu model",
            error.UnknownApplicationBinaryInterface => "unrecognized application binary interface",
            error.InvalidAbiVersion => "invalid application binary interface version",
            error.InvalidOperatingSystemVersion => "invalid operating system version",

            else => @errorName(e),
        };
    }

    fn nextArgument(
        self: Cli,
        comptime command_usage: []const u8,
        comptime option_name: []const u8,
        argument_iterator: *std.process.ArgIterator,
    ) ?[:0]const u8 {
        return argument_iterator.next() orelse {
            std.debug.print(command_usage, .{self.program});

            std.debug.print("Error: expected {s}\n", .{option_name});

            return null;
        };
    }

    fn parseEnumOption(
        self: Cli,
        comptime T: type,
        comptime command_usage: []const u8,
        comptime option_name: []const u8,
        argument_iterator: *std.process.ArgIterator,
    ) ?T {
        const argument = self.nextArgument(
            command_usage,
            option_name,
            argument_iterator,
        ) orelse return null;

        const info = @typeInfo(T);

        inline for (info.@"enum".fields) |enum_field| {
            if (std.mem.eql(u8, enum_field.name, argument)) {
                return @enumFromInt(enum_field.value);
            }
        }

        std.debug.print(Command.Compile.usage, .{self.program});

        std.debug.print("Error: unrecognized {s}: {s}\n", .{ option_name, argument });

        return null;
    }

    fn parseTargetQueryOption(self: Cli, comptime command_usage: []const u8, argument_iterator: *std.process.ArgIterator) ?std.Target {
        const argument = self.nextArgument(
            command_usage,
            "target query",
            argument_iterator,
        ) orelse return null;

        const target_query = std.Target.Query.parse(.{ .arch_os_abi = argument }) catch |err| {
            std.debug.print("Error: could not parse target query: {s}\n", .{errorDescription(err)});

            return null;
        };

        return std.zig.system.resolveTargetQuery(target_query) catch |err| {
            std.debug.print("Error: could not resolve target query: {s}\n", .{errorDescription(err)});

            return null;
        };
    }

    fn parseCpuModelOption(
        self: Cli,
        comptime command_usage: []const u8,
        target: std.Target,
        argument_iterator: *std.process.ArgIterator,
    ) ?*const std.Target.Cpu.Model {
        const argument = self.nextArgument(
            command_usage,
            "cpu model",
            argument_iterator,
        ) orelse return null;

        return target.cpu.arch.parseCpuModel(argument) catch |err| switch (err) {
            error.UnknownCpuModel => {
                std.debug.print("Usage: available cpu models:\n", .{});

                for (target.cpu.arch.allCpuModels()) |cpu_model| {
                    std.debug.print("\t{s}\n", .{cpu_model.name});
                }

                std.debug.print("\nError: unrecognized cpu model: {s}\n", .{argument});

                return null;
            },
        };
    }

    fn parseCpuFeaturesOption(
        self: Cli,
        comptime command_usage: []const u8,
        comptime add: bool,
        target: *std.Target,
        argument_iterator: *std.process.ArgIterator,
    ) bool {
        const argument = self.nextArgument(
            command_usage,
            "cpu features",
            argument_iterator,
        ) orelse return false;

        const all_cpu_features = target.cpu.arch.allFeaturesList();

        var splitted_argument_iterator = std.mem.splitScalar(u8, argument, ',');

        while (splitted_argument_iterator.next()) |splitted_argument| {
            var reached_cpu_feature: bool = false;

            for (all_cpu_features) |cpu_feature| {
                if (std.mem.eql(u8, cpu_feature.name, splitted_argument)) {
                    if (add) {
                        target.cpu.features.addFeature(cpu_feature.index);
                    } else {
                        target.cpu.features.removeFeature(cpu_feature.index);
                    }

                    reached_cpu_feature = true;

                    break;
                }
            }

            if (!reached_cpu_feature) {
                std.debug.print("Usage: available cpu features:\n", .{});

                for (all_cpu_features) |cpu_feature| {
                    std.debug.print("\t{s}\n", .{cpu_feature.name});
                }

                std.debug.print("\nError: unrecognized cpu feature: {s}\n", .{splitted_argument});

                return false;
            }
        }

        return true;
    }

    fn parseArguments(allocator: std.mem.Allocator, argument_iterator: *std.process.ArgIterator) ?Cli {
        var self: Cli = .{
            .allocator = allocator,
            .program = argument_iterator.next().?,
        };

        var target: std.Target = builtin.target;
        var output_kind: OutputKind = .executable;
        var runner_kind: RunnerKind = .executable;
        var code_model: CodeModel = .default;
        var optimization_mode: OptimizationMode = .debug;

        while (argument_iterator.next()) |argument| {
            if (std.mem.eql(u8, argument, "compile")) {
                const root_file_path = self.nextArgument(
                    Command.Compile.usage,
                    "root file path",
                    argument_iterator,
                ) orelse return null;

                var maybe_output_file_path: ?[]const u8 = null;

                while (argument_iterator.next()) |next_argument| {
                    if (std.mem.eql(u8, next_argument, "--output")) {
                        maybe_output_file_path = self.nextArgument(
                            Command.Compile.usage,
                            "output file path",
                            argument_iterator,
                        ) orelse return null;
                    } else if (std.mem.eql(u8, next_argument, "--emit")) {
                        output_kind = self.parseEnumOption(
                            OutputKind,
                            Command.Compile.usage,
                            "output kind",
                            argument_iterator,
                        ) orelse return null;
                    } else if (std.mem.eql(u8, next_argument, "--runner")) {
                        runner_kind = self.parseEnumOption(
                            RunnerKind,
                            Command.Compile.usage,
                            "runner kind",
                            argument_iterator,
                        ) orelse return null;
                    } else if (std.mem.eql(u8, next_argument, "--target")) {
                        target = self.parseTargetQueryOption(
                            Command.Compile.usage,
                            argument_iterator,
                        ) orelse return null;
                    } else if (std.mem.eql(u8, next_argument, "--cpu-model")) {
                        target.cpu.model = self.parseCpuModelOption(
                            Command.Compile.usage,
                            target,
                            argument_iterator,
                        ) orelse return null;
                    } else if (std.mem.eql(u8, next_argument, "--add-cpu-features")) {
                        if (!self.parseCpuFeaturesOption(
                            Command.Compile.usage,
                            true,
                            &target,
                            argument_iterator,
                        )) return null;
                    } else if (std.mem.eql(u8, next_argument, "--remove-cpu-features")) {
                        if (!self.parseCpuFeaturesOption(
                            Command.Compile.usage,
                            false,
                            &target,
                            argument_iterator,
                        )) return null;
                    } else if (std.mem.eql(u8, next_argument, "--code-model")) {
                        code_model = self.parseEnumOption(
                            CodeModel,
                            Command.Compile.usage,
                            "code model",
                            argument_iterator,
                        ) orelse return null;
                    } else if (std.mem.eql(u8, next_argument, "--optimize")) {
                        optimization_mode = self.parseEnumOption(
                            OptimizationMode,
                            Command.Compile.usage,
                            "optimization mode",
                            argument_iterator,
                        ) orelse return null;
                    } else {
                        std.debug.print(Command.Compile.usage, .{self.program});

                        std.debug.print("Error: unrecognized argument: {s}\n", .{next_argument});

                        return null;
                    }
                }

                self.command = .{
                    .compile = .{
                        .root_file_path = root_file_path,
                        .maybe_output_file_path = maybe_output_file_path,
                        .output_kind = output_kind,
                        .runner_kind = runner_kind,
                        .target = target,
                        .code_model = code_model,
                        .optimization_mode = optimization_mode,
                    },
                };
            } else if (std.mem.eql(u8, argument, "run")) {
                const root_file_path = self.nextArgument(
                    Command.Run.usage,
                    "root file path",
                    argument_iterator,
                ) orelse return null;

                while (argument_iterator.next()) |next_argument| {
                    if (std.mem.eql(u8, next_argument, "--runner")) {
                        runner_kind = self.parseEnumOption(
                            RunnerKind,
                            Command.Run.usage,
                            "runner kind",
                            argument_iterator,
                        ) orelse return null;
                    } else if (std.mem.eql(u8, next_argument, "--target")) {
                        target = self.parseTargetQueryOption(
                            Command.Run.usage,
                            argument_iterator,
                        ) orelse return null;
                    } else if (std.mem.eql(u8, next_argument, "--cpu-model")) {
                        target.cpu.model = self.parseCpuModelOption(
                            Command.Run.usage,
                            target,
                            argument_iterator,
                        ) orelse return null;
                    } else if (std.mem.eql(u8, next_argument, "--code-model")) {
                        code_model = self.parseEnumOption(
                            CodeModel,
                            Command.Run.usage,
                            "code model",
                            argument_iterator,
                        ) orelse return null;
                    } else if (std.mem.eql(u8, next_argument, "--add-cpu-features")) {
                        if (!self.parseCpuFeaturesOption(
                            Command.Run.usage,
                            true,
                            &target,
                            argument_iterator,
                        )) return null;
                    } else if (std.mem.eql(u8, next_argument, "--remove-cpu-features")) {
                        if (!self.parseCpuFeaturesOption(
                            Command.Run.usage,
                            false,
                            &target,
                            argument_iterator,
                        )) return null;
                    } else if (std.mem.eql(u8, next_argument, "--optimize")) {
                        optimization_mode = self.parseEnumOption(
                            OptimizationMode,
                            Command.Run.usage,
                            "optimization mode",
                            argument_iterator,
                        ) orelse return null;
                    } else if (std.mem.eql(u8, next_argument, "--")) {
                        var remaining_arguments: std.ArrayListUnmanaged([]const u8) = .{};

                        while (argument_iterator.next()) |remaining_argument| {
                            remaining_arguments.append(self.allocator, remaining_argument) catch |err| {
                                std.debug.print("Error: {s}\n", .{errorDescription(err)});

                                return null;
                            };
                        }

                        self.command = .{
                            .run = .{
                                .root_file_path = root_file_path,
                                .runner_kind = runner_kind,
                                .target = target,
                                .code_model = code_model,
                                .optimization_mode = optimization_mode,
                                .arguments = remaining_arguments.toOwnedSlice(self.allocator) catch |err| {
                                    std.debug.print("Error: {s}\n", .{errorDescription(err)});

                                    return null;
                                },
                            },
                        };

                        return self;
                    }
                }

                self.command = .{
                    .run = .{
                        .root_file_path = root_file_path,
                        .runner_kind = runner_kind,
                        .target = target,
                        .code_model = code_model,
                        .optimization_mode = optimization_mode,
                        .arguments = &.{},
                    },
                };
            } else if (std.mem.eql(u8, argument, "help")) {
                self.command = .help;
            } else {
                std.debug.print(usage, .{self.program});

                std.debug.print("Error: {s} is an unknown command\n", .{argument});

                return null;
            }
        }

        if (self.command == null) {
            std.debug.print(usage, .{self.program});

            std.debug.print("Error: no command provided\n", .{});

            return null;
        }

        return self;
    }

    fn compile(
        self: Cli,
        root_file_path: []const u8,
        maybe_output_file_path: ?[]const u8,
        output_kind: OutputKind,
        runner_kind: RunnerKind,
        target: std.Target,
        code_model: CodeModel,
        optimization_mode: OptimizationMode,
    ) u8 {
        var barq_lib_dir = Compilation.BarqLib.openDir() catch |err| {
            std.debug.print("Error: could not open the barq library directory: {s}\n", .{errorDescription(err)});

            return 1;
        };

        defer barq_lib_dir.close();

        var barq_lib_std_file = barq_lib_dir.openFile("std.bq", .{}) catch |err| {
            std.debug.print("Error: could not open the 'std.bq' file in barq library directory: {s}\n", .{errorDescription(err)});

            return 1;
        };

        defer barq_lib_std_file.close();

        var barq_lib_std_file_path_buffer: [std.fs.max_path_bytes]u8 = undefined;
        const barq_lib_std_file_path = barq_lib_dir.realpath("std.bq", &barq_lib_std_file_path_buffer) catch |err| {
            std.debug.print("Error: could not get the real 'std.bq' file path in barq library directory: {s}\n", .{errorDescription(err)});

            return 1;
        };

        var root_file = std.fs.cwd().openFile(root_file_path, .{}) catch |err| {
            std.debug.print("Error: could not open root file '{s}': {s}\n", .{ root_file_path, errorDescription(err) });

            return 1;
        };

        defer root_file.close();

        const root_file_buffer = root_file.readToEndAllocOptions(self.allocator, std.math.maxInt(u32), null, @alignOf(u8), 0) catch |err| {
            std.debug.print("Error: could not read root file '{s}': {s}\n", .{ root_file_path, errorDescription(err) });

            return 1;
        };

        var compilation: Compilation = .{
            .allocator = self.allocator,

            .root_file = .{ .path = root_file_path, .buffer = root_file_buffer },

            .barq_lib = .{
                .dir = barq_lib_dir,
                .std_file = barq_lib_std_file,
                .std_file_path = barq_lib_std_file_path,
            },

            .target = target,
            .optimization_mode = optimization_mode,
            .code_model = code_model,
        };

        const compilation_file: Compilation.File = switch (runner_kind) {
            .executable => blk: {
                const relative_runner_file_path = "std" ++ std.fs.path.sep_str ++ "runner" ++ std.fs.path.sep_str ++ "exe.bq";

                var runner_file_path_buffer: [std.fs.max_path_bytes]u8 = undefined;

                const runner_file_path = barq_lib_dir.realpath(relative_runner_file_path, &runner_file_path_buffer) catch |err| {
                    std.debug.print("Error: could not find executable runner file path: {s}\n", .{errorDescription(err)});

                    return 1;
                };

                const runner_file = std.fs.cwd().openFile(runner_file_path, .{}) catch |err| {
                    std.debug.print("Error: could not open executable runner file: {s}\n", .{errorDescription(err)});

                    return 1;
                };

                const runner_file_buffer = runner_file.readToEndAllocOptions(self.allocator, std.math.maxInt(u32), null, @alignOf(u8), 0) catch |err| {
                    std.debug.print("Error: could not read executable runner file: {s}\n", .{errorDescription(err)});

                    return 1;
                };

                break :blk .{ .path = runner_file_path, .buffer = runner_file_buffer };
            },

            .none => .{ .path = root_file_path, .buffer = root_file_buffer },
        };

        var sir_parser = Sir.Parser.init(self.allocator, &compilation, compilation_file) catch |err| {
            std.debug.print("Error: {s}\n", .{Cli.errorDescription(err)});

            return 1;
        };

        defer sir_parser.deinit();

        sir_parser.parse() catch |err| switch (err) {
            error.OutOfMemory => {
                std.debug.print("Error: {s}\n", .{Cli.errorDescription(err)});

                return 1;
            },

            error.WithMessage => {
                std.debug.print("{s}:{}:{}: {s}\n", .{
                    compilation_file.path,
                    sir_parser.error_info.?.source_loc.line,
                    sir_parser.error_info.?.source_loc.column,
                    sir_parser.error_info.?.message,
                });

                return 1;
            },

            error.WithoutMessage => {},
        };

        var globals: Sema.Globals = .{};

        const module_id = compilation.putModule(.{
            .file = compilation_file,
            .sir = sir_parser.sir,
            .globals = &globals,
        }) catch |err| {
            std.debug.print("Error: {s}\n", .{Cli.errorDescription(err)});

            return 1;
        };

        var air: Air = .{};

        var sema = Sema.init(self.allocator, &compilation, @intCast(module_id), &air) catch |err| {
            std.debug.print("Error: {s}\n", .{Cli.errorDescription(err)});

            return 1;
        };

        defer sema.deinit();

        sema.analyze() catch |err| switch (err) {
            error.OutOfMemory => {
                std.debug.print("Error: {s}\n", .{Cli.errorDescription(err)});

                return 1;
            },

            error.WithMessage => {
                std.debug.print("{s}:{}:{}: {s}\n", .{
                    compilation_file.path,
                    sema.error_info.?.source_loc.line,
                    sema.error_info.?.source_loc.column,
                    sema.error_info.?.message,
                });

                return 1;
            },

            error.WithoutMessage => return 1,
        };

        switch (output_kind) {
            .assembly => {
                const assembly_file_path = std.fmt.allocPrintZ(self.allocator, "{s}{s}", .{
                    maybe_output_file_path orelse std.fs.path.stem(root_file_path),
                    if (maybe_output_file_path != null) "" else ".s",
                }) catch |err| {
                    std.debug.print("Error: {s}\n", .{errorDescription(err)});

                    return 1;
                };

                defer self.allocator.free(assembly_file_path);

                compilation.emit(air, assembly_file_path, .assembly) catch |err| {
                    std.debug.print("Error: could not emit assembly file: {s}\n", .{errorDescription(err)});

                    return 1;
                };
            },

            .object, .executable => {
                const object_file_path = std.fmt.allocPrintZ(self.allocator, "{s}{s}", .{
                    maybe_output_file_path orelse std.fs.path.stem(root_file_path),
                    if (maybe_output_file_path != null and output_kind == .object) "" else target.ofmt.fileExt(target.cpu.arch),
                }) catch |err| {
                    std.debug.print("Error: {s}\n", .{errorDescription(err)});

                    return 1;
                };

                defer self.allocator.free(object_file_path);

                compilation.emit(air, object_file_path, output_kind) catch |err| {
                    std.debug.print("Error: could not emit object file: {s}\n", .{errorDescription(err)});

                    return 1;
                };

                if (output_kind == .executable) {
                    const exe_file_path = std.fmt.allocPrintZ(self.allocator, "{s}{s}", .{
                        maybe_output_file_path orelse std.fs.path.stem(root_file_path),
                        if (maybe_output_file_path != null) "" else target.exeFileExt(),
                    }) catch |err| {
                        std.debug.print("Error: {s}\n", .{errorDescription(err)});

                        return 1;
                    };

                    const linker_exit_code = compilation.link(
                        object_file_path,
                        exe_file_path,
                    ) catch |err| {
                        std.debug.print("Error: could not link object file: {s}\n", .{errorDescription(err)});

                        return 1;
                    };

                    if (linker_exit_code != 0) {
                        std.debug.print(
                            "Error: could not link object file, linker exited with non-zero exit code: {}\n",
                            .{linker_exit_code},
                        );

                        return linker_exit_code;
                    }

                    std.fs.cwd().deleteFile(object_file_path) catch |err| {
                        std.debug.print("Error: could not delete object file: {s}\n", .{errorDescription(err)});

                        return 1;
                    };
                }
            },

            .none => {},
        }

        return 0;
    }

    fn executeCompileCommand(self: Cli) u8 {
        const options = self.command.?.compile;

        return self.compile(
            options.root_file_path,
            options.maybe_output_file_path,
            options.output_kind,
            options.runner_kind,
            options.target,
            options.code_model,
            options.optimization_mode,
        );
    }

    fn executeRunCommand(self: Cli) u8 {
        const options = self.command.?.run;

        const output_file_path = std.fs.path.stem(options.root_file_path);

        const compile_step_exit_code = self.compile(
            options.root_file_path,
            output_file_path,
            .executable,
            options.runner_kind,
            options.target,
            options.code_model,
            options.optimization_mode,
        );

        if (compile_step_exit_code != 0) return compile_step_exit_code;

        const real_output_file_path = std.fs.realpathAlloc(self.allocator, output_file_path) catch |err| {
            std.debug.print("Error: could not resolve output file path: {s}\n", .{errorDescription(err)});

            return 1;
        };

        defer self.allocator.free(real_output_file_path);

        const exe_process_argv = std.mem.concat(self.allocator, []const u8, &.{
            &.{real_output_file_path},
            options.arguments,
        }) catch |err| {
            std.debug.print("Error: {s}\n", .{errorDescription(err)});

            return 1;
        };

        defer self.allocator.free(exe_process_argv);

        var exe_process = std.process.Child.init(exe_process_argv, self.allocator);
        exe_process.stdin_behavior = .Inherit;
        exe_process.stdout_behavior = .Inherit;
        exe_process.stderr_behavior = .Inherit;

        const termination = exe_process.spawnAndWait() catch |err| {
            std.debug.print("Error: could not run executable: {s}\n", .{errorDescription(err)});

            return 1;
        };

        std.fs.cwd().deleteFile(output_file_path) catch |err| {
            std.debug.print("Error: could not delete output file: {s}\n", .{errorDescription(err)});

            return 1;
        };

        switch (termination) {
            .Exited => |code| return code,

            else => return 1,
        }
    }

    fn executeHelpCommand(self: Cli) u8 {
        std.debug.print(usage, .{self.program});

        return 0;
    }
};

pub fn main() u8 {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};

    const allocator = gpa.allocator();

    var argument_iterator = std.process.ArgIterator.initWithAllocator(allocator) catch |err| {
        std.debug.print("Error: {s}\n", .{Cli.errorDescription(err)});

        return 1;
    };

    defer argument_iterator.deinit();

    const cli = Cli.parseArguments(allocator, &argument_iterator) orelse return 1;

    switch (cli.command.?) {
        .compile => return cli.executeCompileCommand(),
        .run => return cli.executeRunCommand(),
        .help => return cli.executeHelpCommand(),
    }
}
