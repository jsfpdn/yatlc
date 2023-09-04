const std = @import("std");
const builtin = @import("builtin");

const fs = std.fs;
const io = std.io;
const mem = std.mem;
const process = std.process;

const codegen = @import("codegen.zig");
const parser = @import("parser.zig");
const reporter = @import("reporter.zig");
const scanner = @import("scanner.zig");

const MAX_BYTES: usize = 1024 * 1024;
const CLANG_ENV = "YATLC_CLANG_PATH";

const description = "yatlc is a yet-another-toy-language compiler.\n";
const usage =
    \\Usage: yatlc [file] [option ...]
    \\
    \\General options:
    \\-h, --help            Show this help message
    \\-l, --emitllvmir      Emit LLVM IR to a .ll file
    \\-o                    Name of the compiled executable. Defaults to a.out
    \\-t, --emittokens      Emit tokens from lexical analysis to a .t file
    \\-v, --verbose         Print debug information during compilation
    \\
    \\Path to clang compiler can be set via the environment variable YATLC_CLANG_PATH.
    \\Defaults just to `clang`.
    \\
;

pub const options = struct {
    verbose: bool = false,
    emitTokens: bool = false,
    emitLLVMIR: bool = false,
    clangPath: []const u8 = "clang",
    executable: []const u8,
};

pub fn main() !u8 {
    var gpa = std.heap.GeneralPurposeAllocator(.{
        .safety = true,
        .stack_trace_frames = 100,
        .never_unmap = true,
        .retain_metadata = true,
    }){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = process.argsAlloc(allocator) catch |err| {
        fatal("could not read command-line arguments: {s}", .{@errorName(err)});
    };
    defer process.argsFree(allocator, args);

    if (args.len < 1) {
        fatalHelp("received too few arguments", .{});
    }

    var opts = options{
        .executable = std.fmt.allocPrint(allocator, "a.out", .{}) catch unreachable,
        .clangPath = if (std.os.getenv(CLANG_ENV)) |path| path else "clang",
    };
    defer allocator.free(opts.executable);

    for (args[1..]) |arg| {
        if (argSet(arg, "-h", "--help")) {
            io.getStdOut().writeAll(description) catch unreachable;
            io.getStdOut().writeAll(usage) catch unreachable;
            return 0;
        }
    }

    var i: usize = 2;
    while (i < args.len) {
        var arg = args[i];
        i += 1;

        if (argSet(arg, "-h", "--help")) {
            unreachable;
        } else if (argSet(arg, "-v", "--verbose")) {
            opts.verbose = true;
        } else if (argSet(arg, "-l", "--emitllvmir")) {
            opts.emitLLVMIR = true;
        } else if (argSet(arg, "-t", "--emittokens")) {
            opts.emitTokens = true;
        } else if (argSet(arg, "-o", null)) {
            if (i + 1 > args.len) {
                fatal("argument to -o is missing", .{});
            }

            allocator.free(opts.executable);
            opts.executable = std.fmt.allocPrint(allocator, "{s}", .{args[i]}) catch unreachable;

            i += 1;
        } else {
            fatal("unrecognized option {s}", .{arg});
        }
    }

    if (args.len < 2) {
        fatalHelp("received too few arguments", .{});
    }

    const filename = args[1];
    const file: fs.File = fs.cwd().openFile(filename, .{}) catch |err| {
        fatal("could not open file {s}: {s}", .{ filename, @errorName(err) });
    };
    defer file.close();

    const reader = file.reader();
    const contents = try reader.readAllAlloc(allocator, MAX_BYTES);
    defer allocator.free(contents);

    var basename = opts.executable;
    if (std.mem.eql(u8, opts.executable, "a.out")) {
        basename = "a";
    }

    var w: ?std.fs.File.Writer = null;
    if (opts.emitTokens) {
        const tFile = std.fmt.allocPrint(allocator, "{s}.t", .{basename}) catch |err| {
            fatal("could not prepare a .t file: {s}", .{@errorName(err)});
        };
        defer allocator.free(tFile);
        const f = std.fs.cwd().createFile(tFile, .{ .read = false, .truncate = true }) catch |err| {
            fatal("could not create a .t file: {s}", .{@errorName(err)});
        };
        w = f.writer();
    }

    const llvmFilename = std.fmt.allocPrint(allocator, "{s}.ll", .{basename}) catch |err| {
        fatal("could not prepare a .ll file: {s}", .{@errorName(err)});
    };
    defer allocator.free(llvmFilename);
    const llvmFile = std.fs.cwd().createFile(llvmFilename, .{ .read = false, .truncate = true }) catch |err| {
        fatal("could not create a .ll file: {s}", .{@errorName(err)});
    };

    defer if (!opts.emitLLVMIR) std.fs.cwd().deleteFile(llvmFilename) catch unreachable;

    var r = reporter.Reporter.init(contents, filename, io.getStdErr().writer());
    var s = scanner.Scanner.init(contents, w);
    var c = codegen.CodeGen.init(allocator);
    var p = parser.Parser.init(allocator, s, r, c);
    defer p.deinit();
    defer c.deinit();

    p.parse(llvmFile.writer()) catch {};

    const argv = &[_][]const u8{ opts.clangPath, "-Woverride-module", llvmFilename, "-o", opts.executable };
    const result = std.process.Child.exec(.{
        .allocator = allocator,
        .argv = argv,
    }) catch |err| switch (err) {
        std.os.ExecveError.FileNotFound => fatal("could not find clang at path '{s}'", .{opts.clangPath}),
        else => return err,
    };
    defer {
        allocator.free(result.stdout);
        allocator.free(result.stderr);
    }

    switch (result.term) {
        .Signal => |signal| fatal("clang ended by signal {d}", .{signal}),
        .Unknown => |unknown| fatal("clang returned with unknown value {d}", .{unknown}),
        .Stopped => fatal("clang stopped", .{}),
        else => {},
    }

    if (!std.mem.eql(u8, result.stderr, "")) {
        io.getStdOut().writeAll("clang error:\n") catch unreachable;
        io.getStdOut().writeAll(result.stderr) catch unreachable;
        return 1;
    }
    if (!std.mem.eql(u8, result.stdout, "")) {
        io.getStdOut().writeAll("clang output:\n") catch unreachable;
        io.getStdOut().writeAll(result.stdout) catch unreachable;
    }

    return 0;
}

pub fn fatal(comptime format: []const u8, args: anytype) noreturn {
    std.log.err(format, args);
    process.exit(1);
}

pub fn fatalHelp(comptime format: []const u8, args: anytype) noreturn {
    std.log.err(format, args);
    io.getStdOut().writeAll(usage) catch unreachable;
    process.exit(1);
}

pub fn argSet(arg: []const u8, short: []const u8, long: ?[]const u8) bool {
    if (mem.eql(u8, arg, short)) return true;

    if (long) |l| return mem.eql(u8, arg, l);

    return false;
}
