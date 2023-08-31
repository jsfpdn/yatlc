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

const description = "yatlc is a yet-another-toy-language compiler.";
const usage =
    \\Usage: yatlc [file] [option ...]
    \\
    \\General options:
    \\-h, --help            Show this help message
    \\-v, --verbose         Print debug information during compilation
    \\-t, --emittokens      Emit tokens from lexical analysis to a .t file
    \\
;

pub const options = struct {
    verbose: bool = false,
    emitTokens: bool = false,

    // TODO(jsfpdn): custom pub fmt function to print options when verbose?
};

pub fn main() !void {
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

    var opts = options{};
    for (args[1..]) |arg| {
        if (argSet(arg, "-h", "--help")) {
            return io.getStdOut().writeAll(usage);
        }
    }

    for (args[2..]) |arg| {
        if (argSet(arg, "-h", "--help")) {
            return io.getStdOut().writeAll(usage);
        } else if (argSet(arg, "-v", "--verbose")) {
            opts.verbose = true;
        } else if (argSet(arg, "-t", "--emittokens")) {
            opts.emitTokens = true;
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

    var w: ?std.fs.File.Writer = null;
    if (opts.emitTokens) {
        const tFile = std.fmt.allocPrint(allocator, "{s}.t", .{filename}) catch |err| {
            fatal("could not prepare a .t file: {s}", .{@errorName(err)});
        };
        defer allocator.free(tFile);
        const f = std.fs.cwd().createFile(tFile, .{ .read = false, .truncate = true }) catch |err| {
            fatal("could not create a .t file: {s}", .{@errorName(err)});
        };
        w = f.writer();
    }

    const llvmFilename = std.fmt.allocPrint(allocator, "{s}.ll", .{filename}) catch |err| {
        fatal("could not prepare a .t file: {s}", .{@errorName(err)});
    };
    defer allocator.free(llvmFilename);
    const llvmFile = std.fs.cwd().createFile(llvmFilename, .{ .read = false, .truncate = true }) catch |err| {
        fatal("could not create a .t file: {s}", .{@errorName(err)});
    };

    var r = reporter.Reporter.init(contents, filename, io.getStdErr().writer());
    var s = scanner.Scanner.init(contents, w);
    var c = codegen.CodeGen.init(allocator);
    var p = parser.Parser.init(allocator, s, r, c);
    defer p.deinit();
    defer c.deinit();

    p.parse(llvmFile.writer()) catch {};
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

pub fn argSet(arg: []const u8, short: []const u8, long: []const u8) bool {
    return mem.eql(u8, arg, short) or mem.eql(u8, arg, long);
}
