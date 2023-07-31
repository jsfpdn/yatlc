const std = @import("std");
const builtin = @import("builtin");

const fs = std.fs;
const io = std.io;
const mem = std.mem;
const process = std.process;

// TODO(jsfpdn): Simplify imports (single main.zig file exporting all the necessary symbols in all sub-libraries)

const scanner = @import("scanner/scanner.zig");
const reporter = @import("reporter/reporter.zig");
const parser = @import("parser/parser.zig");

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
    var gpa = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = process.argsAlloc(allocator) catch |err| {
        fatal("could not read command-line arguments: {s}", .{@errorName(err)});
    };
    defer process.argsFree(allocator, args);

    if (args.len < 2) {
        fatal("received too few arguments", .{});
    }

    var opts = options{};
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
        const f = std.fs.cwd().createFile(tFile, .{ .read = false, .truncate = true }) catch |err| {
            fatal("could not create a .t file: {s}", .{@errorName(err)});
        };
        w = f.writer();
    }

    var r = reporter.Reporter.init(contents, filename, io.getStdErr().writer());
    var s = scanner.Scanner.init(contents, r, w);
    var p = parser.Parser.init(s, r, allocator);

    p.parse();
}

pub fn fatal(comptime format: []const u8, args: anytype) noreturn {
    std.log.err(format, args);
    process.exit(1);
}

pub fn argSet(arg: []const u8, short: []const u8, long: []const u8) bool {
    return mem.eql(u8, arg, short) or mem.eql(u8, arg, long);
}
