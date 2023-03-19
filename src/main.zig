const std = @import("std");
const clap = @import("clap");

const fs = std.fs;
const io = std.io;

// TODO(jsfpdn): create error reporter.

const scanner = @import("scanner/scanner.zig");
const logger = @import("logger/logger.zig");

const MAX_BYTES: usize = 1024 * 1024;

const description = "yatlc is a yet-another-toy-language compiler.";
const params = clap.parseParamsComptime(
    \\-h, --help        Show this help message
    \\-v, --verbose     Print debug information
    \\<str>             Path to a source file to be compiled
    \\
);

fn showHelp(writer: anytype) !void {
    _ = try writer.write(description ++ std.cstr.line_sep ++ "yatlc:" ++ std.cstr.line_sep);
    try clap.help(writer, clap.Help, &params, .{});
    _ = try writer.write(std.cstr.line_sep);
}

fn showUsage(writer: anytype) !void {
    try clap.usage(writer, clap.Help, &params);
    _ = try writer.write(std.cstr.line_sep);
}

pub fn main() !void {
    var diag = clap.Diagnostic{};
    const errWriter = io.getStdErr().writer();

    var res = clap.parse(clap.Help, &params, clap.parsers.default, .{ .diagnostic = &diag }) catch |err| {
        diag.report(errWriter, err) catch {};
        try showUsage(errWriter);
        return;
    };
    defer res.deinit();

    if (res.args.help) {
        try showHelp(errWriter);
        return;
    }

    const log: logger.Logger = logger.Logger{ .verbose = res.args.verbose };

    if (res.positionals.len != 1) {
        log.err("Path to exactly one source file must be supplied!\n", .{});
        try showUsage(errWriter);
        return;
    }

    const filePath = res.positionals[0];

    const file: fs.File = fs.cwd().openFile(filePath, .{}) catch |err| {
        log.err("could not open file {s}: {s}", .{ filePath, @errorName(err) });
        return;
    };
    defer file.close();

    const reader = file.reader();
    var gpa = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    const contents = try reader.readAllAlloc(allocator, MAX_BYTES);
    defer allocator.free(contents);

    const tok = scanner.Scanner{ .contents = contents, .log = log };
    _ = tok;
}
