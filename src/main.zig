const std = @import("std");
const clap = @import("clap");

const fs = std.fs;
const io = std.io;

const tokenizer = @import("tokenizer/tokenizer.zig");
const logger = @import("logger/logger.zig");

const MAX_BYTES: usize = 1024 * 1024;

const params = clap.parseParamsComptime(
    \\-h, --help
    \\-v, --verbose
    \\<str>
    \\
);

pub fn main() !void {
    var diag = clap.Diagnostic{};
    var res = clap.parse(clap.Help, &params, clap.parsers.default, .{ .diagnostic = &diag }) catch |err| {
        diag.report(io.getStdErr().writer(), err) catch {};
        return err;
    };
    defer res.deinit();

    const log: logger.Logger = logger.Logger{ .verbose = res.args.verbose };

    if (res.positionals.len != 1) {
        log.err("Path to exactly one source file must be supplied!\n", .{});
        try clap.usage(std.io.getStdErr().writer(), clap.Help, &params);
        _ = try std.io.getStdErr().writer().write("\n");
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

    const tok = tokenizer.Tokenizer{ .contents = contents, .log = log };
    tok.printContents();
}
