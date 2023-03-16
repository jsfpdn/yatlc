const std = @import("std");
const clap = @import("clap");

const fs = std.fs;
const io = std.io;

const tokenizer = @import("tokenizer/tokenizer.zig");

// TODO(jsfpdn):
//  * pass files from stdin
//  * parse command line arguments

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
    if (res.args.help)
        std.debug.print("help!\n", .{});
    if (res.args.verbose)
        std.debug.print("verbose!\n", .{});
    for (res.positionals) |pos, i|
        std.debug.print("positional {d}: {s}\n", .{ i, pos });

    const file: fs.File = try fs.cwd().openFile("build.zig", .{});
    defer file.close();

    const reader = file.reader();
    var gpa = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    const contents = try reader.readAllAlloc(allocator, MAX_BYTES);
    defer allocator.free(contents);

    const tok = tokenizer.Tokenizer{ .contents = contents };
    tok.printContents();
}
