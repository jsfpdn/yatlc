const std = @import("std");

const parser = @import("parser.zig");
const scanner = @import("scanner.zig");
const token = @import("token.zig");

test {
    // Driver code to run all tests in this package.
    std.testing.refAllDeclsRecursive(parser);
    std.testing.refAllDeclsRecursive(@This());
}

// TODO: think about testing w/ symbol tables and emitted LLVM IR (this probably means building some parser <-> llvm-cbe bridge
// to be able to mock it properly).

test "parse simple expressions" {
    const tuple = struct { expr: []const u8, parseError: ?parser.ParseError = null, failedAt: ?token.Token = null, errorMsg: ?[]const u8 = null };
    const cases = [_]tuple{
        // .{ .expr = "a" },
    };

    for (cases) |tc| {
        var s = scanner.Scanner.init(tc.expr, null);
        var p = parser.Parser.init(s, null, std.testing.allocator);

        if (p.parseExpression()) |_| {
            // all good
            try std.testing.expect(tc.parseError == null);
        } else |gotErr| {
            if (tc.parseError) |wantErr| {
                try std.testing.expect(gotErr == wantErr);
            } else {
                std.log.err("got error {s} but did not expect any error", .{@errorName(gotErr)});
                try std.testing.expect(false);
            }
        }
    }
}

test "parse statements" {
    const tuple = struct { stmnt: []const u8 };
    const cases = [_]tuple{
        .{ .stmnt = "i32 function();" },
        .{ .stmnt = "i32 testing(){}" },
        .{ .stmnt = "i32 testing(){{}}" },
        // .{ .stmnt = "i32 asd = 123;" },
    };

    for (cases) |tc| {
        var s = scanner.Scanner.init(tc.stmnt, null);
        var p = parser.Parser.init(s, null, std.testing.allocator);

        p.parse();
    }
}
