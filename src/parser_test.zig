const std = @import("std");

const t = @import("types.zig");
const parser = @import("parser.zig");
const scanner = @import("scanner.zig");
const token = @import("token.zig");
const symbols = @import("symbols.zig");

const tt = @import("types_test.zig");

test "parse types" {
    const testCase = struct {
        input: [:0]const u8,
        wantType: ?*t.Type = null,
        wantErr: ?parser.SyntaxError = null,
    };

    const cases = [_]testCase{
        .{ .input = "float", .wantType = t.SimpleType.create(std.testing.allocator, t.SimpleType.FLOAT) },
        .{ .input = "double", .wantType = t.SimpleType.create(std.testing.allocator, t.SimpleType.DOUBLE) },
        .{ .input = "i8", .wantType = t.SimpleType.create(std.testing.allocator, t.SimpleType.I8) },
        .{ .input = "i16", .wantType = t.SimpleType.create(std.testing.allocator, t.SimpleType.I16) },
        .{ .input = "i32", .wantType = t.SimpleType.create(std.testing.allocator, t.SimpleType.I32) },
        .{ .input = "u8", .wantType = t.SimpleType.create(std.testing.allocator, t.SimpleType.U8) },
        .{ .input = "u16", .wantType = t.SimpleType.create(std.testing.allocator, t.SimpleType.U16) },
        .{ .input = "u32", .wantType = t.SimpleType.create(std.testing.allocator, t.SimpleType.U32) },
        .{ .input = "nonsenseType", .wantErr = parser.SyntaxError.UnexpectedToken },
        .{ .input = "[-,-]u32", .wantType = t.Array.create(std.testing.allocator, 2, t.SimpleType.create(std.testing.allocator, t.SimpleType.U32)) },
        .{ .input = "[-,-,-,-,-]bool", .wantType = t.Array.create(std.testing.allocator, 5, t.SimpleType.create(std.testing.allocator, t.SimpleType.BOOL)) },
        .{ .input = "[-,-][-,-,-]bool", .wantType = t.Array.create(std.testing.allocator, 2, t.Array.create(std.testing.allocator, 3, t.SimpleType.create(std.testing.allocator, t.SimpleType.BOOL))) },
    };

    // Destroy all the data prepared for the tests.
    defer for (cases) |tc| if (tc.wantType) |wantType| wantType.destroy(std.testing.allocator);

    for (cases) |tc| {
        var s = scanner.Scanner.init(tc.input, null);
        var p = parser.Parser.init(s, null, std.testing.allocator);
        // parser must be `errdefer`-ed in case an error occurs.
        errdefer p.deinit();

        if (tc.wantErr) |wantErr| {
            try std.testing.expectError(wantErr, p.parseType());
        } else if (tc.wantType) |wantType| {
            const gotType = try p.parseType();
            defer gotType.destroy(std.testing.allocator);

            // std.log.err("\n- {}\n- {}", .{ wantType.*, gotType.* });
            try tt.expectEqualDeep(wantType, gotType);
        }

        p.deinit();
    }
}

test "parse correct top level statement" {
    const testCase = struct {
        input: [:0]const u8,
    };

    const cases = [_]testCase{
        .{ .input = "u32 testFunc() { return @u32(123); }" },
        .{ .input = "unit testFunc();" },
        .{ .input = "unit testFunc(u32 a);" },
        .{ .input = "[-]u32 testFunc(u32 a, [-][-]i32 b);" },
        .{ .input = "[-][-,-,-]i32 testFunc(u32 a, [-][-]i32 b);" },
    };

    for (cases) |tc| {
        var s = scanner.Scanner.init(tc.input, null);
        var p = parser.Parser.init(s, null, std.testing.allocator);
        // parser must be `errdefer`-ed in case an error occurs.
        defer p.deinit();

        try p.parse();
    }
}
