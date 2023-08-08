const std = @import("std");

const types = @import("types.zig");
const parser = @import("parser.zig");
const scanner = @import("scanner.zig");
const token = @import("token.zig");

const tt = token.TokenType;

test {
    // Driver code to run all tests in this package.
    std.testing.refAllDeclsRecursive(parser);
    std.testing.refAllDeclsRecursive(@This());
}

test "parse simple expressions" {}

test "parse statements" {}

fn createSimpleType(s: types.SimpleType, alloc: std.mem.Allocator) *types.Type {
    const t = alloc.create(types.Type) catch unreachable;
    t.* = types.Type{ .sType = s };
    return t;
}

fn createArrayType(ofType: *types.Type, dimensions: usize, alloc: std.mem.Allocator) *types.Type {
    const t = alloc.create(types.Type) catch unreachable;
    t.* = types.Type{ .cType = types.Array{ .ofType = ofType, .dimensions = dimensions } };
    return t;
}

test "parse types" {
    const testCase = struct {
        input: [:0]const u8,
        wantType: ?*types.Type = null,
        wantErr: ?parser.SyntaxError = null,
        wantErrInfo: ?parser.ErrorInfo = null,
    };

    const cases = [_]testCase{
        .{ .input = "float", .wantType = createSimpleType(types.SimpleType.FLOAT, std.testing.allocator) },
        .{ .input = "double", .wantType = createSimpleType(types.SimpleType.DOUBLE, std.testing.allocator) },
        .{ .input = "i8", .wantType = createSimpleType(types.SimpleType.I8, std.testing.allocator) },
        .{ .input = "i16", .wantType = createSimpleType(types.SimpleType.I16, std.testing.allocator) },
        .{ .input = "i32", .wantType = createSimpleType(types.SimpleType.I32, std.testing.allocator) },
        .{ .input = "u8", .wantType = createSimpleType(types.SimpleType.U8, std.testing.allocator) },
        .{ .input = "u16", .wantType = createSimpleType(types.SimpleType.U16, std.testing.allocator) },
        .{ .input = "u32", .wantType = createSimpleType(types.SimpleType.U32, std.testing.allocator) },
        .{ .input = "nonsenseType", .wantErr = parser.SyntaxError.UnexpectedToken, .wantErrInfo = parser.ErrorInfo{
            .failedAt = token.Token{
                .bufferLoc = token.Token.BufferLoc{
                    .start = 0,
                    .end = 11,
                },
                .sourceLoc = token.Token.SourceLoc{
                    .line = 1,
                    .column = 1,
                },
                .symbol = "nonsenseType",
                .tokenType = tt.IDENT,
            },
            .msg = "TODO",
            .recoverable = true,
        } },
        .{ .input = "[-,-]u32", .wantType = createArrayType(createSimpleType(types.SimpleType.U32, std.testing.allocator), 2, std.testing.allocator) },
        .{ .input = "[-,-,-,-,-]bool", .wantType = createArrayType(createSimpleType(types.SimpleType.BOOL, std.testing.allocator), 5, std.testing.allocator) },
        .{ .input = "[-,-][-,-,-]bool", .wantType = createArrayType(createArrayType(createSimpleType(types.SimpleType.BOOL, std.testing.allocator), 3, std.testing.allocator), 2, std.testing.allocator) },
        // TODO: test errors.
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
            try std.testing.expectEqualDeep(tc.wantErrInfo.?, p.errs.pop());
        } else if (tc.wantType) |wantType| {
            const gotType = try p.parseType();
            defer gotType.destroy(std.testing.allocator);

            // std.log.err("\n- {}\n- {}", .{ wantType.*, gotType.* });
            try expectEqualDeep(wantType, gotType);
        }

        p.deinit();
    }
}

fn expectEqualDeep(want: *types.Type, got: *types.Type) !void {
    try std.testing.expectEqual(@intFromEnum(want.*), @intFromEnum(got.*));
    switch (want.*) {
        types.TypeTag.sType => |wantS| try std.testing.expectEqual(wantS, got.sType),
        types.TypeTag.constant => |wantConst| try std.testing.expectEqual(wantConst, got.constant),
        types.TypeTag.cType => |wantC| {
            try std.testing.expectEqual(wantC.dimensions, got.cType.dimensions);
            try expectEqualDeep(wantC.ofType, got.cType.ofType);
        },
    }
}
