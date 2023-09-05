const std = @import("std");

const t = @import("types.zig");

const st = @import("symbols_test.zig");

test "test finding supertypes" {
    const testCase = struct {
        name: []const u8,
        fst: *t.Type,
        snd: *t.Type,
        want: ?*t.Type = null,
    };

    const cases = [_]testCase{
        .{
            .name = "u8+u8=>u8",
            .fst = t.SimpleType.create(std.testing.allocator, t.SimpleType.U8),
            .snd = t.SimpleType.create(std.testing.allocator, t.SimpleType.U8),
            .want = t.SimpleType.create(std.testing.allocator, t.SimpleType.U8),
        },
        .{
            .name = "[]+[-]=>null",
            .fst = t.Array.create(std.testing.allocator, 0, t.SimpleType.create(std.testing.allocator, t.SimpleType.U8)),
            .snd = t.Array.create(std.testing.allocator, 1, t.SimpleType.create(std.testing.allocator, t.SimpleType.U8)),
        },
        .{
            .name = "[-]+[]=>[-]",
            .fst = t.Array.create(std.testing.allocator, 1, t.SimpleType.create(std.testing.allocator, t.SimpleType.U8)),
            .snd = t.Array.create(std.testing.allocator, 0, t.SimpleType.create(std.testing.allocator, t.SimpleType.U8)),
            .want = t.Array.create(std.testing.allocator, 1, t.SimpleType.create(std.testing.allocator, t.SimpleType.U8)),
        },

        .{
            .name = "u8+unit=>null",
            .fst = t.SimpleType.create(std.testing.allocator, t.SimpleType.U8),
            .snd = t.SimpleType.create(std.testing.allocator, t.SimpleType.UNIT),
        },
        .{
            .name = "const+const=>const",
            .fst = t.Constant.create(std.testing.allocator, 0),
            .snd = t.Constant.create(std.testing.allocator, 0),
            .want = t.Constant.create(std.testing.allocator, 0),
        },
    };

    defer for (cases) |tc| {
        if (tc.want) |want| want.destroy(std.testing.allocator);
        tc.fst.destroy(std.testing.allocator);
        tc.snd.destroy(std.testing.allocator);
    };

    for (cases) |tc| {
        errdefer std.log.err("failed at '{s}'", .{tc.name});

        var got = t.leastSupertype(std.testing.allocator, tc.fst, tc.snd);
        defer if (got) |gotT| gotT.destroy(std.testing.allocator);

        if (tc.want) |wantT| {
            if (got) |gotT| {
                expectEqualDeep(wantT, gotT) catch return error.ValueNotExpected;
                continue;
            }
            std.log.err("wanted {s} but got null instead", .{wantT.str()});
            return error.ValueNotExpected;
        } else {
            if (got) |gotT| {
                std.log.err("wanted null but got {s} instead", .{gotT.str()});
                return error.ValueNotExpected;
            }
        }
    }
}

pub const testErrors = error{ExpectedEqual};

pub fn expectEqualDeep(want: *t.Type, got: *t.Type) !void {
    std.testing.expectEqual(@intFromEnum(want.*), @intFromEnum(got.*)) catch return testErrors.ExpectedEqual;
    switch (want.*) {
        t.TypeTag.simple => |wantS| std.testing.expectEqual(wantS, got.simple) catch return testErrors.ExpectedEqual,
        t.TypeTag.constant => return,
        t.TypeTag.array => |wantA| {
            std.testing.expectEqual(wantA.dimensions, got.array.dimensions) catch return testErrors.ExpectedEqual;
            try expectEqualDeep(wantA.ofType, got.array.ofType);
        },
        t.TypeTag.func => |wantF| {
            expectEqualDeep(wantF.retT, got.func.retT) catch return testErrors.ExpectedEqual;
            std.testing.expectEqual(wantF.namedParams, got.func.namedParams) catch return testErrors.ExpectedEqual;
            std.testing.expectEqual(wantF.args.items.len, got.func.args.items.len) catch return testErrors.ExpectedEqual;

            for (0..wantF.args.items.len) |i| {
                try st.expectEqualSymbols(wantF.args.items[i], got.func.args.items[i]);
            }
        },
    }
}

test "array type does not leak memory" {
    var array = t.Array.create(
        std.testing.allocator,
        2,
        t.Array.create(
            std.testing.allocator,
            2,
            t.SimpleType.create(std.testing.allocator, t.SimpleType.BOOL),
        ),
    );

    array.destroy(std.testing.allocator);
}
