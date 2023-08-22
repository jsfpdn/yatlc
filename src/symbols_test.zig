const std = @import("std");

const s = @import("symbols.zig");
const t = @import("types.zig");

const tt = @import("types_test.zig");

pub fn expectEqualSymbols(want: s.Symbol, got: s.Symbol) tt.testErrors!void {
    std.testing.expectEqual(got.name, want.name) catch return tt.testErrors.ExpectedEqual;
    std.testing.expectEqual(got.llvmName, want.llvmName) catch return tt.testErrors.ExpectedEqual;
    try tt.expectEqualDeep(got.t, want.t);
}

test "get on empty SymbolTable does not fail" {
    var st = s.SymbolTable.init(std.testing.allocator);
    defer st.deinit();

    try std.testing.expectEqual(st.get("var1"), null);
}

test "basic SymbolTable usage" {
    var st = s.SymbolTable.init(std.testing.allocator);
    defer st.deinit();

    try st.open();

    // Insert symbol and look it up.
    try st.insert(s.Symbol{ .name = "var1", .t = t.SimpleType.create(std.testing.allocator, t.SimpleType.U8) });
    try std.testing.expectEqual(st.get("var1").?.name, "var1");

    // Creating new scope does not hide symbol in already existing scopes.
    try st.open();
    try std.testing.expectEqual(st.get("var1").?.name, "var1");

    // Add symbol to the innermost scope.
    try st.insert(s.Symbol{ .name = "var2", .t = t.SimpleType.create(std.testing.allocator, t.SimpleType.U8) });
    try std.testing.expectEqual(st.get("var2").?.name, "var2");

    // Closing the scope destroys the symbol.
    st.close();
    try std.testing.expectEqual(st.get("var2"), null);

    st.close();
}
