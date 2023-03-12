const tokens = @import("tokens.zig");
const std = @import("std");

pub const Tokenizer = struct {
    contents: []const u8,

    pub fn printContents(self: Tokenizer) void {
        std.debug.print("{s}\n", .{self.contents});
    }
};

test {
    // Driver code to run all tests in this package.
    std.testing.refAllDeclsRecursive(tokens);
}
