const tokens = @import("tokens.zig");
const std = @import("std");

const logger = @import("../logger/logger.zig");

pub const Tokenizer = struct {
    contents: []const u8,
    log: logger.Logger,

    pub fn printContents(self: Tokenizer) void {
        self.log.debug("{s}\n", .{self.contents});
    }
};

test {
    // Driver code to run all tests in this package.
    std.testing.refAllDeclsRecursive(tokens);
}
