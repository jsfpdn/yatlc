const std = @import("std");

pub fn main() !void {
    std.debug.print("This is the yatl compiler.", .{});
}

test {
    // Driver code to run all tests in descendant sub-packages.
    const tokenizer = @import("tokenizer/tokenizer.zig");

    std.testing.refAllDeclsRecursive(tokenizer);
    std.testing.refAllDeclsRecursive(@This());
}
