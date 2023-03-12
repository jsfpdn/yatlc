const tokens = @import("tokens.zig");
const std = @import("std");

test {
    // Driver code to run all tests in this package.
    std.testing.refAllDeclsRecursive(tokens);
}
