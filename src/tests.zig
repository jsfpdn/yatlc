const std = @import("std");

const tokenizer = @import("tokenizer/tokenizer.zig");

test {
    // Driver code to run all tests in descendant sub-packages.
    std.testing.refAllDeclsRecursive(tokenizer);
    std.testing.refAllDeclsRecursive(@This());
}
