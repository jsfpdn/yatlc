const std = @import("std");

const scanner_test = @import("scanner_test.zig");
const parser_test = @import("parser_test.zig");
const codegen_test = @import("codegen.zig");

test {
    // Driver code to run all tests in descendant sub-packages.
    std.testing.refAllDeclsRecursive(scanner_test);
    std.testing.refAllDeclsRecursive(parser_test);
    std.testing.refAllDeclsRecursive(codegen_test);
    std.testing.refAllDeclsRecursive(@This());
}
