const std = @import("std");

const scanner_test = @import("scanner/scanner_test.zig");
const parser_test = @import("parser/parser_test.zig");

test {
    // Driver code to run all tests in descendant sub-packages.
    std.testing.refAllDeclsRecursive(scanner_test);
    std.testing.refAllDeclsRecursive(parser_test);
    std.testing.refAllDeclsRecursive(@This());
}
