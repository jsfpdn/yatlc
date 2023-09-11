const std = @import("std");

const codegen_test = @import("codegen.zig");
const parser_test = @import("parser_test.zig");
const scanner_test = @import("scanner_test.zig");
const symbols_test = @import("symbols_test.zig");

test {
    // Driver code to run all tests in descendant sub-packages.
    std.testing.refAllDeclsRecursive(codegen_test);
    std.testing.refAllDeclsRecursive(parser_test);
    std.testing.refAllDeclsRecursive(scanner_test);
    std.testing.refAllDeclsRecursive(symbols_test);
    std.testing.refAllDeclsRecursive(@This());
}
