const std = @import("std");

/// Convert double to hexadecimal.
pub fn doubleToHexadecimal(alloc: std.mem.Allocator, double: f64) []const u8 {
    var bits: u64 = @bitCast(double);
    var ret = [_]u8{ 48, 120, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 97, 98, 99, 100, 101, 102 };

    for (1..17) |j| {
        var i = 18 - j;
        var hexDigit: u8 = @truncate(bits & 15);
        ret[i] = hexadecimalSymbol(hexDigit);
        bits >>= 4;
    }

    return std.fmt.allocPrint(alloc, "{s}", .{ret}) catch unreachable;
}

fn hexadecimalSymbol(digit: u8) u8 {
    return if (digit <= 9) digit + 48 else digit + 87;
}
