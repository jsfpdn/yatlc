const std = @import("std");

pub const Logger = struct {
    verbose: bool,

    pub fn debug(self: Logger, comptime format: []const u8, args: anytype) void {
        if (self.verbose)
            std.log.debug(format ++ std.cstr.line_sep, args);
    }

    pub fn info(_: Logger, comptime format: []const u8, args: anytype) void {
        std.log.info(format ++ std.cstr.line_sep, args);
    }

    pub fn warn(_: Logger, comptime format: []const u8, args: anytype) void {
        std.log.warn(format ++ std.cstr.line_sep, args);
    }

    pub fn err(_: Logger, comptime format: []const u8, args: anytype) void {
        std.log.err(format ++ std.cstr.line_sep, args);
    }
};
