const token = @import("scanner/token.zig");
const Token = token.Token;

pub const reporter = struct {
    contents: []const u8,


    pub fn report(tok: Token, msg: []const u8) void {}
};
