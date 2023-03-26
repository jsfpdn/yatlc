const scanner = @import("../scanner/scanner.zig");
const reporter = @import("../reporter/reporter.zig");

pub const Parser = struct {
    scanner: scanner.Scanner,

    // Current token
    tok: scanner.Token,

    rep: reporter.Reporter,

    pub fn init(s: scanner.Scanner, r: reporter.Reporter) Parser {
        return .{
            .scanner = s,
            .tok = undefined,
            .rep = r,
        };
    }

    pub fn parse(self: *Parser) void {
        var tok: scanner.Token = undefined;

        while (tok.tokenType != scanner.TokenType.EOF) {
            tok = self.scanner.next();
        }
    }
};
