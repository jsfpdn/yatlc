const std = @import("std");

const io = std.io;

const token = @import("token.zig");
const scanner = @import("scanner.zig");

const Token = token.Token;

pub const Reporter = struct {
    contents: []const u8,
    file: []const u8,

    pub fn init(contents: []const u8, file: []const u8) Reporter {
        return .{
            .contents = contents,
            .file = file,
        };
    }

    /// report prints out a diagnostic message and relevant info regarding the lexing error.
    pub fn report(self: *Reporter, tok: Token, msg: []const u8) void {
        const loc = self.line(tok);
        const stdout = io.getStdOut().writer();

        stdout.print("{s}:{d}:{d}: Could not tokenize '{s}': {s}.\n", .{ self.file, tok.sourceLoc.line, tok.sourceLoc.column, tok.symbol, msg }) catch unreachable;
        stdout.print("    {s}\n", .{self.contents[loc.start..loc.end]}) catch unreachable;

        pad(stdout, ' ', 4 + tok.bufferLoc.start - loc.start);
        stdout.print("↑", .{}) catch unreachable;
        pad(stdout, '~', tok.bufferLoc.end - tok.bufferLoc.start);
        stdout.print("\n", .{}) catch unreachable;

        pad(stdout, ' ', 4 + tok.bufferLoc.start - loc.start);
        stdout.print("└ Note: invalid token starts here\n\n", .{}) catch unreachable;
    }

    /// line returns the beginning and end of the line where the token resides.
    fn line(self: *Reporter, tok: Token) Token.BufferLoc {
        var loc = Token.BufferLoc{ .start = tok.bufferLoc.start, .end = tok.bufferLoc.end };

        // Find beginning of the line.
        while (loc.start > 1) {
            if (self.contents[loc.start] == '\n') {
                loc.start += 1;
                break;
            }
            loc.start -= 1;
        }

        // Find the end of the line.
        while (loc.end < self.contents.len) {
            if (self.contents[loc.end] == '\n') {
                break;
            }
            loc.end += 1;
        }

        return loc;
    }
};

fn pad(writer: anytype, c: u8, times: usize) void {
    var i: u32 = 0;
    while (i < times) {
        writer.print("{c}", .{c}) catch unreachable;
        i += 1;
    }
}
