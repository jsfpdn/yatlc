const std = @import("std");

const io = std.io;

const token = @import("token.zig");
const scanner = @import("scanner.zig");

const Token = token.Token;

pub const Reporter = struct {
    contents: []const u8,
    file: []const u8,
    writer: std.fs.File.Writer,

    pub fn init(contents: []const u8, file: []const u8, writer: std.fs.File.Writer) Reporter {
        return .{
            .contents = contents,
            .file = file,
            .writer = writer,
        };
    }

    /// report prints out a diagnostic message and relevant info regarding the lexing error.
    pub fn report(self: *const Reporter, tok: Token, msg: []const u8) void {
        // TODO(jsfpdn): fix reporting of multiline comments.
        const loc = self.line(tok);

        self.writer.print("{s}:{d}:{d}: Could not tokenize '{s}': {s}.\n", .{ self.file, tok.sourceLoc.line, tok.sourceLoc.column, tok.symbol, msg }) catch unreachable;
        self.writer.print("    {s}\n", .{self.contents[loc.start..loc.end]}) catch unreachable;

        self.pad(' ', 4 + tok.bufferLoc.start - loc.start);
        self.writer.print("↑", .{}) catch unreachable;
        self.pad('~', tok.bufferLoc.end - tok.bufferLoc.start);
        self.writer.print("\n", .{}) catch unreachable;

        self.pad(' ', 4 + tok.bufferLoc.start - loc.start);
        self.writer.print("└ Note: invalid token starts here\n\n", .{}) catch unreachable;
    }

    /// line returns the beginning and end of the line where the token resides.
    fn line(self: *const Reporter, tok: Token) Token.BufferLoc {
        var loc = Token.BufferLoc{ .start = tok.bufferLoc.start, .end = tok.bufferLoc.end };

        // Find beginning of the line.
        while (loc.start >= 1 and self.contents[loc.start - 1] != '\n') {
            loc.start -= 1;
        }

        // Find the end of the line.
        while (loc.end < self.contents.len and self.contents[loc.end] != '\n') {
            loc.end += 1;
        }

        return loc;
    }

    fn pad(self: *const Reporter, c: u8, times: usize) void {
        var i: u32 = 0;
        while (i < times) {
            self.writer.print("{c}", .{c}) catch unreachable;
            i += 1;
        }
    }
};
