const std = @import("std");

const io = std.io;

const scanner = @import("scanner.zig");
const token = @import("token.zig");

const Token = scanner.Token;

pub const Level = enum {
    WARNING,
    ERROR,
    NOTE,

    pub fn str(self: Level) [:0]const u8 {
        return switch (self) {
            Level.WARNING => "WARNING",
            Level.ERROR => "ERROR",
            Level.NOTE => "NOTE",
        };
    }
};

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
    pub fn report(self: *const Reporter, tok: Token, level: Level, msg: []const u8) void {
        // TODO(jsfpdn): fix reporting of multiline comments.
        const loc = self.line(tok);

        self.writer.print("{s}: {s}:{d}:{d}: {s}.\n", .{ level.str(), self.file, tok.sourceLoc.line, tok.sourceLoc.column, msg }) catch unreachable;
        self.writer.print("    {s}\n", .{self.contents[loc.start..loc.end]}) catch unreachable;

        self.pad(' ', 4 + tok.bufferLoc.start - loc.start);
        self.writer.print("^\n", .{}) catch unreachable;
    }

    /// line returns the beginning and end of the line where the token resides.
    fn line(self: *const Reporter, tok: Token) token.BufferLoc {
        var loc = token.BufferLoc{ .start = tok.bufferLoc.start, .end = tok.bufferLoc.end };

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
        for (0..times - 1) |_| {
            self.writer.print("{c}", .{c}) catch unreachable;
        }
    }
};
