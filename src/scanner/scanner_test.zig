const std = @import("std");

const scanner = @import("scanner.zig");

const logger = @import("../logger/logger.zig");

const token = @import("token.zig");
const Token = token.Token;
const TokenType = token.TokenType;

test {
    // Driver code to run all tests in this package.
    std.testing.refAllDeclsRecursive(token);
    std.testing.refAllDeclsRecursive(@This());
}

fn expectTokensEqual(want: Token, got: Token) !void {
    try std.testing.expectEqual(want.tokenType, got.tokenType);
    try std.testing.expectEqual(want.bufferLoc.start, got.bufferLoc.start);
    try std.testing.expectEqual(want.bufferLoc.end, got.bufferLoc.end);
    try std.testing.expectEqual(want.sourceLoc.line, got.sourceLoc.line);
    try std.testing.expectEqual(want.sourceLoc.column, got.sourceLoc.column);
}

fn createTestScanner(input: []const u8) scanner.Scanner {
    return scanner.createScanner(input, logger.Logger{ .verbose = true });
}

test "eat whitespace only" {
    var s = createTestScanner("  \t\n\r  ");

    s.eatWhitespace();
    try std.testing.expect(s.eof() == true);
}

test "eat EOF" {
    var s = createTestScanner("");

    const tok = s.next();
    try expectTokensEqual(Token{ .tokenType = TokenType.EOF, .bufferLoc = Token.BufferLoc{ .start = 0, .end = 0 }, .sourceLoc = Token.SourceLoc{ .line = 1, .column = 1 } }, tok);
}

test "OP and OP_ASSIGN tokens" {
    var s = createTestScanner(" += +   -= -");

    const tokAddAssign = s.next();
    try expectTokensEqual(Token{ .tokenType = TokenType.ADD_ASSIGN, .bufferLoc = Token.BufferLoc{ .start = 1, .end = 2 }, .sourceLoc = Token.SourceLoc{ .line = 1, .column = 2 } }, tokAddAssign);

    const tokAdd = s.next();
    try expectTokensEqual(Token{ .tokenType = TokenType.ADD, .bufferLoc = Token.BufferLoc{ .start = 4, .end = 4 }, .sourceLoc = Token.SourceLoc{ .line = 1, .column = 5 } }, tokAdd);

    const tokSubAssign = s.next();
    try expectTokensEqual(Token{ .tokenType = TokenType.SUB_ASSIGN, .bufferLoc = Token.BufferLoc{ .start = 8, .end = 9 }, .sourceLoc = Token.SourceLoc{ .line = 1, .column = 9 } }, tokSubAssign);

    const tokSub = s.next();
    try expectTokensEqual(Token{ .tokenType = TokenType.SUB, .bufferLoc = Token.BufferLoc{ .start = 11, .end = 11 }, .sourceLoc = Token.SourceLoc{ .line = 1, .column = 12 } }, tokSub);

    const tokEOF = s.next();
    try expectTokensEqual(Token{ .tokenType = TokenType.EOF, .bufferLoc = Token.BufferLoc{ .start = 12, .end = 12 }, .sourceLoc = Token.SourceLoc{ .line = 1, .column = 13 } }, tokEOF);

    try std.testing.expectEqualStrings("-=", s.symbol(tokSubAssign));
}

test "scan identifiers and keywords" {
    const tuple = struct { symbol: []const u8, tokenType: TokenType };

    const cases = [_]tuple{
        .{ .symbol = "break", .tokenType = TokenType.BREAK },
        .{ .symbol = "continue", .tokenType = TokenType.CONTINUE },
        .{ .symbol = "else", .tokenType = TokenType.ELSE },
        .{ .symbol = "if", .tokenType = TokenType.IF },
        .{ .symbol = "return", .tokenType = TokenType.RETURN },
        .{ .symbol = "while", .tokenType = TokenType.WHILE },
        .{ .symbol = "identifier", .tokenType = TokenType.IDENT },
        .{ .symbol = "Identifier", .tokenType = TokenType.IDENT },
        .{ .symbol = "Identifier2", .tokenType = TokenType.IDENT }, // FIXME(jsfpdn)
        // .{ .symbol = "__Ident_ifier_", .tokenType = TokenType.IDENT },
        // TODO(jsfpdn): ^ support identifiers with underscores.
    };

    var s = createTestScanner("break continue \n\t else if return while identifier Identifier Identifier2");

    var tok: token.Token = undefined;
    for (cases) |tc| {
        tok = s.next();
        try std.testing.expectEqualStrings(tc.symbol, s.symbol(tok));
        try std.testing.expectEqual(tc.tokenType, tok.tokenType);
    }
}
