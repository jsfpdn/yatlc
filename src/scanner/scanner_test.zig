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

test "scan identifiers, keywords, and builtin" {
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
        .{ .symbol = "Identifier2", .tokenType = TokenType.IDENT },
        .{ .symbol = "__Ident_ifier_", .tokenType = TokenType.IDENT },
        .{ .symbol = "@toBool", .tokenType = TokenType.BF_TOBOOL },
        .{ .symbol = "@toInt", .tokenType = TokenType.BF_TOINT },
        .{ .symbol = "@toFloat", .tokenType = TokenType.BF_TOFLOAT },
        .{ .symbol = "@len", .tokenType = TokenType.BF_LEN },
        .{ .symbol = "@print", .tokenType = TokenType.BF_PRINT },
        .{ .symbol = "@read", .tokenType = TokenType.BF_READ },
        .{ .symbol = "EOF", .tokenType = TokenType.EOF },
    };

    var s = createTestScanner("break continue \n\t else if return while identifier Identifier Identifier2 __Ident_ifier_ @toBool \t\n @toInt @toFloat @len @print @read \n\t");

    var tok: token.Token = undefined;
    for (cases) |tc| {
        tok = s.next();
        try std.testing.expectEqualStrings(tc.symbol, s.symbol(tok));
        try std.testing.expectEqual(tc.tokenType, tok.tokenType);
    }
}

test "scan invalid tokens" {
    // TODO(jsfpdn): Once error reporting is set up, test it here as well.
    var s = createTestScanner("  ____ @nonExisting_Builtin2 \n");

    var tok = s.next();
    try std.testing.expectEqualStrings("____", s.symbol(tok));
    try std.testing.expectEqual(TokenType.ILLEGAL, tok.tokenType);

    tok = s.next();
    try std.testing.expectEqualStrings("@nonExisting_Builtin2", s.symbol(tok));
    try std.testing.expectEqual(TokenType.ILLEGAL, tok.tokenType);
}

test "scan number literals" {
    var s = createTestScanner("123 0 0123 .23 123.45 0b1001 0x14AaF 0o1237");

    var tok = s.next();
    try std.testing.expectEqual(TokenType.INT, tok.tokenType);
    try std.testing.expectEqualStrings("123", s.symbol(tok));

    tok = s.next();
    try std.testing.expectEqual(TokenType.INT, tok.tokenType);
    try std.testing.expectEqualStrings("0", s.symbol(tok));

    tok = s.next();
    try std.testing.expectEqual(TokenType.INT, tok.tokenType);
    try std.testing.expectEqualStrings("0123", s.symbol(tok));

    tok = s.next();
    try std.testing.expectEqual(TokenType.FLOAT, tok.tokenType);
    try std.testing.expectEqualStrings(".23", s.symbol(tok));

    tok = s.next();
    try std.testing.expectEqual(TokenType.FLOAT, tok.tokenType);
    try std.testing.expectEqualStrings("123.45", s.symbol(tok));

    // TODO(jsfpdn): Implement the following.
    tok = s.next();
    try std.testing.expectEqual(TokenType.INT, tok.tokenType);
    try std.testing.expectEqualStrings("0b1001", s.symbol(tok));

    tok = s.next();
    try std.testing.expectEqual(TokenType.INT, tok.tokenType);
    try std.testing.expectEqualStrings("0x14AaF", s.symbol(tok));

    tok = s.next();
    try std.testing.expectEqual(TokenType.INT, tok.tokenType);
    try std.testing.expectEqualStrings("0o1237", s.symbol(tok));
}

test "scan malformed number literals" {
    // TODO(jsfpdn): Test binary numbers with digits > 1 etc.
}

test "scan string literals" {
    var s = createTestScanner(
        \\ "this is a string literal!" "multiline
        \\string literal!"
        \\ "this has \" escaped quotes!"
    );

    var tok = s.next();
    try std.testing.expectEqual(TokenType.STRING, tok.tokenType);
    try std.testing.expectEqualStrings("\"this is a string literal!\"", s.symbol(tok));

    tok = s.next();
    try std.testing.expectEqual(TokenType.STRING, tok.tokenType);
    try std.testing.expectEqualStrings("\"multiline\nstring literal!\"", s.symbol(tok));

    tok = s.next();
    try std.testing.expectEqual(TokenType.STRING, tok.tokenType);
    try std.testing.expectEqualStrings("\"this has \\\" escaped quotes!\"", s.symbol(tok));
}

test "scan unclosed string literals" {
    var s = createTestScanner("\"look at me, I'm unclosed!");

    var tok = s.next();
    try std.testing.expectEqual(TokenType.ILLEGAL, tok.tokenType);
    try std.testing.expectEqualStrings("\"look at me, I'm unclosed!", s.symbol(tok));
}

test "scan comments" {
    var s = createTestScanner(
        \\ // This is a comment 1!
        \\      // This is a // comment 2!
        \\
        \\ /* multiline comment! */
        \\ /* multiline comment!
        \\      /* nested multiline comment! */
        \\  this is also a comment */
    );

    var tok = s.next();
    try std.testing.expectEqualStrings("// This is a comment 1!", s.symbol(tok));
    try std.testing.expectEqual(TokenType.COMMENT, tok.tokenType);

    tok = s.next();
    try std.testing.expectEqualStrings("// This is a // comment 2!", s.symbol(tok));
    try std.testing.expectEqual(TokenType.COMMENT, tok.tokenType);

    tok = s.next();
    try std.testing.expectEqual(TokenType.COMMENT, tok.tokenType);
    try std.testing.expectEqualStrings("/* multiline comment! */", s.symbol(tok));

    tok = s.next();
    try std.testing.expectEqual(TokenType.COMMENT, tok.tokenType);

    const nestedComment =
        \\/* multiline comment!
        \\      /* nested multiline comment! */
        \\  this is also a comment */
    ;

    try std.testing.expectEqualStrings(nestedComment, s.symbol(tok));
}

test "scan malformed multiline comments" {
    const comment =
        \\/* multiline comment that won't close!
        \\      /* nested multiline comment! */
    ;

    var s = createTestScanner(comment);

    var tok = s.next();
    try std.testing.expectEqualStrings(comment, s.symbol(tok));
    try std.testing.expectEqual(TokenType.ILLEGAL, tok.tokenType);
}
