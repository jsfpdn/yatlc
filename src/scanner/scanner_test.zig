const std = @import("std");

const scanner = @import("scanner.zig");
const Scanner = scanner.Scanner;

const token = @import("token.zig");
const Token = token.Token;
const TokenType = token.TokenType;

const reporter = @import("reporter.zig");
const Reporter = reporter.Reporter;

test {
    // Driver code to run all tests in this package.
    std.testing.refAllDeclsRecursive(token);
    std.testing.refAllDeclsRecursive(@This());
}

const tuple = struct { symbol: []const u8, tokenType: TokenType };

fn expectTokensEqual(want: Token, got: Token) !void {
    try std.testing.expectEqual(want.tokenType, got.tokenType);
    try std.testing.expectEqual(want.bufferLoc.start, got.bufferLoc.start);
    try std.testing.expectEqual(want.bufferLoc.end, got.bufferLoc.end);
    try std.testing.expectEqual(want.sourceLoc.line, got.sourceLoc.line);
    try std.testing.expectEqual(want.sourceLoc.column, got.sourceLoc.column);
    try std.testing.expectEqualStrings(want.symbol, got.symbol);
}

test "eat EOF" {
    const contents = "";
    var r = &Reporter.init(contents, "testing_file", std.io.getStdOut().writer());
    var s = Scanner.init(contents, r);

    const tok = s.next();
    try expectTokensEqual(Token{ .tokenType = TokenType.EOF, .bufferLoc = Token.BufferLoc{ .start = 0, .end = 0 }, .sourceLoc = Token.SourceLoc{ .line = 1, .column = 1 }, .symbol = "EOF" }, tok);
}

test "OP and OP_ASSIGN tokens with locations" {
    const contents = " += +   -= -";
    var r = &Reporter.init(contents, "testing_file", std.io.getStdOut().writer());
    var s = Scanner.init(contents, r);

    const cases = [_]Token{
        Token{ .tokenType = TokenType.ADD_ASSIGN, .bufferLoc = Token.BufferLoc{ .start = 1, .end = 2 }, .sourceLoc = Token.SourceLoc{ .line = 1, .column = 2 }, .symbol = "+=" },
        Token{ .tokenType = TokenType.ADD, .bufferLoc = Token.BufferLoc{ .start = 4, .end = 4 }, .sourceLoc = Token.SourceLoc{ .line = 1, .column = 5 }, .symbol = "+" },
        Token{ .tokenType = TokenType.SUB_ASSIGN, .bufferLoc = Token.BufferLoc{ .start = 8, .end = 9 }, .sourceLoc = Token.SourceLoc{ .line = 1, .column = 9 }, .symbol = "-=" },
        Token{ .tokenType = TokenType.SUB, .bufferLoc = Token.BufferLoc{ .start = 11, .end = 11 }, .sourceLoc = Token.SourceLoc{ .line = 1, .column = 12 }, .symbol = "-" },
        Token{ .tokenType = TokenType.EOF, .bufferLoc = Token.BufferLoc{ .start = 12, .end = 12 }, .sourceLoc = Token.SourceLoc{ .line = 1, .column = 13 }, .symbol = "EOF" },
    };

    var tok: token.Token = undefined;
    for (cases) |tc| {
        tok = s.next();
        try std.testing.expectEqualStrings(tc.symbol, tok.symbol);
        try std.testing.expectEqual(tc.tokenType, tok.tokenType);
    }
}

test "scan identifiers, keywords, and builtin" {
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

    const contents = "break continue \n\t else if return while identifier Identifier Identifier2 __Ident_ifier_ @toBool \t\n @toInt @toFloat @len @print @read \n\t";
    var r = &Reporter.init(contents, "testing_file", std.io.getStdOut().writer());
    var s = Scanner.init(contents, r);

    var tok: token.Token = undefined;
    for (cases) |tc| {
        tok = s.next();
        try std.testing.expectEqualStrings(tc.symbol, tok.symbol);
        try std.testing.expectEqual(tc.tokenType, tok.tokenType);
    }
}

test "scan invalid tokens" {
    const contents = "  _ \t\n ____ @nonExisting_Builtin2 \n";
    var r = &Reporter.init(contents, "testing_file", std.io.getStdOut().writer());
    var s = Scanner.init(contents, r);

    const cases = [_]tuple{
        .{ .symbol = "_", .tokenType = TokenType.ILLEGAL },
        .{ .symbol = "____", .tokenType = TokenType.ILLEGAL },
        .{ .symbol = "@nonExisting_Builtin2", .tokenType = TokenType.ILLEGAL },
    };

    var tok: token.Token = undefined;
    for (cases) |tc| {
        tok = s.next();
        try std.testing.expectEqualStrings(tc.symbol, tok.symbol);
        try std.testing.expectEqual(tc.tokenType, tok.tokenType);
    }
}

test "scan number literals" {
    const contents = "123 0 0123 .23 123.45 0b1001 0x14AaF 0o1237";
    var r = &Reporter.init(contents, "testing_file", std.io.getStdOut().writer());
    var s = Scanner.init(contents, r);

    const cases = [_]tuple{
        .{ .symbol = "123", .tokenType = TokenType.INT },
        .{ .symbol = "0", .tokenType = TokenType.INT },
        .{ .symbol = "0123", .tokenType = TokenType.INT },
        .{ .symbol = ".23", .tokenType = TokenType.FLOAT },
        .{ .symbol = "123.45", .tokenType = TokenType.FLOAT },
        //     .{ .symbol = "0b1001", .tokenType = TokenType.INT },
        //     .{ .symbol = "0x14AaF", .tokenType = TokenType.INT },
        //     .{ .symbol = "0o1237", .tokenType = TokenType.INT },
    };

    var tok: token.Token = undefined;
    for (cases) |tc| {
        tok = s.next();
        try std.testing.expectEqualStrings(tc.symbol, tok.symbol);
        try std.testing.expectEqual(tc.tokenType, tok.tokenType);
    }
}

test "scan malformed number literals" {
    // TODO(jsfpdn): Test binary numbers with digits > 1 etc.
}

test "scan string literals" {
    const contents =
        \\ "this is a string literal!" "multiline
        \\string literal!"
        \\ "this has \" escaped quotes!"
    ;

    var r = &Reporter.init(contents, "testing_file", std.io.getStdOut().writer());
    var s = Scanner.init(contents, r);

    const cases = [_]tuple{
        .{ .symbol = "\"this is a string literal!\"", .tokenType = TokenType.STRING },
        .{ .symbol = "\"multiline\nstring literal!\"", .tokenType = TokenType.STRING },
        .{ .symbol = "\"this has \\\" escaped quotes!\"", .tokenType = TokenType.STRING },
    };

    var tok: token.Token = undefined;
    for (cases) |tc| {
        tok = s.next();
        try std.testing.expectEqualStrings(tc.symbol, tok.symbol);
        try std.testing.expectEqual(tc.tokenType, tok.tokenType);
    }
}

test "scan unclosed string literals" {
    const contents = "\"look at me, I'm unclosed!";
    var r = &Reporter.init(contents, "testing_file", std.io.getStdOut().writer());
    var s = Scanner.init(contents, r);

    var tok = s.next();
    try std.testing.expectEqual(TokenType.ILLEGAL, tok.tokenType);
    try std.testing.expectEqualStrings("\"look at me, I'm unclosed!", tok.symbol);
}

test "scan comments" {
    const contents =
        \\ // This is a comment 1!
        \\      // This is a // comment 2!
        \\
        \\ /* multiline comment! */
        \\ /* multiline comment!
        \\      /* nested multiline comment! */
        \\  this is also a comment */
    ;
    var r = &Reporter.init(contents, "testing_file", std.io.getStdOut().writer());
    var s = Scanner.init(contents, r);

    const nestedComment =
        \\/* multiline comment!
        \\      /* nested multiline comment! */
        \\  this is also a comment */
    ;

    const cases = [_]tuple{
        .{ .symbol = "// This is a comment 1!", .tokenType = TokenType.COMMENT },
        .{ .symbol = "// This is a // comment 2!", .tokenType = TokenType.COMMENT },
        .{ .symbol = "/* multiline comment! */", .tokenType = TokenType.COMMENT },
        .{ .symbol = nestedComment, .tokenType = TokenType.COMMENT },
    };

    var tok: token.Token = undefined;
    for (cases) |tc| {
        tok = s.next();
        try std.testing.expectEqualStrings(tc.symbol, tok.symbol);
        try std.testing.expectEqual(tc.tokenType, tok.tokenType);
    }
}

test "scan malformed multiline comments" {
    const contents =
        \\/* multiline comment that won't close!
        \\      /* nested multiline comment! */
    ;
    var r = &Reporter.init(contents, "testing_file", std.io.getStdOut().writer());
    var s = Scanner.init(contents, r);

    var tok = s.next();
    try std.testing.expectEqualStrings(contents, tok.symbol);
    try std.testing.expectEqual(TokenType.ILLEGAL, tok.tokenType);
}
