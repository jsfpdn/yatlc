const std = @import("std");

const scanner = @import("scanner.zig");
const Scanner = scanner.Scanner;

const token = @import("token.zig");
const Token = token.Token;
const TokenType = token.TokenType;

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
    var s = Scanner.init(contents, null);

    const tok = s.next();
    try expectTokensEqual(Token{ .tokenType = TokenType.EOF, .bufferLoc = token.BufferLoc{ .start = 0, .end = 0 }, .sourceLoc = Token.SourceLoc{ .line = 1, .column = 1 }, .symbol = "EOF" }, tok);
}

test "OP and OP_ASSIGN tokens with locations" {
    const contents = " += +   -= -";
    var s = Scanner.init(contents, null);

    const cases = [_]Token{
        Token{ .tokenType = TokenType.ADD_ASSIGN, .bufferLoc = token.BufferLoc{ .start = 1, .end = 2 }, .sourceLoc = Token.SourceLoc{ .line = 1, .column = 2 }, .symbol = "+=" },
        Token{ .tokenType = TokenType.ADD, .bufferLoc = token.BufferLoc{ .start = 4, .end = 4 }, .sourceLoc = Token.SourceLoc{ .line = 1, .column = 5 }, .symbol = "+" },
        Token{ .tokenType = TokenType.SUB_ASSIGN, .bufferLoc = token.BufferLoc{ .start = 8, .end = 9 }, .sourceLoc = Token.SourceLoc{ .line = 1, .column = 9 }, .symbol = "-=" },
        Token{ .tokenType = TokenType.SUB, .bufferLoc = token.BufferLoc{ .start = 11, .end = 11 }, .sourceLoc = Token.SourceLoc{ .line = 1, .column = 12 }, .symbol = "-" },
        Token{ .tokenType = TokenType.EOF, .bufferLoc = token.BufferLoc{ .start = 12, .end = 12 }, .sourceLoc = Token.SourceLoc{ .line = 1, .column = 13 }, .symbol = "EOF" },
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
        .{ .symbol = "do", .tokenType = TokenType.DO },
        .{ .symbol = "identifier", .tokenType = TokenType.IDENT },
        .{ .symbol = "Identifier", .tokenType = TokenType.IDENT },
        .{ .symbol = "Identifier2", .tokenType = TokenType.IDENT },
        .{ .symbol = "__Ident_ifier_", .tokenType = TokenType.IDENT },
        .{ .symbol = "bool", .tokenType = TokenType.IDENT },
        .{ .symbol = "i32", .tokenType = TokenType.IDENT },
        .{ .symbol = "i16", .tokenType = TokenType.IDENT },
        .{ .symbol = "u32", .tokenType = TokenType.IDENT },
        .{ .symbol = "u16", .tokenType = TokenType.IDENT },
        .{ .symbol = "char", .tokenType = TokenType.IDENT },
        .{ .symbol = "str", .tokenType = TokenType.IDENT },
        .{ .symbol = "void", .tokenType = TokenType.IDENT },
        .{ .symbol = "float", .tokenType = TokenType.IDENT },
        .{ .symbol = "len", .tokenType = TokenType.LEN },
        .{ .symbol = "print", .tokenType = TokenType.PRINT },
        .{ .symbol = "readln", .tokenType = TokenType.READLN },
        .{ .symbol = "exit", .tokenType = TokenType.EXIT },
        .{ .symbol = "<", .tokenType = TokenType.LT },
        .{ .symbol = "<<", .tokenType = TokenType.B_LSH },
        .{ .symbol = "<=", .tokenType = TokenType.LEQ },
        .{ .symbol = "<<=", .tokenType = TokenType.LSH_ASSIGN },
        .{ .symbol = ">", .tokenType = TokenType.GT },
        .{ .symbol = ">>", .tokenType = TokenType.B_RSH },
        .{ .symbol = ">=", .tokenType = TokenType.GEQ },
        .{ .symbol = "not", .tokenType = TokenType.NOT },
        .{ .symbol = "EOF", .tokenType = TokenType.EOF },
    };

    const contents = "break continue \n\t else if return while \t\n do identifier Identifier Identifier2 __Ident_ifier_ bool \t\n i32 \t i16 \n\t u32 u16 char str void float len print readln exit < << <= <<= > >> >= not\n\t";
    var s = Scanner.init(contents, null);

    var tok: token.Token = undefined;
    for (cases) |tc| {
        tok = s.next();
        try std.testing.expectEqualStrings(tc.symbol, tok.symbol);
        try std.testing.expectEqual(tc.tokenType, tok.tokenType);
    }
}

test "scan invalid tokens" {
    const contents = "  _ \t\n ____ \n";
    var s = Scanner.init(contents, null);

    const cases = [_]tuple{
        .{ .symbol = "_", .tokenType = TokenType.ILLEGAL },
        .{ .symbol = "____", .tokenType = TokenType.ILLEGAL },
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
    var s = Scanner.init(contents, null);

    const cases = [_]tuple{
        .{ .symbol = "123", .tokenType = TokenType.C_INT },
        .{ .symbol = "0", .tokenType = TokenType.C_INT },
        .{ .symbol = "0123", .tokenType = TokenType.C_INT },
        .{ .symbol = ".23", .tokenType = TokenType.C_FLOAT },
        .{ .symbol = "123.45", .tokenType = TokenType.C_FLOAT },
        .{ .symbol = "0b1001", .tokenType = TokenType.C_INT },
        .{ .symbol = "0x14AaF", .tokenType = TokenType.C_INT },
        .{ .symbol = "0o1237", .tokenType = TokenType.C_INT },
    };

    var tok: token.Token = undefined;
    for (cases) |tc| {
        tok = s.next();
        try std.testing.expectEqualStrings(tc.symbol, tok.symbol);
        try std.testing.expectEqual(tc.tokenType, tok.tokenType);
    }
}

test "scan string literals" {
    const contents =
        \\ "this is a string literal!" "multiline
        \\string literal!"
        \\ "this has \" escaped quotes!"
        \\ 'a'
    ;

    var s = Scanner.init(contents, null);

    const cases = [_]tuple{
        .{ .symbol = "\"this is a string literal!\"", .tokenType = TokenType.C_STRING },
        .{ .symbol = "\"multiline\nstring literal!\"", .tokenType = TokenType.C_STRING },
        .{ .symbol = "\"this has \\\" escaped quotes!\"", .tokenType = TokenType.C_STRING },
        .{ .symbol = "'a'", .tokenType = TokenType.C_CHAR },
    };

    var tok: token.Token = undefined;
    for (cases) |tc| {
        tok = s.next();
        try std.testing.expectEqualStrings(tc.symbol, tok.symbol);
        try std.testing.expectEqual(tc.tokenType, tok.tokenType);
    }
}

test "scan character literals" {
    const contents = " 'a'  '\\a'";

    var s = Scanner.init(contents, null);
    const cases = [_]tuple{
        .{ .symbol = "'a'", .tokenType = TokenType.C_CHAR },
        .{ .symbol = "'\\a'", .tokenType = TokenType.C_CHAR },
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
    var s = Scanner.init(contents, null);

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
        \\ /*
        \\ /* comment */*/
    ;
    var s = Scanner.init(contents, null);

    // Comments are skipped.
    try std.testing.expectEqual(TokenType.EOF, s.next().tokenType);
}

test "scan malformed multiline comments" {
    const contents =
        \\/* multiline comment that won't close!
        \\      /* nested multiline comment! */
    ;
    var s = Scanner.init(contents, null);

    var tok = s.next();
    try std.testing.expectEqualStrings(contents, tok.symbol);
    try std.testing.expectEqual(TokenType.ILLEGAL, tok.tokenType);
}

test "peek lexems without modifying the state of the tokenizer" {
    const contents = "asdf 123 /* asd */";

    var s = Scanner.init(contents, null);

    var tok = s.peek();
    try std.testing.expectEqualStrings("asdf", tok.symbol);
    try std.testing.expectEqual(TokenType.IDENT, tok.tokenType);

    // Test that the scanner did not move forward.
    tok = s.peek();
    try std.testing.expectEqualStrings("asdf", tok.symbol);
    try std.testing.expectEqual(TokenType.IDENT, tok.tokenType);

    _ = s.next();
    tok = s.peek();
    try std.testing.expectEqualStrings("123", tok.symbol);
    try std.testing.expectEqual(TokenType.C_INT, tok.tokenType);

    // Test that the scanner did not move forward.
    tok = s.peek();
    try std.testing.expectEqualStrings("123", tok.symbol);
    try std.testing.expectEqual(TokenType.C_INT, tok.tokenType);
}
