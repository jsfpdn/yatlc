const std = @import("std");
const expect = std.testing.expect;

pub const Token = struct {
    tokenType: TokenType,
    bufferLoc: BufferLoc,
    sourceLoc: SourceLoc,

    pub const BufferLoc = struct {
        start: usize,
        end: usize,
    };

    pub const SourceLoc = struct {
        line: usize,
        column: usize,
    };

    pub fn len(self: *Token) usize {
        return self.bufferLoc.end - self.bufferLoc.start;
    }
};

pub const TokenType = enum {
    // Special tokens
    ILLEGAL,
    EOF,
    COMMENT,

    // Identifiers and basic type literals
    IDENT, // variable_name
    INT, // 42
    FLOAT, // 42.24
    CHAR, // 'a'
    STRING, // "string"

    // Arithmetical operators
    ADD, // +
    SUB, // -
    MUL, // *
    QUO, // /
    REM, // %

    // Bitwise operators
    AND, // &
    OR, // |

    // Asignment operator
    ASSIGN, // =

    // Arithmetical assignment operators
    ADD_ASSIGN, // +=
    SUB_ASSIGN, // -=
    MUL_ASSIGN, // *=
    QUO_ASSIGN, // /=
    REM_ASSIGN, // %=

    // Bitwise assignment operators
    AND_ASSIGN, // &=
    OR_ASSIGN, // |=

    // Logical operators
    LAND, // &&
    LOR, // ||
    INC, // ++
    DEC, // --

    // Relational operators
    EQL, // ==
    LSS, // <
    GTR, // >
    NOT, // !
    NEQ, // !=
    LEQ, // <=
    GEQ, // >=

    LPAREN, // (
    LBRACK, // [
    LBRACE, // {
    COMMA, // ,
    PERIOD, // .

    RPAREN, // )
    RBRACK, // ]
    RBRACE, // }
    SEMICOLON, // ;
    COLON, // :

    // keywords
    BREAK,
    CONTINUE,
    ELSE,
    GOTO,
    IF,
    RETURN,
    WHILE,

    // builtin functions
    BF_LEN, // @len
    BF_TOINT, // @toInt
    BF_TOFLOAT, // @toFloat
    // TODO: add builtin functions for printing and user input.

    const keywords = std.ComptimeStringMap(TokenType, .{
        .{ "break", .BREAK },
        .{ "continue", .CONTINUE },
        .{ "else", .ELSE },
        .{ "goto", .GOTO },
        .{ "if", .IF },
        .{ "return", .RETURN },
        .{ "while", .WHILE },
    });

    pub fn getKeyword(bytes: []const u8) ?TokenType {
        return keywords.get(bytes);
    }

    pub const TokenNameTable = [@typeInfo(TokenType).Enum.fields.len][:0]const u8{
        "ILLEGAL",
        "EOF",
        "COMMENT",

        "IDENT",
        "INT",
        "FLOAT",
        "CHAR",
        "STRING",

        "+",
        "-",
        "*",
        "/",
        "%",

        "&",
        "|",

        "=",

        "+=",
        "-=",
        "*=",
        "/=",
        "%=",

        "&=",
        "|=",

        "&&",
        "||",
        "++",
        "--",

        "==",
        "<",
        ">",
        "!",
        "!=",
        "<=",
        ">=",

        "(",
        "[",
        "{",
        ",",
        ".",

        ")",
        "]",
        "}",
        ";",
        ":",

        "break",
        "continue",
        "else",
        "goto",
        "if",
        "return",
        "while",

        "@len",
        "@toInt",
        "@toFloat",
    };

    pub fn str(self: TokenType) [:0]const u8 {
        return TokenNameTable[@enumToInt(self)];
    }
};

test "enforce correct size of TokenNameTable size" {
    try expect(TokenType.TokenNameTable.len == @typeInfo(TokenType).Enum.fields.len);
}

test "enum to string representation mapping" {
    // Test few of the mappings from enum to its textual representation:
    try expect(std.mem.eql(u8, TokenType.INC.str(), "++"));
    try expect(std.mem.eql(u8, TokenType.BREAK.str(), "break"));
    try expect(std.mem.eql(u8, TokenType.EQL.str(), "=="));
    try expect(std.mem.eql(u8, TokenType.LPAREN.str(), "("));
    try expect(std.mem.eql(u8, TokenType.BF_LEN.str(), "@len"));
}
