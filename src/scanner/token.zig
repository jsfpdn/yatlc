const std = @import("std");
const expect = std.testing.expect;

pub const Token = struct {
    tokenType: TokenType,
    bufferLoc: BufferLoc,
    sourceLoc: SourceLoc,
    symbol: []const u8,

    pub const BufferLoc = struct {
        start: usize,
        end: usize,
    };

    pub const SourceLoc = struct {
        line: usize,
        column: usize,
    };

    pub fn len(self: Token) usize {
        return self.bufferLoc.end - self.bufferLoc.start;
    }

    pub fn str(self: Token) []const u8 {
        return self.symbol;
    }
};

pub const TokenType = enum(u8) {
    // Special tokens
    ILLEGAL,
    EOF,
    COMMENT,

    // Identifiers
    IDENT, // variable_name

    // Constants
    C_INT, // 420
    C_FLOAT, // 3.14
    C_CHAR, // 'a'
    C_STRING, // "asd"
    C_BOOL, // true or false

    // Arithmetical operators
    ADD, // +
    SUB, // -
    MUL, // *
    QUO, // /
    REM, // %

    // Bitwise operators
    AND, // &
    OR, // |
    XOR, // ^
    LSH, // <<
    RSH, // >>

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
    XOR_ASSIGN, // ^

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
    QST, // ?

    // keywords
    BREAK,
    CONTINUE,
    ELSE,
    IF,
    RETURN,
    WHILE,
    FOR,

    // special "@" symbol for type conversions and built-in functions
    AT, // @

    // names of built-in functions that must be preceeded with "@"
    LEN, // len
    PRINT, // print
    READ, // read

    const keywords = std.ComptimeStringMap(TokenType, .{
        .{ "break", .BREAK },
        .{ "continue", .CONTINUE },
        .{ "else", .ELSE },
        .{ "if", .IF },
        .{ "return", .RETURN },
        .{ "while", .WHILE },
    });

    const builtins = std.ComptimeStringMap(TokenType, .{
        .{ "len", .LEN },
        .{ "print", .PRINT },
        .{ "read", .READ },
    });

    pub fn getKeyword(bytes: []const u8) ?TokenType {
        return keywords.get(bytes);
    }

    pub fn getBuiltin(bytes: []const u8) ?TokenType {
        return builtins.get(bytes);
    }

    pub fn isBuiltin(token: TokenType) bool {
        return @enumToInt(token) >= @enumToInt(TokenType.LEN) and @enumToInt(token) <= @enumToInt(TokenType.READ);
    }

    pub fn isAssignment(token: TokenType) bool {
        return @enumToInt(token) >= @enumToInt(TokenType.ASSIGN) and @enumToInt(token) <= @enumToInt(TokenType.XOR_ASSIGN);
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
        "BOOL",

        "+",
        "-",
        "*",
        "/",
        "%",

        "&",
        "|",
        "^",
        "<<",
        ">>",

        "=",

        "+=",
        "-=",
        "*=",
        "/=",
        "%=",

        "&=",
        "|=",
        "^=",

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
        "?",

        "break",
        "continue",
        "else",
        "if",
        "return",
        "while",
        "for",

        "@",

        "len",
        "print",
        "read",
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
    try expect(std.mem.eql(u8, TokenType.LEN.str(), "len"));
}
