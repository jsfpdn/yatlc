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

    // types
    T_I32, // i32
    T_I16, // i16
    T_U32, // u32
    T_U16, // u16
    T_CHAR, // char
    T_STRING, // str
    T_BOOL, // bool
    T_VOID, // void
    T_FLOAT, // float

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

    const types = std.ComptimeStringMap(TokenType, .{
        .{ "i32", .T_I32 },
        .{ "i16", .T_I16 },
        .{ "u32", .T_U32 },
        .{ "u16", .T_U16 },
        .{ "char", .T_CHAR },
        .{ "str", .T_STRING },
        .{ "bool", .T_BOOL },
        .{ "void", .T_VOID },
        .{ "float", .T_FLOAT },
    });

    pub fn getKeyword(bytes: []const u8) ?TokenType {
        return keywords.get(bytes);
    }

    pub fn getBuiltin(bytes: []const u8) ?TokenType {
        return builtins.get(bytes);
    }

    pub fn getType(bytes: []const u8) ?TokenType {
        return types.get(bytes);
    }

    pub fn isBuiltin(token: TokenType) bool {
        return @enumToInt(token) >= @enumToInt(TokenType.LEN) and @enumToInt(token) <= @enumToInt(TokenType.READ);
    }

    pub fn isType(token: TokenType) bool {
        return @enumToInt(token) >= @enumToInt(TokenType.T_I32) and @enumToInt(token) <= @enumToInt(TokenType.T_FLOAT);
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

        "i32",
        "i16",
        "u32",
        "u16",
        "char",
        "string",
        "bool",
        "void",
        "float",
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

test "types are correctly classified" {
    try expect(TokenType.isType(TokenType.T_I32));
    try expect(TokenType.isType(TokenType.T_I16));
    try expect(TokenType.isType(TokenType.T_U32));
    try expect(TokenType.isType(TokenType.T_U32));
    try expect(TokenType.isType(TokenType.T_STRING));
    try expect(TokenType.isType(TokenType.T_CHAR));
    try expect(TokenType.isType(TokenType.T_FLOAT));
    try expect(TokenType.isType(TokenType.T_BOOL));
}
