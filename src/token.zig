const std = @import("std");
const expect = std.testing.expect;

pub const BufferLoc = struct {
    start: usize,
    end: usize,
};

pub const Token = struct {
    tokenType: TokenType,
    bufferLoc: BufferLoc,
    sourceLoc: SourceLoc,
    symbol: []const u8,

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
    C_NULL, // null
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
    B_AND, // &
    B_OR, // |
    B_XOR, // ^
    B_LSH, // <<
    B_RSH, // >>

    NOT, // not
    AND, // and
    OR, // or

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
    XOR_ASSIGN, // ^=

    LSH_ASSIGN, // <<=
    RSH_ASSIGN, // >>=
    LAND_ASSIGN, // &&=
    LOR_ASSIGN, // ||=

    // Logical operators
    LAND, // &&
    LOR, // ||
    INC, // ++
    DEC, // --

    // Relational operators
    EQL, // ==
    LT, // <
    GT, // >
    NEQ, // !=
    LEQ, // <=
    GEQ, // >=

    NEG, // !

    LPAREN, // (
    LBRACK, // [
    LBRACE, // {
    COMMA, // ,

    RPAREN, // )
    RBRACK, // ]
    RBRACE, // }
    SEMICOLON, // ;
    COLON, // :
    QUESTION_MARK, // ?
    D_QUESTION_MARK, // ??

    // keywords
    BREAK,
    CONTINUE,
    ELSE,
    IF,
    RETURN,
    WHILE,
    DO,
    FOR,

    // special "@" symbol for type conversions and built-in functions
    AT, // @

    // special "#" symbol for address-of operator
    HASH, // #

    // names of built-in functions that must be preceeded with "@"
    ALLOC, // alloc
    MALLOC, // malloc
    REALLOC, // realloc
    FREE, // free
    SIZEOF, // sizeof
    LEN, // len
    PRINT, // print
    READLN, // readln
    BITCAST, // bitcast
    EXIT, // exit

    const keywords = std.ComptimeStringMap(TokenType, .{
        .{ "break", .BREAK },
        .{ "continue", .CONTINUE },
        .{ "else", .ELSE },
        .{ "if", .IF },
        .{ "return", .RETURN },
        .{ "while", .WHILE },
        .{ "do", .DO },
        .{ "for", .FOR },
    });

    const builtins = std.ComptimeStringMap(TokenType, .{
        .{ "alloc", .ALLOC },
        .{ "malloc", .MALLOC },
        .{ "realloc", .REALLOC },
        .{ "free", .FREE },
        .{ "sizeof", .SIZEOF },
        .{ "len", .LEN },
        .{ "print", .PRINT },
        .{ "readln", .READLN },
        .{ "exit", .EXIT },
    });

    pub fn getKeyword(bytes: []const u8) ?TokenType {
        return keywords.get(bytes);
    }

    pub fn getBuiltin(bytes: []const u8) ?TokenType {
        return builtins.get(bytes);
    }

    pub fn isBuiltin(token: TokenType) bool {
        return @intFromEnum(token) >= @intFromEnum(TokenType.ALLOC) and @intFromEnum(token) <= @intFromEnum(TokenType.EXIT);
    }

    // Assignments are =, +=, -=, *=, /=, %=, &=, |=, ^=, <<=, >>=, &&= and ||=.
    pub fn isAssignment(token: TokenType) bool {
        return @intFromEnum(token) >= @intFromEnum(TokenType.ASSIGN) and @intFromEnum(token) <= @intFromEnum(TokenType.LOR_ASSIGN);
    }

    // Relational operators are ==, >, <, >=, <= and !=.
    pub fn isRelational(token: TokenType) bool {
        return @intFromEnum(token) >= @intFromEnum(TokenType.EQL) and @intFromEnum(token) <= @intFromEnum(TokenType.GEQ);
    }

    // Bitwise operators are &, |, ^, << and >>.
    pub fn isBitwise(token: TokenType) bool {
        return @intFromEnum(token) >= @intFromEnum(TokenType.B_AND) and @intFromEnum(token) <= @intFromEnum(TokenType.B_RSH);
    }

    // Lower priority arithmetic operators are all the bitwise operators and + and -.
    pub fn isLowerPrioArithmetic(token: TokenType) bool {
        return isBitwise(token) or token == TokenType.ADD or token == TokenType.SUB;
    }

    // Higher priority arithmetic operators are *, / and %.
    pub fn isHigherPrioArithmetic(token: TokenType) bool {
        return @intFromEnum(token) >= @intFromEnum(TokenType.MUL) and @intFromEnum(token) <= @intFromEnum(TokenType.REM);
    }

    pub const TokenNameTable = [@typeInfo(TokenType).Enum.fields.len][:0]const u8{
        "ILLEGAL",
        "EOF",
        "COMMENT",

        "IDENT",
        "NULL",
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

        "not",
        "and",
        "or",

        "=",

        "+=",
        "-=",
        "*=",
        "/=",
        "%=",

        "&=",
        "|=",
        "^=",

        "<<=",
        ">>=",
        "&&=",
        "||=",

        "&&",
        "||",
        "++",
        "--",

        "==",
        "<",
        ">",
        "!=",
        "<=",
        ">=",

        "!",

        "(",
        "[",
        "{",
        ",",

        ")",
        "]",
        "}",
        ";",
        ":",
        "?",
        "??",

        "break",
        "continue",
        "else",
        "if",
        "return",
        "while",
        "do",
        "for",

        "@",
        "#",

        "alloc",
        "malloc",
        "realloc",
        "free",
        "sizeof",
        "len",
        "print",
        "readln",
        "bitcast",
        "exit",
    };

    pub fn str(self: TokenType) [:0]const u8 {
        return TokenNameTable[@intFromEnum(self)];
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
