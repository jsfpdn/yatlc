const std = @import("std");
const expect = std.testing.expect;

const Token = enum {
    // Special tokens
    ILLEGAL,
    EOF,
    COMMENT,

    // Identifiers and basic type literals
    IDENT, // variable_name
    INT, // 42
    FLOAT, // 42.24
    CHAR, // 'a'

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

    // builtin functions
    BF_LEN, // @len
    BF_TOINT, // @toInt
    BF_TOFLOAT, // @toFloat
    // TODO: add builtin functions for printing and user input.

    pub const TokenNameTable = [@typeInfo(Token).Enum.fields.len][:0]const u8{
        "ILLEGAL",
        "EOF",
        "COMMENT",

        "IDENT",
        "INT",
        "FLOAT",
        "CHAR",

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

        "@len",
        "@toInt",
        "@toFloat",
    };

    pub fn str(self: Token) [:0]const u8 {
        return TokenNameTable[@enumToInt(self)];
    }
};

test "enforce correct size of TokenNameTable size" {
    try expect(Token.TokenNameTable.len == @typeInfo(Token).Enum.fields.len);
}

test "enum to string representation mapping" {
    // Test few of the mappings from enum to its textual representation:
    try expect(std.mem.eql(u8, Token.INC.str(), "++"));
    try expect(std.mem.eql(u8, Token.BREAK.str(), "break"));
    try expect(std.mem.eql(u8, Token.EQL.str(), "=="));
    try expect(std.mem.eql(u8, Token.LPAREN.str(), "("));
    try expect(std.mem.eql(u8, Token.BF_LEN.str(), "@len"));
}
