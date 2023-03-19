const std = @import("std");

const token = @import("token.zig");
const Token = token.Token;
const TokenType = token.TokenType;

const logger = @import("../logger/logger.zig");

pub fn createScanner(contents: []const u8, log: logger.Logger) Scanner {
    return Scanner{ .contents = contents, .log = log, .offset = 0, .charOffset = 1, .lineOffset = 1 };
}

pub const Scanner = struct {
    contents: []const u8, // Source code

    // scanning state
    offset: usize, // Describes the current location of scanner within the linear Scanner.contents
    charOffset: usize, // Together with Scanner.lineOffset, describes the current location of scanner
    lineOffset: usize, // Together with Scanner.charOffset, describes the current location of scanner

    log: logger.Logger,

    // TODO(jsfpdn): inject an errorHandler/reporter.
    // TODO(jsfpdn): Think about error recovery during lexical analysis. It would be nice to report an
    //       error to a parser but still be able to continue.

    pub fn next(self: *Scanner) token.Token {
        self.eatWhitespace();

        var tok = Token{
            .tokenType = TokenType.ILLEGAL,
            .bufferLoc = Token.BufferLoc{ .start = self.offset, .end = self.offset },
            .sourceLoc = Token.SourceLoc{ .line = self.lineOffset, .column = self.charOffset },
        };

        if (self.eof()) {
            tok.tokenType = TokenType.EOF;
            return tok;
        }

        const c = self.consume() catch unreachable;
        switch (c) {
            '(' => tok.tokenType = TokenType.LPAREN,
            '[' => tok.tokenType = TokenType.LBRACK,
            '{' => tok.tokenType = TokenType.LBRACE,
            ',' => tok.tokenType = TokenType.COMMA,
            ')' => tok.tokenType = TokenType.RPAREN,
            ']' => tok.tokenType = TokenType.RBRACK,
            '}' => tok.tokenType = TokenType.RBRACE,
            ';' => tok.tokenType = TokenType.SEMICOLON,
            ':' => tok.tokenType = TokenType.COLON,
            '*' => self.switch2(&tok, '=', TokenType.MUL_ASSIGN, TokenType.MUL),
            '%' => self.switch2(&tok, '=', TokenType.REM_ASSIGN, TokenType.REM),
            '=' => self.switch2(&tok, '=', TokenType.EQL, TokenType.ASSIGN),
            '<' => self.switch2(&tok, '=', TokenType.LEQ, TokenType.LSS),
            '>' => self.switch2(&tok, '=', TokenType.GEQ, TokenType.GTR),
            '!' => self.switch2(&tok, '=', TokenType.NEQ, TokenType.NOT),
            '+' => self.switch3(&tok, '=', TokenType.ADD_ASSIGN, '+', TokenType.INC, TokenType.ADD),
            '-' => self.switch3(&tok, '=', TokenType.SUB_ASSIGN, '-', TokenType.DEC, TokenType.SUB),
            '&' => self.switch3(&tok, '=', TokenType.AND_ASSIGN, '&', TokenType.LAND, TokenType.AND),
            '|' => self.switch3(&tok, '=', TokenType.OR_ASSIGN, '|', TokenType.LOR, TokenType.OR),
            '@' => {
                // TODO(jsfpdn): Handle builtins.
            },
            '.' => {
                tok.tokenType = TokenType.PERIOD;

                const p = self.peek() catch return tok;
                if (isNumeric(p)) {
                    tok.tokenType = TokenType.FLOAT;
                    self.parseInteger(&tok);
                }

                return tok;
            },

            '/' => {
                // Special care must be taken if / or * follows due to the analysis of comments.
                const p = self.peek() catch return tok;

                if (p == '/' or p == '*') {
                    // TODO(jsfpdn): parse comments.
                } else {
                    self.switch2(&tok, '=', TokenType.QUO_ASSIGN, TokenType.QUO);
                }
            },
            'a'...'z', 'A'...'Z' => {
                tok.tokenType = TokenType.IDENT;
                self.parseIdent(&tok);
                if (token.TokenType.getKeyword(self.symbol(tok))) |keyword| {
                    tok.tokenType = keyword;
                }
            },
            '0'...'9' => {
                tok.tokenType = TokenType.INT;
                self.parseInteger(&tok);

                const p = self.peek() catch return tok;
                if (p != '.') {
                    return tok;
                }

                tok.tokenType = TokenType.FLOAT;
                // Consume ".".
                self.advance();
                self.parseInteger(&tok);
            },
            else => unreachable,
        }

        // TODO: error handling.
        return tok;
    }

    pub fn parseIdent(self: *Scanner, tok: *Token) void {
        // TODO(jsfpdn): fix identifiers with underscores.
        while (true and !self.eof()) {
            const p = self.peek() catch unreachable;
            if (!isAlphanumeric(p)) {
                tok.bufferLoc.end = self.offset - 1;
                return;
            }
            self.advance();
        }
    }

    pub fn parseInteger(self: *Scanner, tok: *Token) void {
        while (true and !self.eof()) {
            const d = self.peek() catch unreachable;
            if (!isNumeric(d)) {
                tok.bufferLoc.end = self.offset;
                return;
            }
            self.advance();
        }
    }

    fn advance(self: *Scanner) void {
        if (self.eof()) {
            return;
        }

        const p = self.peek() catch unreachable;
        if (p == '\n') {
            self.lineOffset += 1;
            self.charOffset = 1;
        }
        self.charOffset += 1;
        self.offset += 1;
    }

    fn switch2(self: *Scanner, tok: *Token, shouldFollow: u8, ifFollows: TokenType, elseFollows: TokenType) void {
        tok.tokenType = elseFollows;

        if (self.peek()) |follows| {
            if (follows == shouldFollow) {
                tok.tokenType = ifFollows;
                tok.bufferLoc.end = self.offset;

                self.advance();
            }
        } else |err| switch (err) {
            error.EOF => {},
        }
    }

    fn switch3(self: *Scanner, tok: *Token, ifA: u8, thenA: TokenType, elifB: u8, thenB: TokenType, elseFollows: TokenType) void {
        tok.tokenType = elseFollows;

        if (self.peek()) |follows| {
            if (follows == ifA) {
                tok.tokenType = thenA;
            } else if (follows == elifB) {
                tok.tokenType = thenB;
            } else {
                return;
            }
            tok.bufferLoc.end = self.offset;

            self.advance();
        } else |err| switch (err) {
            error.EOF => {},
        }
    }

    pub fn eatWhitespace(self: *Scanner) void {
        while (true and !self.eof()) {
            const c = self.peek() catch unreachable;
            switch (c) {
                ' ', '\t', '\n', '\r' => self.advance(),
                else => return,
            }
        }
    }

    pub fn eof(self: *Scanner) bool {
        return self.offset >= self.contents.len;
    }

    fn peek(self: *Scanner) !u8 {
        if (self.offset >= self.contents.len) return error.EOF;

        return self.contents[self.offset];
    }

    fn consume(self: *Scanner) !u8 {
        if (self.eof()) return error.EOF;

        const c = self.contents[self.offset];
        self.advance();
        return c;
    }

    // Return the actual symbol (lexeme) of the particular token.
    pub fn symbol(self: Scanner, tok: Token) []const u8 {
        return self.contents[tok.bufferLoc.start .. tok.bufferLoc.end + 1];
    }
};

fn isAlphanumeric(c: u8) bool {
    switch (c) {
        'A'...'z', '0'...'9' => return true,
        else => return false,
    }
}

fn isNumeric(c: u8) bool {
    return c >= '0' and c <= '9';
}
