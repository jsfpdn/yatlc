const std = @import("std");

const token = @import("token.zig");
const Token = token.Token;
const TokenType = token.TokenType;

pub fn createScanner(contents: []const u8, errReporter: *const fn (Token, []const u8) void) Scanner {
    return Scanner{ .contents = contents, .errReporter = errReporter, .offset = 0, .charOffset = 1, .lineOffset = 1 };
}

pub fn defaultReporter(tok: Token, msg: []const u8) void {
    std.debug.print("error: {s}: {s}\n", .{ @tagName(tok.tokenType), msg });
}

pub const Scanner = struct {
    contents: []const u8, // Source code

    // scanning state
    offset: usize, // Describes the current location of scanner within the linear Scanner.contents
    charOffset: usize, // Together with Scanner.lineOffset, describes the current location of scanner
    lineOffset: usize, // Together with Scanner.charOffset, describes the current location of scanner

    errReporter: *const fn (Token, []const u8) void,

    // TODO(jsfpdn): inject an errorHandler/reporter.
    // TODO(jsfpdn): Think about error recovery during lexical analysis. It would be nice to report an
    //       error to a parser but still be able to continue.

    pub fn next(self: *Scanner) token.Token {
        self.eatWhitespace();

        var tok = Token{
            .tokenType = TokenType.ILLEGAL,
            .bufferLoc = Token.BufferLoc{ .start = self.offset, .end = self.offset },
            .sourceLoc = Token.SourceLoc{ .line = self.lineOffset, .column = self.charOffset },
            .symbol = "",
        };

        if (self.eof()) {
            tok.tokenType = TokenType.EOF;
            tok.symbol = "EOF";
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
            '"' => self.parseStringLiteral(&tok),
            '@' => self.parseBuiltin(&tok),
            'a'...'z', 'A'...'Z', '_' => self.parseIdentOrKeyword(&tok),
            '0'...'9' => self.parseNumber(&tok),
            '.' => {
                // Either a single period or a floating point number with just a decimal part.
                tok.tokenType = TokenType.PERIOD;

                if (self.peek()) |p| {
                    if (isNumeric(p)) {
                        self.parseDecimalPart(&tok);
                    }
                } else |_| {}
            },
            '/' => {
                // Special care must be taken if / or * follows due to the analysis of comments.
                const p = self.peek() catch return tok;
                switch (p) {
                    '/' => self.parseSinglelineComment(&tok),
                    '*' => self.parseMultilineComment(&tok),
                    else => self.switch2(&tok, '=', TokenType.QUO_ASSIGN, TokenType.QUO),
                }
            },
            else => unreachable,
        }

        tok.symbol = self.symbol(tok);
        return tok;
    }

    fn parseIdentOrKeyword(self: *Scanner, tok: *Token) void {
        tok.tokenType = TokenType.IDENT;
        self.parseIdent(tok);
        if (token.TokenType.getKeyword(self.symbol(tok.*))) |keyword| {
            tok.tokenType = keyword;
        } else {
            var otherThanUnderscore = false;
            for (self.symbol(tok.*)) |char| {
                if (char != '_') {
                    otherThanUnderscore = true;
                    break;
                }
            }

            if (!otherThanUnderscore) {
                tok.tokenType = TokenType.ILLEGAL;
                // TODO(jsfpdn): error reporting: identifier must not be composed entirely from underscores.
            }
        }
    }

    fn parseIdent(self: *Scanner, tok: *Token) void {
        while (true) {
            if (self.eof()) {
                tok.bufferLoc.end = self.offset - 1;
                return;
            }
            const p = self.peek() catch unreachable;
            if (!isAlphanumeric(p) and p != '_') {
                tok.bufferLoc.end = self.offset - 1;
                return;
            }
            self.advance();
        }
    }

    fn parseNumber(self: *Scanner, tok: *Token) void {
        tok.tokenType = TokenType.INT;
        self.parseInteger(tok);

        const p = self.peek() catch return;
        if (p != '.') {
            return;
        }
        self.parseDecimalPart(tok);
    }

    fn parseDecimalPart(self: *Scanner, tok: *Token) void {
        tok.tokenType = TokenType.FLOAT;
        // Consume ".".
        self.advance();
        self.parseInteger(tok);
    }

    fn parseInteger(self: *Scanner, tok: *Token) void {
        // TODO(jsfpdn): Implement binary/octal/hexadecimal number parsing.
        while (true and !self.eof()) {
            const d = self.peek() catch unreachable;
            if (!isNumeric(d)) {
                tok.bufferLoc.end = self.offset - 1;
                return;
            }
            self.advance();
        }
    }

    fn parseBuiltin(self: *Scanner, tok: *Token) void {
        self.parseIdent(tok);
        const s = self.contents[tok.bufferLoc.start .. tok.bufferLoc.end + 1];
        if (token.TokenType.getBuiltin(s)) |builtin| {
            tok.tokenType = builtin;
            return;
        }
        // TODO(jsfpdn): Handle error reporting when such builtin does not exist.
        //               Parser could recover from this error - just skip this token.
    }

    fn parseStringLiteral(self: *Scanner, tok: *Token) void {
        tok.tokenType = TokenType.STRING;
        var escaped = false;

        while (true) {
            if (self.eof()) {
                tok.bufferLoc.end = self.offset - 1;
                tok.tokenType = TokenType.ILLEGAL;
                // TODO(jsfpdn): error reporting: unclosed string literal.
                return;
            }

            const p = self.peek() catch unreachable;

            self.advance();
            if (p == '"' and !escaped) {
                tok.bufferLoc.end = self.offset - 1;
                return;
            }

            escaped = p == '\\';
        }
    }

    fn parseSinglelineComment(self: *Scanner, tok: *Token) void {
        var p = self.peek() catch unreachable;
        if (p != '/') unreachable;
        // Conusme the '/'.
        self.advance();

        while (true) {
            // TODO(jsfpdn): refactor me!
            if (self.eof()) {
                tok.tokenType = TokenType.COMMENT;
                tok.bufferLoc.end = self.offset - 1;
                return;
            }

            p = self.peek() catch unreachable;

            if (p == '\n') {
                tok.tokenType = TokenType.COMMENT;
                tok.bufferLoc.end = self.offset - 1;

                self.advance();
                return;
            }

            self.advance();
        }
    }

    fn parseMultilineComment(self: *Scanner, tok: *Token) void {
        var toBeClosed: u32 = 1;
        while (true) {
            self.advance();
            if (toBeClosed == 0) {
                tok.tokenType = TokenType.COMMENT;
                if (self.eof()) {
                    tok.bufferLoc.end = self.offset - 1;
                } else {
                    tok.bufferLoc.end = self.offset - 2;
                }
                return;
            }

            if (self.eof()) {
                // TODO(jsfpdn): error handling - report message 'multiline comment not closed'
                tok.tokenType = TokenType.ILLEGAL;
                tok.bufferLoc.end = self.offset - 1;
                return;
            }

            if (self.followsCommentOpening()) {
                toBeClosed += 1;
            } else if (self.followsCommentClosing()) {
                toBeClosed -= 1;
            }
        }
    }

    fn followsCommentOpening(self: *Scanner) bool {
        const p = self.peek() catch return false;
        const pn = self.peekNext() catch return false;

        if ((p == '/') and (pn == '*')) {
            self.advance();
            self.advance();
            return true;
        }
        return false;
    }

    fn followsCommentClosing(self: *Scanner) bool {
        const p = self.peek() catch return false;
        const pn = self.peekNext() catch return false;

        if (p == '*' and pn == '/') {
            self.advance();
            self.advance();
            return true;
        }
        return false;
    }

    fn advance(self: *Scanner) void {
        if (self.eof()) {
            return;
        }

        const p = self.peek() catch unreachable;
        self.offset += 1;
        self.charOffset += 1;
        if (p == '\n') {
            self.lineOffset += 1;
            self.charOffset = 1;
        }
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

    fn eatWhitespace(self: *Scanner) void {
        while (true and !self.eof()) {
            const c = self.peek() catch unreachable;
            switch (c) {
                ' ', '\t', '\n', '\r' => self.advance(),
                else => return,
            }
        }
    }

    fn eof(self: *Scanner) bool {
        return self.offset >= self.contents.len;
    }

    fn peek(self: *Scanner) !u8 {
        if (self.offset >= self.contents.len) return error.EOF;

        return self.contents[self.offset];
    }

    fn peekNext(self: *Scanner) !u8 {
        if (self.offset + 1 >= self.contents.len) return error.EOF;

        return self.contents[self.offset + 1];
    }

    fn consume(self: *Scanner) !u8 {
        if (self.eof()) return error.EOF;

        const c = self.contents[self.offset];
        self.advance();
        return c;
    }

    // Return the actual symbol (lexeme) of the particular token.
    fn symbol(self: Scanner, tok: Token) []const u8 {
        switch (tok.tokenType) {
            TokenType.EOF => return "EOF",
            else => {},
        }

        return self.contents[tok.bufferLoc.start .. tok.bufferLoc.end + 1];
    }
};

fn isAlphanumeric(c: u8) bool {
    switch (c) {
        'a'...'z', 'A'...'Z', '0'...'9' => return true,
        else => return false,
    }
}

fn isNumeric(c: u8) bool {
    return c >= '0' and c <= '9';
}
