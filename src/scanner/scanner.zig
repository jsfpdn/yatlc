const std = @import("std");

const token = @import("token.zig");
const reporter = @import("../reporter/reporter.zig");

pub const Token = token.Token;
pub const TokenType = token.TokenType;

pub const Scanner = struct {
    contents: []const u8, // Source code

    // scanning state
    offset: usize, // Describes the current location of scanner within the linear Scanner.contents
    charOffset: usize, // Together with Scanner.lineOffset, describes the current location of scanner
    lineOffset: usize, // Together with Scanner.charOffset, describes the current location of scanner

    // reporter is used for reporting error messages.
    reporter: ?reporter.Reporter,
    // errorMessage describes currently encountered error.
    errorMessage: []const u8,

    // tokenWriter is used for writing tokens.
    tokenWriter: ?std.fs.File.Writer,
    // lastToken is used for proper line breaking when writing tokens to the writer.
    lastToken: ?Token,

    /// Init prepares the scanner for lexical analysis.
    pub fn init(contents: []const u8, r: ?reporter.Reporter, tokenWriter: ?std.fs.File.Writer) Scanner {
        return .{
            .contents = contents,
            .reporter = r,

            .offset = 0,
            .charOffset = 1,
            .lineOffset = 1,
            .errorMessage = "",

            .tokenWriter = tokenWriter,
            .lastToken = null,
        };
    }

    /// Next reads the source code and returns the next lexem.
    ///
    /// If any error was encountered, it is reported to the reporter.
    ///
    /// If tokenWriter was supplied during the initialization, the analyzed token
    /// is formatted and written to it.
    pub fn next(self: *Scanner) token.Token {
        self.eatWhitespace();

        var tok = Token{
            .tokenType = TokenType.ILLEGAL,
            .bufferLoc = Token.BufferLoc{ .start = self.offset, .end = self.offset },
            .sourceLoc = Token.SourceLoc{ .line = self.lineOffset, .column = self.charOffset },
            .symbol = "",
        };

        defer self.emitToken(tok);

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
            '0'...'9' => self.parseNumber(&tok, c),
            '.' => {
                // Either a single period or a floating point number with just a decimal part.
                tok.tokenType = TokenType.PERIOD;

                if (self.peek()) |p| {
                    if (isNumeric(p)) {
                        tok.tokenType = TokenType.FLOAT;
                        self.advance();
                        self.parseInteger(&tok, 10);
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
            else => self.errorMessage = "Unrecognized token",
        }

        tok.symbol = self.symbol(tok);
        if (tok.tokenType == TokenType.ILLEGAL) {
            if (self.reporter) |r| {
                r.report(tok, self.errorMessage);
                self.errorMessage = "";
            }
        }

        return tok;
    }

    /// eof denotes whether end-of-file has been reached.
    pub fn eof(self: *Scanner) bool {
        return self.offset >= self.contents.len;
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
                self.errorMessage = "Identifier cannot be composed entirely from underscores";
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

    fn parseNumber(self: *Scanner, tok: *Token, leading: u8) void {
        tok.tokenType = TokenType.INT;

        var base = @as(usize, 10);
        if (leading == '0') {
            // Check for '0x', '0o' and '0b' prefixes.
            const pn = self.peek() catch unreachable;
            switch (pn) {
                'x' => base = 16,
                'o' => base = 8,
                'b' => base = 2,
                else => {},
            }

            if (base != 10) {
                // Step over 'x', 'o', or 'b' prefix.
                self.advance();
            }
        }

        self.parseInteger(tok, base);

        const p = self.peek() catch return;
        if (p != '.') {
            return;
        }
        tok.tokenType = TokenType.FLOAT;
        self.advance();
        self.parseInteger(tok, base);
    }

    fn parseInteger(self: *Scanner, tok: *Token, base: usize) void {
        _ = self.peek() catch |err| switch (err) {
            error.EOF => {
                self.errorMessage = "number literal must not be empty";
                return;
            },
        };

        switch (base) {
            16 => self.parseHexadecimal(tok),
            10 => self.parseDecimal(tok),
            8 => self.parseOctal(tok),
            2 => self.parseBinary(tok),
            else => @panic("number must be in base 16, 10, 8, or 2!"),
        }
    }

    fn parseDecimal(self: *Scanner, tok: *Token) void {
        while (!self.eof()) {
            const d = self.peek() catch break;
            switch (d) {
                '0'...'9' => self.advance(),
                else => break,
            }
        }

        tok.bufferLoc.end = self.offset - 1;
    }

    fn parseHexadecimal(self: *Scanner, tok: *Token) void {
        while (!self.eof()) {
            const d = self.peek() catch break;
            switch (d) {
                '0'...'9', 'a'...'f', 'A'...'F' => self.advance(),
                else => break,
            }
        }

        tok.bufferLoc.end = self.offset - 1;
    }

    fn parseOctal(self: *Scanner, tok: *Token) void {
        while (!self.eof()) {
            const d = self.peek() catch break;
            switch (d) {
                '0'...'7' => self.advance(),
                else => break,
            }
        }
        tok.bufferLoc.end = self.offset - 1;
    }

    fn parseBinary(self: *Scanner, tok: *Token) void {
        while (!self.eof()) {
            const d = self.peek() catch break;
            switch (d) {
                '0', '1' => self.advance(),
                else => break,
            }
        }

        tok.bufferLoc.end = self.offset - 1;
    }

    fn parseBuiltin(self: *Scanner, tok: *Token) void {
        self.parseIdent(tok);
        const s = self.contents[tok.bufferLoc.start .. tok.bufferLoc.end + 1];
        if (token.TokenType.getBuiltin(s)) |builtin| {
            tok.tokenType = builtin;
            return;
        }
        tok.tokenType = TokenType.ILLEGAL;
        self.errorMessage = "Builtin function does not exist";
    }

    fn parseStringLiteral(self: *Scanner, tok: *Token) void {
        tok.tokenType = TokenType.STRING;
        var escaped = false;

        while (true) {
            if (self.eof()) {
                tok.bufferLoc.end = self.offset - 1;
                tok.tokenType = TokenType.ILLEGAL;
                self.errorMessage = "String literal is not closed with quotes";
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

        while (!self.eof()) {
            p = self.peek() catch unreachable;

            if (p == '\n') {
                break;
            }

            self.advance();
        }

        tok.tokenType = TokenType.COMMENT;
        tok.bufferLoc.end = self.offset - 1;
        self.advance();
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
                self.errorMessage = "Multiline comment not closed with '*/'";
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

    /// Return the actual symbol (lexeme) of the particular token.
    fn symbol(self: Scanner, tok: Token) []const u8 {
        switch (tok.tokenType) {
            TokenType.EOF => return "EOF",
            else => {},
        }

        return self.contents[tok.bufferLoc.start .. tok.bufferLoc.end + 1];
    }

    /// Emit the supplied token to a writer if the writer was provided.
    fn emitToken(self: *Scanner, tok: Token) void {
        if (self.tokenWriter) |writer| {
            if (self.lastToken) |ltok| {
                if (ltok.sourceLoc.line < tok.sourceLoc.line) {
                    wrappedPrint(writer, "\n", .{});
                }
            }

            wrappedPrint(writer, "({s}: '{s}') ", .{ @tagName(tok.tokenType), tok.symbol });
            if (tok.tokenType == TokenType.EOF) {
                wrappedPrint(writer, "\n", .{});
            }

            self.lastToken = tok;
        }
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

/// wrappedPrint is a convenience function to panic when encountering error during printing via writer.
fn wrappedPrint(writer: std.fs.File.Writer, comptime format: []const u8, args: anytype) void {
    writer.print(format, args) catch |err| {
        std.debug.print("compiler error: could not write token to a file: {s}\n", .{@errorName(err)});
        @panic("compiler error: could not write token to a file!");
    };
}
