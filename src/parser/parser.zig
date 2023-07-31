const std = @import("std");

const types = @import("types.zig");
const scanner = @import("../scanner/scanner.zig");
const token = @import("../scanner/token.zig");
const reporter = @import("../reporter/reporter.zig");

pub const Arg = struct {
    ident: token.Token,
    identType: token.TokenType,
};

pub const ParseError = error{
    UnexpectedToken,
    TypeError,
};

pub const Expression = struct {
    // expType denotes the type of the result of this expression.
    // token.TokenType.isType(this) must always evaluate to true.
    // TODO: refactor types to standalone module?
    expType: types.Type,

    // callsFunction denotes whether there is a function call in the expression somewhere.
    // This is done to restrict function calls in the top-level statements.
    callsFunction: bool = false,
};

pub const Parser = struct {
    scanner: scanner.Scanner,
    rep: ?reporter.Reporter,

    alloc: std.mem.Allocator,

    // failedAt contains the token where the parsing failed.
    failedAt: ?token.Token,
    // errorMsg contains additional information about the encountered error.
    errorMsg: ?[]const u8,

    // TODO:
    // * IR emitter
    // * analysis: main is defined (with correct arguments)
    // * unified error reporting
    // * should we build explicit trees when parsing expressions?
    // * zero-initialized variables?

    pub fn init(s: scanner.Scanner, r: ?reporter.Reporter, alloc: std.mem.Allocator) Parser {
        return .{
            .scanner = s,
            .rep = r,
            .alloc = alloc,
            .failedAt = null,
            .errorMsg = null,
        };
    }

    pub fn parse(self: *Parser) void {
        while (self.scanner.peek().tokenType != token.TokenType.EOF) {
            self.parseTopLevelStatement() catch {
                std.log.err("{d}:{d}: {s}", .{ self.failedAt.?.sourceLoc.line, self.failedAt.?.sourceLoc.column, self.errorMsg.? });
                self.alloc.free(self.errorMsg.?);
            };
        }

        // TODO: check for main etc.
    }

    fn parseTopLevelStatement(self: *Parser) !void {
        // 1) global variable definition: <type> <ident> = <expr>;
        // 2) (forward) function declaration: <type> <ident>(<arglist>);
        // 3) function definition: <func_decl> <body>

        // TODO: parsing the type like this may result in incomprehensible error message talking about IDENT instead of type.
        const identType = try self.consumeGet(token.TokenType.IDENT);
        _ = types.SimpleType.getType(identType.symbol) orelse {
            self.failedAt = identType;
            // Only way the std.fmt.allocPrint can fail is with `OutOfMemory` where it makes sense for us to fail.
            self.errorMsg = std.fmt.allocPrint(self.alloc, "expected a type but found '{s}' instead", .{identType.str()}) catch unreachable;
            return ParseError.UnexpectedToken;
        };

        const ident = try self.consumeGet(token.TokenType.IDENT);
        _ = ident;

        var follows = self.scanner.peek();
        if (follows.tokenType == token.TokenType.LPAREN) {
            // `<type> <ident>(` means that we're parsing function declaration.
            const argList = self.parseArgList() catch unreachable;
            defer argList.deinit(); // TODO(jsfpdn): should not be deinited but inserted into a symbol table instead.

            follows = self.scanner.peek();
            if (follows.tokenType == token.TokenType.SEMICOLON) {
                // just a forward function declaration
                std.log.info("finished with top level function declaration", .{});
                try self.consume(token.TokenType.SEMICOLON);
            } else if (follows.tokenType == token.TokenType.LBRACE) {
                try self.parseBody();
                std.log.info("parsed function body", .{});
            } else {
                self.failedAt = follows;
                self.errorMsg = std.fmt.allocPrint(self.alloc, "expected ';' or function body but found {s} instead", .{@tagName(follows.tokenType)}) catch unreachable;
                _ = self.scanner.next();
                return ParseError.UnexpectedToken;
            }
        } else if (token.TokenType.isAssignment(follows.tokenType)) {
            // everything ok - got identifier
            // parse expression (watch out, cannot contain function calls)
            _ = try self.parseExpression();
            std.log.err("would parse expression", .{});
        } else {
            self.failedAt = follows;
            self.errorMsg = std.fmt.allocPrint(self.alloc, "expected either function or variable definition but found {s} instead", .{@tagName(follows.tokenType)}) catch unreachable;
            _ = self.scanner.next();
            return ParseError.UnexpectedToken;
        }
    }

    fn parseStatement(self: *Parser) ParseError!void {
        const tok = self.scanner.peek();
        try switch (tok.tokenType) {
            token.TokenType.IDENT => self.parseVariableAssignment(),
            token.TokenType.LBRACE => self.parseBody(),
            token.TokenType.IF => self.parseIfStatement(),
            token.TokenType.FOR => self.parseForStatement(),
            token.TokenType.WHILE => self.parseWhileStatement(),
            token.TokenType.RETURN => {},
            else => {
                if (types.SimpleType.getType(tok.symbol)) |t| {
                    _ = t;
                    return self.parseVariableDeclaration();
                }
                // TODO: return error & report it.
            },
        };
    }

    fn parseVariableDeclaration(self: *Parser) ParseError!void {
        // `i32 asd;`, `i32 asd = 123;`
        _ = self;
    }

    fn parseVariableAssignment(self: *Parser) ParseError!void {
        // `asd = return_i32();` should all work.
        // TODO: type checking

        const lhs = try self.consumeGet(token.TokenType.IDENT);
        _ = lhs;

        const assignOp = self.scanner.next();
        if (!token.TokenType.isAssignment(assignOp.tokenType)) {
            std.log.err("expected variable assignment, found {s}", .{@tagName(assignOp.tokenType)});
            return ParseError.UnexpectedToken;
        }

        const rhs = try self.parseExpression();
        _ = rhs;

        try self.consume(token.TokenType.SEMICOLON);
    }

    fn parseIfStatement(self: *Parser) ParseError!void {
        // if (<expr>) <body> | <stmnt>; else <body> | <stmnt>;
        // TODO: type checking
        try self.consume(scanner.TokenType.IF);
        try self.consume(scanner.TokenType.LPAREN);
        // TODO: should it be parseExpression or parseLogicExpressions?
        const cond = try self.parseExpression();
        _ = cond;

        try self.consume(scanner.TokenType.RPAREN);
        // TODO: now can follow either a body or single statement followed by semicolon.
        if (self.scanner.peek().tokenType != token.TokenType.LBRACE) {
            try self.parseStatement();
        } else {
            try self.parseBody();
        }

        try self.consume(scanner.TokenType.ELSE);

        if (self.scanner.peek().tokenType != token.TokenType.LBRACE) {
            try self.parseStatement();
        } else {
            try self.parseBody();
        }
    }

    fn parseForStatement(self: *Parser) ParseError!void {
        try self.consume(scanner.TokenType.FOR);
        try self.consume(scanner.TokenType.LPAREN);

        if (self.scanner.peek().tokenType != token.TokenType.COMMA) {
            // TODO: try both variable assignment and variable declaration?
            try self.parseVariableDeclaration();
        }

        try self.consume(scanner.TokenType.COMMA);

        if (self.scanner.peek().tokenType != token.TokenType.COMMA) {
            _ = try self.parseLogicExpressions();
        }

        try self.consume(scanner.TokenType.COMMA);

        if (self.scanner.peek().tokenType != token.TokenType.RPAREN) {
            try self.parseVariableAssignment();
        }

        try self.consume(scanner.TokenType.RPAREN);
        try self.parseBody();
    }

    fn parseWhileStatement(self: *Parser) void {
        _ = self;
    }

    fn parseArgList(self: *Parser) !std.ArrayList(Arg) {
        var tok = self.scanner.next();
        if (tok.tokenType != token.TokenType.LPAREN) {
            std.log.err("expected '(' but found {s}", .{@tagName(tok.tokenType)});
            return error.ExpectedLPAREN;
        }

        var args = std.ArrayList(Arg).init(self.alloc);
        var expectComma = false;

        while (true) {
            var nextTok = self.scanner.next();
            if (nextTok.tokenType == token.TokenType.RPAREN) {
                break;
            }

            if (expectComma) {
                if (nextTok.tokenType != token.TokenType.COMMA) {
                    std.log.err("expected ',' but got {s}", .{@tagName(nextTok.tokenType)});
                    return error.ExpectedComma;
                }

                // Jump to the next token.
                nextTok = self.scanner.next();
            }

            if (nextTok.tokenType != token.TokenType.IDENT) {
                std.log.err("expected IDENT but got {s}", .{@tagName(nextTok.tokenType)});
                return error.ExpectedIdentifier;
            }

            const identType = self.scanner.next();
            _ = types.SimpleType.getType(identType.symbol) orelse {
                std.log.err("expected type but got {s}", .{@tagName(identType.tokenType)});
                return error.ExpectedIdentifierType;
            };

            args.append(Arg{ .ident = nextTok, .identType = identType.tokenType }) catch unreachable;
            expectComma = true;
        }

        return args;
    }

    fn parseBody(self: *Parser) ParseError!void {
        try self.consume(token.TokenType.LBRACE);

        var tok = self.scanner.peek();
        while (tok.tokenType != token.TokenType.RBRACE and tok.tokenType != token.TokenType.EOF) {
            try self.parseStatement();
            tok = self.scanner.peek();
        }
    }

    // E^1
    pub fn parseExpression(self: *Parser) ParseError!Expression {
        return self.parseTernaryExpression();
        // TODO: handle assignments with operations.
    }

    // E^2
    fn parseTernaryExpression(self: *Parser) ParseError!Expression {
        // <b_expr> ? <expr> : <expr>;
        // exp is potentially a boolen condition for the ternary operator.
        var exp = try self.parseLogicExpressions();
        // TODO: type checking
        // * condition must be a boolean expression
        // * if and else branches must have the same type
        // * automatic type conversion if one branch is more specific than the other

        var tok = self.scanner.peek();
        if (tok.tokenType != token.TokenType.QST) {
            return exp;
        }

        var thenBranch = try self.parseExpression();
        try self.consume(token.TokenType.COLON);
        var elseBranch = try self.parseExpression();

        _ = thenBranch;
        _ = elseBranch;

        return exp;
    }

    // E^3, binary and, or, ||, &&
    fn parseLogicExpressions(self: *Parser) ParseError!Expression {
        var exp = try self.parseNot();

        var tok = self.scanner.peek();
        switch (tok.tokenType) {
            token.TokenType.OR, token.TokenType.AND, token.TokenType.LAND, token.TokenType.LOR => {},
            else => return exp,
        }

        // last_value:
        // konec:
        // dedeny_konec:
        while (true) {
            tok = self.scanner.peek();
            switch (tok.tokenType) {
                token.TokenType.OR => {},
                token.TokenType.AND => {},
                token.TokenType.LAND => {},
                token.TokenType.LOR => {},
                else => return exp,
            }

            _ = self.scanner.next();
            var exp2 = try self.parseNot();
            _ = exp2;
        }
    }

    // E^4, unary not
    fn parseNot(self: *Parser) ParseError!Expression {
        const tok = self.scanner.peek();
        if (tok.tokenType != token.TokenType.NOT) {
            return self.parseRelationalExpression();
        }

        // Skip the '!' and continue.
        // TODO: type checking
        _ = self.scanner.next();
        return self.parseNot();
    }

    // E^5, ==, !=, >, <, >=, =<
    fn parseRelationalExpression(self: *Parser) ParseError!Expression {
        // TODO: handle exp (typechecking)
        var exp = try self.parseArithmeticExpression();

        var tok = self.scanner.peek();
        switch (tok.tokenType) {
            token.TokenType.EQL, token.TokenType.NEQ, token.TokenType.LSS, token.TokenType.GTR, token.TokenType.LEQ, token.TokenType.GEQ => {},
            else => return exp,
        }

        while (true) {
            tok = self.scanner.peek();

            switch (tok.tokenType) {
                token.TokenType.EQL, token.TokenType.NEQ, token.TokenType.LSS, token.TokenType.GTR, token.TokenType.LEQ, token.TokenType.GEQ => {},
                else => return exp,
            }

            _ = self.scanner.next();
            var exp2 = try self.parseArithmeticExpression();
            _ = exp2;
        }
    }

    // E^6, +, -, >>, <<, &, ^, |
    fn parseArithmeticExpression(self: *Parser) ParseError!Expression {
        // TODO: handle exp (typechecking)
        var exp = try self.parseUnaryOperators();

        var tok = self.scanner.peek();
        switch (tok.tokenType) {
            token.TokenType.ADD, token.TokenType.SUB, token.TokenType.LSS, token.TokenType.GTR, token.TokenType.LEQ, token.TokenType.GEQ => {},
            else => return exp,
        }

        while (true) {
            tok = self.scanner.peek();

            switch (tok.tokenType) {
                token.TokenType.ADD, token.TokenType.SUB, token.TokenType.LSS, token.TokenType.GTR, token.TokenType.LEQ, token.TokenType.GEQ => {},
                else => return exp,
            }

            _ = self.scanner.next();
            var exp2 = try self.parseUnaryOperators();
            _ = exp2;
        }
    }

    // E^7, -, !
    fn parseUnaryOperators(self: *Parser) ParseError!Expression {
        // TODO: handle exps (type checking)
        const tok = self.scanner.peek();
        return switch (tok.tokenType) {
            token.TokenType.SUB, token.TokenType.NEQ => self.parseUnaryOperators(),
            else => self.parseArithmeticExpressionsLower(),
        };
    }

    // E^8, *, /, %
    fn parseArithmeticExpressionsLower(self: *Parser) ParseError!Expression {
        var exp = try self.parseArrayIndexingAndPrefixExpressions();

        var tok = self.scanner.peek();
        switch (tok.tokenType) {
            token.TokenType.MUL, token.TokenType.QUO, token.TokenType.REM => {},
            else => return exp,
        }

        while (true) {
            tok = self.scanner.peek();

            switch (tok.tokenType) {
                token.TokenType.MUL, token.TokenType.QUO, token.TokenType.REM => {},
                else => return exp, // TODO: this may be incorrect.
            }

            _ = self.scanner.next();
            var exp2 = try self.parseArrayIndexingAndPrefixExpressions();
            _ = exp2;
        }
    }

    // E^9, array indexing and ++, --, #
    fn parseArrayIndexingAndPrefixExpressions(self: *Parser) ParseError!Expression {
        const exp = try self.parseI();

        // TODO:
        //  * implement the pointer-of operator #
        //  * array indexing (=> array type implemented)
        while (true) {
            const tok = self.scanner.peek();
            switch (tok.tokenType) {
                token.TokenType.INC, token.TokenType.DEC => {
                    // TODO: refactor this mess.
                    if (!types.IsNum(exp.expType)) {
                        self.failedAt = tok;
                        self.errorMsg = std.fmt.allocPrint(self.alloc, "value of type {s} cannot be incremented or decremented", .{@tagName(exp.expType)}) catch unreachable;
                        return ParseError.TypeError;
                    }

                    _ = self.scanner.next();
                    return exp;
                },
                token.TokenType.LBRACK => {
                    // TODO: handle brackets.
                    //  * checking bounds?
                    //  * checking type? e.g. `exp` is not of type array of somethings therefore cannot be indexed
                    //  * parse the indexing expression even when typecheking^ fails?
                    //  * special care for multidimensional array
                    try self.consume(token.TokenType.LBRACK);
                    const indexExp = try self.parseExpression();
                    // TODO: this^ can fail and write some message. This means that there cannot be just a single
                    // error message but array of them. The error reporting below therefore can override something.

                    if (!types.IsIntegral(indexExp.expType)) {
                        self.failedAt = tok;
                        self.errorMsg = std.fmt.allocPrint(self.alloc, "array cannot be index with type {s}", .{@tagName(indexExp.expType)}) catch unreachable;
                        return ParseError.TypeError;
                    }

                    try self.consume(token.TokenType.RBRACK);
                    // TODO: what expression should be returned?
                    return exp;
                },
                // TBD: this must be fixed next. (tests fail)
                else => unreachable,
            }
        }
        unreachable;
    }

    fn parseI(self: *Parser) ParseError!Expression {
        var tok = self.scanner.next();

        // TODO: handle type casting

        switch (tok.tokenType) {
            token.TokenType.LPAREN => {
                const exp = try self.parseExpression();
                try self.consume(token.TokenType.LPAREN);
                return exp;
            },
            token.TokenType.IDENT => {
                // TODO:
                // * check identifier in the symbol table
                //    => whether it's already declared
                //    => retrieve the type for the expression
                //    => is it a function call?

                return Expression{
                    .expType = types.Type{ .sType = types.SimpleType.STRING },
                };
            },
            token.TokenType.C_INT => {
                // TODO: check ranges and decide the type.
                return Expression{
                    .expType = types.Type{ .sType = types.SimpleType.I32 },
                };
            },
            token.TokenType.C_FLOAT => {
                return Expression{
                    .expType = types.Type{ .sType = types.SimpleType.FLOAT },
                };
            },
            token.TokenType.C_BOOL => {
                return Expression{
                    .expType = types.Type{ .sType = types.SimpleType.BOOL },
                };
            },
            token.TokenType.C_STRING => {
                return Expression{
                    .expType = types.Type{ .sType = types.SimpleType.STRING },
                };
            },
            token.TokenType.C_CHAR => {
                return Expression{
                    .expType = types.Type{ .sType = types.SimpleType.CHAR },
                };
            },
            token.TokenType.AT => {
                // todo: handle builtin functions (only casting here makes sense)
            },
            else => {
                // TODO: what now?
                // What if we parse "," that's valid for for loops?
            },
        }
        unreachable;
    }

    fn consume(self: *Parser, want: scanner.TokenType) ParseError!void {
        _ = try self.consumeGet(want);
    }

    fn consumeGet(self: *Parser, want: scanner.TokenType) ParseError!scanner.Token {
        const next = self.scanner.next();

        if (next.tokenType == token.TokenType.ILLEGAL) {
            // It does not make sense to report ILLEGAL tokens here since the scanner has already done it.
            // TODO: Remove error reporting from scanner and do it just here for simplicity?
            return ParseError.UnexpectedToken;
        }

        if (want != next.tokenType) {
            self.failedAt = next;
            // Only way the std.fmt.allocPrint can fail is with `OutOfMemory` where it makes sense for us to fail.
            self.errorMsg = std.fmt.allocPrint(self.alloc, "expected '{s}' but found '{s}' instead", .{ token.TokenType.str(want), token.TokenType.str(next.tokenType) }) catch unreachable;
            return ParseError.UnexpectedToken;
        }

        return next;
    }
};
