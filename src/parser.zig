const std = @import("std");

const types = @import("types.zig");
const scanner = @import("scanner.zig");
const token = @import("token.zig");
const reporter = @import("reporter.zig");
const symbols = @import("symbols.zig");

const tt = token.TokenType;

pub const SyntaxError = error{ UnexpectedToken, TypeError, OutOfMemory, RecoverableError };

pub const Expression = struct {
    // expType denotes the type of the result of this expression.
    // token.TokenType.isType(this) must always evaluate to true.
    lType: types.Type = undefined,

    rValue: []const u8 = "",
    rType: types.Type = undefined,

    hasLValue: bool = false,
    lValueComputed: bool = false,
    lValue: []const u8 = "",

    semicolonFollows: bool = true,
    endsWithReturn: bool = false,

    // callsFunction denotes whether there is a function call in the expression somewhere.
    // This is done to restrict function calls in the top-level statements.
    callsFunction: bool = false,
};

pub const Parser = struct {
    scanner: scanner.Scanner,
    rep: ?reporter.Reporter,

    alloc: std.mem.Allocator,

    st: symbols.SymbolTable,

    emit: bool,

    // TODO:
    // * IR emitter
    // * analysis: main is defined (with correct arguments)
    // * zero-initialized variables?

    pub fn init(s: scanner.Scanner, r: ?reporter.Reporter, alloc: std.mem.Allocator) Parser {
        return .{
            .scanner = s,
            .rep = r,
            .alloc = alloc,
            .st = symbols.SymbolTable.init(alloc),
            .emit = true,
        };
    }

    pub fn deinit(self: *Parser) void {
        self.st.deinit();
    }

    pub fn parse(self: *Parser) !void {
        // Open the global scope.
        try self.st.open();

        var next = self.scanner.peek();
        while (next.tokenType != tt.EOF) {
            if (next.tokenType == tt.COMMENT) {
                self.consume(tt.COMMENT) catch unreachable;
            } else {
                self.parseTopLevelStatement() catch {};
            }
            next = self.scanner.peek();
        }

        // Report any declared but not defined functions.
        var it = self.st.globalIterator();
        while (it.next()) |s| {
            switch (s.t.*) {
                types.TypeTag.func => |func| {
                    if (!func.defined) {
                        self.report(s.location, reporter.Level.ERROR, "function '{s}' is declared but not defined", .{s.name}, true, true);
                    }
                },
                else => {},
            }
        }

        // TODO: check for main etc.
        self.st.close();
    }

    fn parseTopLevelStatement(self: *Parser) SyntaxError!void {
        // 1) global variable definition: <type> <ident> = <expr>;
        // 2) (forward) function declaration: <type> <ident>(<arglist>);
        // 3) function definition: <type> <ident>(<arglist>) <body>
        const t = try self.parseType(true);
        const ident = try self.consumeGet(tt.IDENT);

        var next = self.scanner.next();
        switch (next.tokenType) {
            tt.ASSIGN => {
                // TODO: self.parseExpression()?
            },
            tt.LPAREN => {
                var ft = self.alloc.create(types.Type) catch unreachable;
                ft.* = types.Type{ .func = types.Func.init(self.alloc, t) };
                errdefer ft.destroy(self.alloc);
                ft.func.args = try self.parseArgList();

                var fs = symbols.Symbol{
                    .name = ident.symbol,
                    .llvmName = "TODO",
                    .location = ident,
                    .t = ft,
                };

                // Try to parse the function body if it is a function definition.
                next = self.scanner.peek();
                if (next.tokenType == tt.LBRACE) {
                    // Open a new scope just for the function arguments. This way,
                    // arguments can be shadowed in the function body.
                    try self.st.open();

                    fs.t.func.namedParams = true;
                    for (fs.t.func.args.items) |arg| {
                        if (std.mem.eql(u8, arg.name, "")) {
                            if (fs.t.func.namedParams) {
                                self.report(fs.location, reporter.Level.ERROR, "function definition of '{s}' must have named arguments", .{fs.name}, true, true);
                                self.emit = false;
                            }
                            fs.t.func.namedParams = false;
                        } else {
                            var newArg = arg.clone(self.alloc);
                            self.st.insert(newArg) catch |err| switch (err) {
                                symbols.SymbolError.SymbolAlreadyExists => {
                                    self.report(fs.location, reporter.Level.ERROR, "cannot define 2 function arguments with the same name", .{}, true, true);
                                    self.emit = false;
                                    // Destroy the new argument immediately since we need to move on with the parsing.
                                    newArg.destroy(self.alloc);
                                },
                                symbols.SymbolError.OutOfMemory => return SyntaxError.OutOfMemory,
                            };
                        }
                    }

                    try self.parseBody();
                    // Close the scope just for the function arguments.
                    self.st.close();

                    fs.t.func.defined = true;
                } else {
                    try self.consume(tt.SEMICOLON);
                }

                // Check that if there's a symbol with the same name, it must be a function with identical
                // argument types that has been declared but not defined yet.
                if (self.st.get(ident.symbol)) |s| {
                    switch (s.t.*) {
                        types.TypeTag.func => |otherFunc| {
                            ft.func.defines(otherFunc) catch |err| {
                                switch (err) {
                                    types.Func.DefinitionErrors.AlreadyDefined => {
                                        self.report(fs.location, reporter.Level.ERROR, "function '{s}' is already defined", .{fs.name}, true, true);
                                        self.report(s.location, reporter.Level.NOTE, "'{s}' first defined here", .{fs.name}, false, false);
                                        s.t.func.defined = true; // Mark the declared function as defined in order to not emit unrelated error.
                                    },
                                    types.Func.DefinitionErrors.ArgTypeMismatch => {
                                        self.report(fs.location, reporter.Level.ERROR, "definition of '{s}' does not match its declaration", .{fs.name}, true, true);
                                        self.report(s.location, reporter.Level.NOTE, "'{s}' first declared here", .{fs.name}, false, false);
                                        s.t.func.defined = true; // Mark the declared function as defined in order to not emit unrelated error.
                                        // TODO: Make error reporting better - show where the mismatch is.
                                    },
                                }
                                self.emit = false;

                                // TODO: Return better error? We need to return with error
                                // in order to destroy all the data structures.
                                return SyntaxError.TypeError;
                            };

                            // Upsert the declared function with it's definition in the symbol table.
                            self.st.upsert(fs);
                        },
                        else => {
                            self.emit = false;
                            self.report(fs.location, reporter.Level.ERROR, "function '{s}' shadows global variable", .{fs.name}, true, true);
                            // TODO: Return better error? We need to return with error
                            // in order to destroy all the data structures.
                            return SyntaxError.TypeError;
                        },
                    }
                } else {
                    // SymbolAlreadyExists cannot occur since we check it above^.
                    self.st.insert(fs) catch unreachable;
                }
            },
            else => {
                t.destroy(self.alloc);

                self.emit = false;
                self.report(next, reporter.Level.ERROR, "expected either function declaration, definition or global variable definiton", .{}, true, true);
                return SyntaxError.UnexpectedToken;
            },
        }
    }

    fn parseArgList(self: *Parser) SyntaxError!std.ArrayList(symbols.Symbol) {
        // LPAREN is already consumed, scanner must point to the type of the first argument.
        var expectComma = false;
        var next = self.scanner.peek();

        const argNamesE = enum { DoNotKnow, Named, Unnamed };
        var argNames = argNamesE.DoNotKnow;

        var args = std.ArrayList(symbols.Symbol).init(self.alloc);
        while (next.tokenType != tt.RPAREN) {
            if (expectComma) try self.consume(tt.COMMA);

            var s = symbols.Symbol{
                .t = try self.parseType(true),
            };

            errdefer s.t.destroy(self.alloc);
            if (argNames == argNamesE.Unnamed or
                ((self.scanner.peek().tokenType == tt.COMMA or self.scanner.peek().tokenType == tt.RPAREN) and argNames == argNamesE.DoNotKnow))
            {
                argNames = argNamesE.Unnamed;
                s.location = undefined;
            } else {
                argNames = argNamesE.Named;
                const id = try self.consumeGet(tt.IDENT);
                s.name = id.symbol;
                s.llvmName = "TODO";
                s.location = id;
            }

            if (std.mem.eql(u8, s.name, "") and types.SimpleType.isType(s.name)) {
                self.emit = false;
                self.report(next, reporter.Level.ERROR, "function argument must not be named as type", .{}, true, true);
            } else {
                try args.append(s);
            }

            expectComma = true;
            next = self.scanner.peek();
        }

        self.consume(tt.RPAREN) catch unreachable;
        return args;
    }

    /// Tries to parse a type. If a type cannot be parsed, scanner's state is rewinded back
    /// to the state before parsing.
    fn tryParseType(self: *Parser, verbose: bool) ?*types.Type {
        const oldOffset = self.scanner.offset;
        const oldCharOffset = self.scanner.charOffset;
        const oldLineOffest = self.scanner.lineOffset;
        const oldLastToken = self.scanner.lastToken;

        var t = self.parseType(verbose) catch {
            self.scanner.offset = oldOffset;
            self.scanner.charOffset = oldCharOffset;
            self.scanner.lineOffset = oldLineOffest;
            self.scanner.lastToken = oldLastToken;
            return null;
        };

        return t;
    }

    /// Parses a type. `verbose` argument specifies whether to report an error when parsing the type.
    pub fn parseType(self: *Parser, verbose: bool) SyntaxError!*types.Type {
        var tok = self.scanner.next();

        var tp = try self.alloc.create(types.Type);
        errdefer self.alloc.destroy(tp);

        return switch (tok.tokenType) {
            tt.IDENT => if (types.SimpleType.getType(tok.symbol)) |t| {
                tp.* = types.Type{ .sType = t };
                return tp;
            } else {
                self.emit = false;
                if (verbose) self.report(tok, reporter.Level.ERROR, "'{s}' is not a type", .{tok.symbol}, true, true);
                return SyntaxError.UnexpectedToken;
            },
            tt.LBRACK => {
                var array: types.Array = .{};
                while (tok.tokenType != tt.RBRACK) {
                    try self.consume(tt.SUB);
                    array.dimensions += 1;

                    tok = self.scanner.next();
                    if (tok.tokenType == tt.COMMA) {}
                }
                array.ofType = try self.parseType(true);
                tp.* = types.Type{ .cType = array };
                return tp;
            },
            else => {
                self.emit = false;
                if (verbose) self.report(tok, reporter.Level.ERROR, "expected type, found '{s}' instead", .{tok.symbol}, true, true);
                return SyntaxError.UnexpectedToken;
            },
        };
    }

    // E
    pub fn parseExpression(self: *Parser) SyntaxError!void {
        var tok = self.scanner.peek();

        while (!endParseExpression(tok.tokenType)) {
            switch (tok.tokenType) {
                tt.WHILE => try self.parseWhile(),
                tt.DO => try self.parseDoWhile(),
                tt.FOR => try self.parseFor(),
                tt.IF => try self.parseIf(),
                tt.RETURN => try self.parseReturn(),
                tt.BREAK => try self.parseBreak(),
                tt.CONTINUE => try self.parseContinue(),
                tt.SEMICOLON => try self.consume(tt.SEMICOLON),
                else => {
                    _ = try self.parseSubExpression();
                    // TODO: semicolon magic here.
                },
            }

            tok = self.scanner.peek();
        }
    }

    fn endParseExpression(tokenType: tt) bool {
        const enders = [_]tt{ tt.EOF, tt.RBRACE, tt.RBRACK, tt.RPAREN, tt.COMMA, tt.COLON };

        for (enders) |ender| if (ender == tokenType) return true;

        return false;
    }

    fn parseIf(self: *Parser) SyntaxError!void {
        try self.consume(tt.IF);
        try self.consume(tt.LPAREN);
        _ = try self.parseExpression(); // TODO: typecheck
        try self.consume(tt.RPAREN);
        try self.parseBody();

        if (self.scanner.peek().tokenType != tt.ELSE) {
            // TODO: implement this.
            return;
        }

        self.consume(tt.ELSE) catch unreachable;

        if (self.scanner.peek().tokenType == tt.IF) {
            // TODO: take extra care with synthesized arguments here. Check what should happen
            // if parseI fails.
            _ = try self.parseIf();
        } else {
            try self.parseBody();
        }
    }

    fn parseFor(self: *Parser) SyntaxError!void {
        self.consume(tt.FOR) catch unreachable;
        try self.consume(tt.LPAREN);
        _ = try self.parseExpression();
        try self.consume(tt.COMMA);
        _ = try self.parseExpression(); // TODO: typecheck
        try self.consume(tt.COMMA);
        _ = try self.parseExpression();
        try self.consume(tt.RPAREN);
        try self.parseBody();
    }

    fn parseWhile(self: *Parser) SyntaxError!void {
        self.consume(tt.WHILE) catch unreachable;
        try self.consume(tt.LPAREN);
        _ = try self.parseExpression(); // TODO: typecheck
        try self.parseBody();
    }

    fn parseDoWhile(self: *Parser) SyntaxError!void {
        self.consume(tt.DO) catch unreachable;
        try self.parseBody();
        try self.consume(tt.WHILE);
        try self.consume(tt.LPAREN);
        _ = try self.parseExpression(); // TODO: typecheck
        try self.consume(tt.RPAREN);
    }

    fn parseBody(self: *Parser) SyntaxError!void {
        self.consume(tt.LBRACE) catch unreachable;
        _ = try self.parseExpression();
        try self.consume(tt.RBRACE);
    }

    fn parseReturn(self: *Parser) SyntaxError!void {
        self.consume(tt.RETURN) catch unreachable;

        switch (self.scanner.peek().tokenType) {
            tt.SEMICOLON, tt.RPAREN, tt.RBRACK, tt.RBRACE, tt.COMMA, tt.COLON => {
                // TODO: check that the return type of the current context is void ("unit"),
                // throw type error otherwise since return argument is expected (of the correct type).
            },
            else => try self.parseExpression(),
        }
    }

    fn parseBreak(self: *Parser) SyntaxError!void {
        // TODO: check that the break_stack is not empty (=> there's something to break out of)
        self.consume(tt.BREAK) catch unreachable;
        // TODO: semicolon magic here.
    }

    fn parseContinue(self: *Parser) SyntaxError!void {
        // TODO: check that the continue_stack is not empty (=> there's something to continue in)
        self.consume(tt.CONTINUE) catch unreachable;
        // TODO: semicolon magic here.
    }

    // E^1
    fn parseSubExpression(self: *Parser) SyntaxError!Expression {
        var exp = Expression{ .lType = undefined };
        var decl = true;

        if (self.tryParseType(false)) |t| {
            // TODO:parseType moves the scanner!!!!
            // This must not move the scanner and return whatever is under it instead....
            // TODO: work on this first...
            const ident = try self.consumeGet(tt.IDENT);
            // TODO: emit alloca
            self.st.insert(symbols.Symbol{
                .name = ident.symbol,
                .llvmName = "TODO",
                .location = ident,
                .t = t,
            }) catch {
                self.emit = false;
                self.report(ident, reporter.Level.ERROR, "'{s}' is already defined in the current scope", .{ident.symbol}, true, true);
            };
        } else {
            // not a type
            decl = false;
            exp = try self.parseTernaryExpression();
        }

        var next = self.scanner.peek();

        if (!token.TokenType.isAssignment(next.tokenType)) return exp;
        if (decl) next = try self.consumeGet(tt.ASSIGN);

        if (!exp.hasLValue) {
            self.emit = false;
            self.report(next, reporter.Level.ERROR, "does not have l-value", .{}, true, true);
            return SyntaxError.RecoverableError;
        }

        switch (next.tokenType) {
            tt.ASSIGN => {
                const rExp = try self.parseSubExpression();
                exp.rType = rExp.lType;

                if (exp.lType.equals(types.Type{ .sType = types.SimpleType.UNIT }) and exp.rType.equals(types.Type{ .sType = types.SimpleType.UNIT })) {} else {
                    // TODO: type conversion goes here: lType and rType
                }

                // exp.rValue =
                exp.semicolonFollows = true;
            },
            tt.ADD_ASSIGN => {},
            tt.SUB_ASSIGN => {},
            tt.MUL_ASSIGN => {},
            tt.QUO_ASSIGN => {},
            tt.REM_ASSIGN => {},
            tt.LSH_ASSIGN => {},
            tt.RSH_ASSIGN => {},
            tt.AND_ASSIGN => {},
            tt.XOR_ASSIGN => {},
            tt.OR_ASSIGN => {},
            tt.LAND_ASSIGN => {},
            tt.LOR_ASSIGN => {},
            else => @panic("ICE: no other assignments can occur"),
        }

        return exp;
    }

    // E^2
    fn parseTernaryExpression(self: *Parser) SyntaxError!Expression {
        // <b_expr> ? <expr> : <expr>;
        // exp is potentially a boolen condition for the ternary operator.
        var exp = try self.parseLogicExpressions();
        // TODO: type checking
        // * condition must be a boolean expression
        // * if and else branches must have the same type
        // * automatic type conversion if one branch is more specific than the other

        var tok = self.scanner.peek();
        if (tok.tokenType != tt.QUESTION_MARK) {
            return exp;
        }

        var thenBranch = try self.parseExpression();
        try self.consume(tt.COLON);
        var elseBranch = try self.parseExpression();

        _ = thenBranch;
        _ = elseBranch;

        return exp;
    }

    // E^3, binary and, or, ||, &&
    fn parseLogicExpressions(self: *Parser) SyntaxError!Expression {
        var exp = try self.parseNot();

        var tok = self.scanner.peek();
        switch (tok.tokenType) {
            tt.OR, tt.AND, tt.LAND, tt.LOR => {},
            else => return exp,
        }

        // last_value:
        // konec:
        // dedeny_konec:
        while (true) {
            tok = self.scanner.peek();
            switch (tok.tokenType) {
                tt.OR => {},
                tt.AND => {},
                tt.LAND => {},
                tt.LOR => {},
                else => return exp,
            }

            _ = self.scanner.next();
            var exp2 = try self.parseNot();
            _ = exp2;
        }
    }

    // E^4, unary not
    fn parseNot(self: *Parser) SyntaxError!Expression {
        const tok = self.scanner.peek();
        if (tok.tokenType != tt.NOT) {
            return self.parseRelationalExpression();
        }

        // Skip the '!' and continue.
        // TODO: type checking
        _ = self.scanner.next();
        return self.parseNot();
    }

    // E^5, ==, !=, >, <, >=, =<
    fn parseRelationalExpression(self: *Parser) SyntaxError!Expression {
        // TODO: handle exp (typechecking)
        var exp = try self.parseArithmeticExpression();

        var tok = self.scanner.peek();
        switch (tok.tokenType) {
            tt.EQL, tt.NEQ, tt.LT, tt.GT, tt.LEQ, tt.GEQ => {},
            else => return exp,
        }

        while (true) {
            tok = self.scanner.peek();

            switch (tok.tokenType) {
                tt.EQL, tt.NEQ, tt.LT, tt.GT, tt.LEQ, tt.GEQ => {},
                else => return exp,
            }

            _ = self.scanner.next();
            var exp2 = try self.parseArithmeticExpression();
            _ = exp2;
        }
    }

    // E^6, +, -, >>, <<, &, ^, |
    fn parseArithmeticExpression(self: *Parser) SyntaxError!Expression {
        // TODO: handle exp (typechecking)
        var exp = try self.parseUnaryOperators();

        var tok = self.scanner.peek();
        switch (tok.tokenType) {
            tt.ADD, tt.SUB, tt.LT, tt.GT, tt.LEQ, tt.GEQ => {},
            else => return exp,
        }

        while (true) {
            tok = self.scanner.peek();

            switch (tok.tokenType) {
                tt.ADD, tt.SUB, tt.LT, tt.GT, tt.LEQ, tt.GEQ => {},
                else => return exp,
            }

            _ = self.scanner.next();
            var exp2 = try self.parseUnaryOperators();
            _ = exp2;
        }
    }

    // E^7, -, !
    fn parseUnaryOperators(self: *Parser) SyntaxError!Expression {
        // TODO: handle exps (type checking)
        const tok = self.scanner.peek();
        return switch (tok.tokenType) {
            tt.SUB, tt.NEQ => self.parseUnaryOperators(),
            else => self.parseArithmeticExpressionsLower(),
        };
    }

    // E^8, *, /, %
    fn parseArithmeticExpressionsLower(self: *Parser) SyntaxError!Expression {
        var exp = try self.parseArrayIndexingAndPrefixExpressions();

        var tok = self.scanner.peek();
        switch (tok.tokenType) {
            tt.MUL, tt.QUO, tt.REM => {},
            else => return exp,
        }

        while (true) {
            tok = self.scanner.peek();

            switch (tok.tokenType) {
                tt.MUL, tt.QUO, tt.REM => {},
                else => return exp, // TODO: this may be incorrect.
            }

            _ = self.scanner.next();
            var exp2 = try self.parseArrayIndexingAndPrefixExpressions();
            _ = exp2;
        }
    }

    // E^9, array indexing and ++, --, #
    fn parseArrayIndexingAndPrefixExpressions(self: *Parser) SyntaxError!Expression {
        const exp = try self.parseI();

        // TODO:
        //  * implement the pointer-of operator #
        //  * array indexing (=> array type implemented)
        while (true) {
            const tok = self.scanner.peek();
            switch (tok.tokenType) {
                tt.INC, tt.DEC => {
                    if (!types.IsNum(exp.lType)) {
                        self.emit = false;
                        self.report(tok, reporter.Level.ERROR, "value of type '{s}' cannot be incremented or decremented", .{@tagName(exp.lType)}, true, true);
                        return SyntaxError.TypeError;
                    }

                    _ = self.scanner.next();
                    return exp;
                },
                tt.LBRACK => {
                    // TODO: handle brackets.
                    //  * checking bounds?
                    //  * checking type? e.g. `exp` is not of type array of somethings therefore cannot be indexed
                    //  * parse the indexing expression even when typecheking^ fails?
                    //  * special care for multidimensional array
                    try self.consume(tt.LBRACK);
                    const indexExp = try self.parseSubExpression();
                    // TODO: this^ can fail and write some message. This means that there cannot be just a single
                    // error message but array of them. The error reporting below therefore can override something.

                    if (!types.IsIntegral(indexExp.lType)) {
                        self.emit = false;
                        self.report(tok, reporter.Level.ERROR, "array cannot be indexed with type '{s}'", .{@tagName(indexExp.lType)}, true, true);
                        return SyntaxError.TypeError;
                    }

                    // TODO: check that if the index is constant, it is positive.

                    try self.consume(tt.RBRACK);
                    // TODO: what expression should be returned?
                    return exp;
                },
                // TBD: this must be fixed next. (tests fail)
                else => return exp,
            }
        }
        unreachable;
    }

    fn parseI(self: *Parser) SyntaxError!Expression {
        var tok = self.scanner.next();

        // TODO: handle type casting

        switch (tok.tokenType) {
            tt.LPAREN => {
                const exp = try self.parseSubExpression();
                try self.consume(tt.RPAREN);
                return exp;
            },
            tt.IDENT => {
                // TODO:
                // * check identifier in the symbol table
                //    => whether it's already declared
                //    => retrieve the type for the expression
                //    => is it a function call?

                return Expression{
                    .lType = types.Type{ .sType = types.SimpleType.STRING },
                };
            },
            tt.C_INT => {
                // TODO: check ranges and decide the type.
                return Expression{
                    .lType = types.Type{ .sType = types.SimpleType.I32 },
                };
            },
            tt.C_FLOAT => {
                return Expression{
                    .lType = types.Type{ .sType = types.SimpleType.FLOAT },
                };
            },
            tt.C_BOOL => {
                return Expression{
                    .lType = types.Type{ .sType = types.SimpleType.BOOL },
                };
            },
            tt.C_STRING => {
                return Expression{
                    .lType = types.Type{ .sType = types.SimpleType.STRING },
                };
            },
            tt.C_CHAR => {
                return Expression{
                    .lType = types.Type{ .sType = types.SimpleType.CHAR },
                };
            },
            tt.AT => {
                // todo: handle builtin functions (only casting here makes sense)
            },
            else => {
                // TODO: handle what should be done here.
                return Expression{
                    .lType = types.Type{ .sType = types.SimpleType.CHAR },
                };
            },
        }
        unreachable;
    }

    fn consume(self: *Parser, want: tt) SyntaxError!void {
        _ = try self.consumeGet(want);
    }

    fn consumeGet(self: *Parser, want: tt) SyntaxError!scanner.Token {
        const next = self.scanner.next();

        if (next.tokenType == tt.ILLEGAL) {
            self.emit = false;
            self.report(next, reporter.Level.ERROR, "encountered illegal symbol '{s}'", .{next.symbol}, true, true);
        }

        if (want != next.tokenType) {
            self.emit = false;
            self.report(next, reporter.Level.ERROR, "expected '{s}' but found '{s}' instead", .{ tt.str(want), next.symbol }, true, true);
            return SyntaxError.UnexpectedToken;
        }

        return next;
    }

    fn report(self: *Parser, tok: token.Token, level: reporter.Level, comptime fmt: []const u8, args: anytype, showLine: bool, space: bool) void {
        if (self.rep) |rep| {
            const msg = std.fmt.allocPrint(self.alloc, fmt, args) catch unreachable;
            defer self.alloc.free(msg);

            if (space) {
                rep.space();
                rep.space();
            }
            rep.report(tok, level, msg, showLine);
        }
    }
};
