const std = @import("std");

const types = @import("types.zig");
const scanner = @import("scanner.zig");
const token = @import("token.zig");
const reporter = @import("reporter.zig");
const symbols = @import("symbols.zig");

const tt = token.TokenType;

pub const SyntaxError = error{ UnexpectedToken, TypeError, OutOfMemory, RecoverableError };

pub const Expression = struct {
    t: *types.Type = undefined,
    lt: *types.Type = undefined,

    rValue: []const u8 = "",

    hasLValue: bool = false,
    lValueComputed: bool = false,
    lValue: []const u8 = "",

    semiMustFollow: bool = false,
    endsWithReturn: bool = false,
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
                self.parseTopLevelStatement() catch return SyntaxError.UnexpectedToken;
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

        const t = try self.parseType();
        // errdefer t.destroy(self.alloc);

        const ident = try self.consumeGet(tt.IDENT);

        var next = self.scanner.next();
        switch (next.tokenType) {
            tt.ASSIGN => {
                // TODO: self.parseExpression()?
            },
            tt.LPAREN => {
                var ft = self.alloc.create(types.Type) catch unreachable;
                ft.* = types.Type{ .func = types.Func.init(self.alloc, t) };
                // TODO: Freeing ft?
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

                    var exp = try self.parseBody();

                    errdefer exp.t.destroy(self.alloc);
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
                // t.destroy(self.alloc);

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
        errdefer args.deinit();

        while (next.tokenType != tt.RPAREN) {
            if (expectComma) try self.consume(tt.COMMA);

            var s = symbols.Symbol{
                .t = try self.parseType(),
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

    /// Parses a type.
    pub fn parseType(self: *Parser) SyntaxError!*types.Type {
        var tok = self.scanner.next();

        var tp = try self.alloc.create(types.Type);
        errdefer self.alloc.destroy(tp);

        return switch (tok.tokenType) {
            tt.IDENT => if (types.SimpleType.getType(tok.symbol)) |t| {
                tp.* = types.Type{ .simple = t };
                return tp;
            } else {
                self.emit = false;
                self.report(tok, reporter.Level.ERROR, "'{s}' is not a type", .{tok.symbol}, true, true);
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
                array.ofType = try self.parseType();
                tp.* = types.Type{ .array = array };
                return tp;
            },
            else => {
                self.emit = false;
                self.report(tok, reporter.Level.ERROR, "expected type, found '{s}' instead", .{tok.symbol}, true, true);
                return SyntaxError.UnexpectedToken;
            },
        };
    }

    // E
    pub fn parseExpression(self: *Parser) SyntaxError!Expression {
        var tok = self.scanner.peek();

        while (!endParseExpression(tok.tokenType)) {
            var exp: ?Expression = null;
            defer {
                // Destroy the old expression after every iteration.
                // TODO: This must be fixed in order to properly return the expression.
                if (exp) |e| e.t.destroy(self.alloc);
            }

            switch (tok.tokenType) {
                tt.WHILE => {
                    exp = try self.parseWhile();
                    errdefer exp.t.destroy(self.alloc);
                },
                tt.DO => {
                    exp = try self.parseDoWhile();
                    errdefer exp.t.destroy(self.alloc);
                },
                tt.FOR => {
                    exp = try self.parseFor();
                    errdefer exp.t.destroy(self.alloc);
                },
                tt.IF => {
                    exp = try self.parseIf();
                    errdefer exp.t.destroy(self.alloc);
                },
                tt.RETURN => {
                    exp = try self.parseReturn();
                    errdefer exp.t.destroy(self.alloc);
                },
                tt.BREAK => {
                    exp = try self.parseBreak();
                    errdefer exp.t.destroy(self.alloc);
                },
                tt.CONTINUE => {
                    exp = try self.parseContinue();
                    errdefer exp.t.destroy(self.alloc);
                },
                tt.SEMICOLON => {
                    // TODO: What should happen here with the expression?
                    try self.consume(tt.SEMICOLON);
                },
                else => {
                    exp = try self.parseSubExpression();
                    errdefer exp.t.destroy(self.alloc);
                },
            }

            tok = self.scanner.peek();
        }

        return Expression{};
    }

    fn endParseExpression(tokenType: tt) bool {
        const enders = [_]tt{ tt.EOF, tt.RBRACE, tt.RBRACK, tt.RPAREN, tt.COMMA, tt.COLON };

        for (enders) |ender| if (ender == tokenType) return true;

        return false;
    }

    fn parseIf(self: *Parser) SyntaxError!Expression {
        try self.consume(tt.IF);
        try self.consume(tt.LPAREN);

        var ifExp = try self.parseExpression();
        errdefer ifExp.t.destroy(self.alloc);

        try self.consume(tt.RPAREN);

        var body = try self.parseBody();
        errdefer body.t.destroy(self.alloc);

        if (self.scanner.peek().tokenType != tt.ELSE) {
            // TODO: implement this.
            return Expression{};
        }

        self.consume(tt.ELSE) catch unreachable;

        if (self.scanner.peek().tokenType == tt.IF) {
            var anotherIfExp = try self.parseIf();
            errdefer anotherIfExp.t.destroy(self.alloc);
        } else {
            var anotherBody = try self.parseBody();
            errdefer anotherBody.t.destroy(self.alloc);
        }

        return Expression{};
    }

    fn parseFor(self: *Parser) SyntaxError!Expression {
        self.consume(tt.FOR) catch unreachable;
        try self.consume(tt.LPAREN);

        var initExp = try self.parseExpression();
        errdefer initExp.t.destroy(self.alloc);

        try self.consume(tt.COMMA);

        var condExp = try self.parseExpression();
        errdefer condExp.t.destroy(self.alloc);

        try self.consume(tt.COMMA);

        var stepExp = try self.parseExpression();
        errdefer stepExp.t.destroy(self.alloc);

        try self.consume(tt.RPAREN);

        var body = try self.parseBody();
        errdefer body.t.destroy(self.alloc);

        return Expression{};
    }

    fn parseWhile(self: *Parser) SyntaxError!Expression {
        self.consume(tt.WHILE) catch unreachable;
        try self.consume(tt.LPAREN);

        var exp = try self.parseExpression(); // TODO: typecheck
        errdefer exp.t.destroy(self.alloc);
        try self.consume(tt.RPAREN);

        var body = try self.parseBody();
        errdefer body.t.destroy(self.alloc);

        return Expression{};
    }

    fn parseDoWhile(self: *Parser) SyntaxError!Expression {
        self.consume(tt.DO) catch unreachable;

        var body = try self.parseBody();
        errdefer body.t.destroy(self.alloc);

        try self.consume(tt.WHILE);
        try self.consume(tt.LPAREN);

        var exp = try self.parseExpression(); // TODO: typecheck
        errdefer exp.t.destroy(self.alloc);

        try self.consume(tt.RPAREN);
        return Expression{};
    }

    fn parseBody(self: *Parser) SyntaxError!Expression {
        try self.consume(tt.LBRACE);

        var exp = try self.parseExpression();
        errdefer exp.t.destroy(self.alloc);

        try self.consume(tt.RBRACE);

        return exp;
    }

    fn parseReturn(self: *Parser) SyntaxError!Expression {
        self.consume(tt.RETURN) catch unreachable;

        return switch (self.scanner.peek().tokenType) {
            tt.SEMICOLON, tt.RPAREN, tt.RBRACK, tt.RBRACE, tt.COMMA, tt.COLON => blk: {
                // TODO: check that the return type of the current context is void ("unit"),
                // throw type error otherwise since return argument is expected (of the correct type).
                break :blk Expression{};
            },
            else => try self.parseSubExpression(),
        };
    }

    fn parseBreak(self: *Parser) SyntaxError!Expression {
        // TODO: check that the break_stack is not empty (=> there's something to break out of)
        self.consume(tt.BREAK) catch unreachable;
        // TODO: semicolon magic here.
        return Expression{};
    }

    fn parseContinue(self: *Parser) SyntaxError!Expression {
        // TODO: check that the continue_stack is not empty (=> there's something to continue in)
        self.consume(tt.CONTINUE) catch unreachable;
        // TODO: semicolon magic here.
        return Expression{};
    }

    // E^1
    fn parseSubExpression(self: *Parser) SyntaxError!Expression {
        var exp = Expression{};
        var declaration = true;

        var tok = self.scanner.peek();
        if (std.mem.eql(u8, tok.symbol, "unit")) {
            try self.consume(tt.IDENT);
            try self.define(tok.symbol, types.SimpleType.create(self.alloc, types.SimpleType.UNIT), tok);
            // TODO: Is it possible to continue with parsing even when TypeError occured?
            // TODO: set expression attributes - type = unit, lvalue...
        } else if (types.startsType(tok.symbol)) {
            // Current token under the cursor is either a simple type or an array type.
            var t = try self.parseType();
            errdefer t.destroy(self.alloc);

            tok = try self.consumeGet(tt.IDENT);
            try self.define(tok.symbol, t, tok);
            // TODO: Is it possible to continue with parsing even when TypeError occured?
            // TODO: emit IR & set expression attributes.
        } else {
            declaration = false;

            exp = try self.parseTernaryExpression();
            errdefer exp.t.destroy(self.alloc);
        }

        var assignTok = self.scanner.peek();
        if (!token.TokenType.isAssignment(tok.tokenType)) {
            return exp;
        }

        if (declaration) {
            try self.consume(tt.ASSIGN);
        }

        // TODO: Check that expression has lvalue, error and return otherwise.
        // TODO: Set expression attributes.

        switch (assignTok.tokenType) {
            tt.ASSIGN => {
                self.consume(tt.ASSIGN) catch unreachable;
                var subExp = try self.parseSubExpression();
                errdefer subExp.t.destroy(self.alloc);
                // TODO: Set expression attributes.
            },
            tt.ADD_ASSIGN, tt.SUB_ASSIGN, tt.MUL_ASSIGN, tt.QUO_ASSIGN, tt.REM_ASSIGN, tt.LSH_ASSIGN, tt.RSH_ASSIGN, tt.AND_ASSIGN, tt.OR_ASSIGN, tt.XOR_ASSIGN => {
                // TODO: When generating IR, do not forget about signedness for tt.QUO_ASSIGN.
                // TODO: Create a single function for emitting all the necessary IR (including type conversions) depending on the operation.

                self.consume(assignTok.tokenType) catch unreachable;
                var rhsExp = try self.parseSubExpression();
                errdefer rhsExp.t.destroy(self.alloc);

                if (!exp.lt.isIntegral()) {
                    self.report(tok, reporter.Level.ERROR, "{s} is allowed only for integral types, not for {s}", .{ assignTok.symbol, exp.lt.str() }, true, true);
                    return SyntaxError.TypeError;
                }

                if (!rhsExp.t.isIntegral()) {
                    self.report(tok, reporter.Level.ERROR, "{s} is allowed only for integral types, not for {s}", .{ assignTok.symbol, exp.lt.str() }, true, true);
                    return SyntaxError.TypeError;
                }

                // TODO: Set expression attributes.
                exp.semiMustFollow = true;
            },
            // Assignments:tt.LAND_ASSIGN and tt.LOR_ASSIGN are special due to the fact that they are lazily evaluated.
            tt.LAND_ASSIGN, tt.LOR_ASSIGN => {
                // TODO: Create a single function for emitting all the necessary IR (including type conversions) for the lazy operations.
                self.consume(tt.LAND_ASSIGN) catch unreachable;
                if (!exp.lt.equals(types.Type{ .simple = types.SimpleType.BOOL })) {
                    self.report(tok, reporter.Level.ERROR, "{s} is allowed only for booleans, not for {s}", .{ assignTok.symbol, exp.lt.str() }, true, true);
                    return SyntaxError.TypeError;
                }

                var rhsExp = try self.parseSubExpression();
                errdefer rhsExp.t.destroy(self.alloc);

                if (!rhsExp.t.equals(types.Type{ .simple = types.SimpleType.BOOL })) {
                    self.report(tok, reporter.Level.ERROR, "{s} is allowed only for booleans, not for {s}", .{ assignTok.symbol, exp.lt.str() }, true, true);
                    return SyntaxError.TypeError;
                }

                // TODO: Set expression attributes.
            },
            else => @panic("ICE: exhausted all the possible assignments"),
        }

        return exp;
    }

    // Defines an identifier in te currently open scope. If a symbol with the same identifier
    // is already defined in the current scope, error is logged instead.
    fn define(self: *Parser, ident: []const u8, t: *types.Type, at: token.Token) SyntaxError!void {
        if (self.st.defined(ident)) {
            var prevDef = self.st.get(ident).?;
            self.report(
                at,
                reporter.Level.ERROR,
                "'{s}' already defined at {d}:{d}",
                .{ ident, prevDef.location.sourceLoc.line, prevDef.location.sourceLoc.column },
                true,
                true,
            );
            return SyntaxError.TypeError;
        }

        self.st.insert(symbols.Symbol{
            .name = ident,
            .llvmName = "TODO",
            .location = at,
            .t = t,
        }) catch unreachable;

        return;
    }

    // E^2
    fn parseTernaryExpression(self: *Parser) SyntaxError!Expression {
        // exp is potentially a boolen condition for the ternary operator.
        var exp = try self.parseLogicExpressions();
        errdefer exp.t.destroy(self.alloc);

        var tok = self.scanner.peek();
        if (tok.tokenType != tt.QUESTION_MARK and tok.tokenType != tt.D_QUESTION_MARK) {
            return exp;
        }

        if (!exp.t.equals(types.Type{ .simple = types.SimpleType.BOOL })) {
            self.report(tok, reporter.Level.ERROR, "non-boolean condition in if statement", .{}, true, true);
            // We can try and continue with the parsing.
        }

        return switch (tok.tokenType) {
            tt.D_QUESTION_MARK => blk: {
                self.consume(tt.D_QUESTION_MARK) catch unreachable;
                // TODO: What's selection bool?

                var exp2 = try self.parseExpression();
                errdefer exp2.t.destroy(self.alloc);

                try self.consume(tt.COLON);

                var exp3 = try self.parseTernaryExpression();
                errdefer exp3.t.destroy(self.alloc);

                // TODO: exp2.t and exp3.t must have common supertype, TypeError otherwise.
                if (exp2.hasLValue and exp3.hasLValue and exp2.lt.equals(exp3.lt.*)) {
                    if (!exp2.lt.equals(types.Type{ .simple = types.SimpleType.UNIT })) {}
                }

                // TODO: Set expression attributes.
                break :blk exp;
            },
            tt.QUESTION_MARK => blk: {
                self.consume(tt.QUESTION_MARK) catch unreachable;

                var exp2 = try self.parseExpression();
                errdefer exp2.t.destroy(self.alloc);

                try self.consume(tt.COLON);

                var exp3 = try self.parseTernaryExpression();
                errdefer exp3.t.destroy(self.alloc);

                // TODO: exp2.t and exp3.t must have common supertype, TypeError otherwise.
                if (exp2.hasLValue and exp3.hasLValue and exp2.lt.equals(exp3.lt.*)) {
                    if (!exp2.lt.equals(types.Type{ .simple = types.SimpleType.UNIT })) {}
                }

                break :blk exp;
            },
            else => unreachable,
        };
    }

    // E^3, binary and, or, ||, &&
    fn parseLogicExpressions(self: *Parser) SyntaxError!Expression {
        var exp = try self.parseNot();
        errdefer exp.t.destroy(self.alloc);

        var tok = self.scanner.peek();
        switch (tok.tokenType) {
            // TODO: add 'and' and 'or' tokens.
            tt.B_OR, tt.B_AND, tt.LAND, tt.LOR => {},
            else => return exp,
        }

        if (!exp.t.equals(types.Type{ .simple = types.SimpleType.BOOL })) {
            self.report(tok, reporter.Level.ERROR, "non-boolean condition in if statement", .{}, true, true);
            // We can try and continue with the parsing.
        }

        // TODO: Figure this part out.
        while (true) {
            tok = self.scanner.peek();
            switch (tok.tokenType) {
                tt.LAND => {},
                tt.LOR => {},
                tt.B_AND => {},
                tt.B_OR => {},
                else => return exp,
            }
        }
    }

    // E^4, unary not
    fn parseNot(self: *Parser) SyntaxError!Expression {
        const tok = self.scanner.peek();
        if (tok.tokenType == tt.NOT) {
            self.consume(tt.NOT) catch unreachable;

            var exp = try self.parseNot();
            errdefer exp.t.destroy(self.alloc);

            if (!exp.t.equals(types.Type{ .simple = types.SimpleType.BOOL })) {
                self.report(tok, reporter.Level.ERROR, "can negate only booleans", .{}, true, true);
                // We can try and continue with the parsing.
            }
            return exp;
        }

        return self.parseRelationalExpression();
    }

    // E^5, ==, !=, >, <, >=, =<
    fn parseRelationalExpression(self: *Parser) SyntaxError!Expression {
        var xExp = try self.parseArithmeticExpression();
        errdefer xExp.t.destroy(self.alloc);

        if (!tt.isRelational(self.scanner.peek().tokenType)) {
            return xExp;
        }

        var accResult: []const u8 = ""; // Accumulated result of all the comparisons together.
        var result: []const u8 = ""; // Current result.
        while (tt.isRelational(self.scanner.peek().tokenType)) {
            var op = self.scanner.next();
            var yExp = try self.parseArithmeticExpression();
            errdefer yExp.t.destroy(self.alloc);

            // TODO: Get supertype of xExp and yExp and store it in t
            // If there is no common supertype, throw TypeError
            var t: *types.Type = undefined;

            var valX: ?[]const u8 = null;
            _ = valX;
            var valY: ?[]const u8 = null;
            _ = valY;

            switch (t.*) {
                types.TypeTag.simple => |simpleType| {
                    if (simpleType == types.SimpleType.UNIT) {
                        switch (op.tokenType) {
                            tt.EQL => result = "1",
                            tt.NEQ => result = "0",
                            // Even though we hit type error, we can continue with the parsing.
                            else => self.report(op, reporter.Level.ERROR, "unit type can be only tested for equality and inequality", .{}, true, true),
                        }
                    } else if (simpleType == types.SimpleType.FLOAT or simpleType == types.SimpleType.DOUBLE) {
                        // TODO: Generate IR depending on the operation.
                    } else {
                        // Type must be one of [u64...u8, i64...i8, bool].
                        // TODO: Generate IR depending on the operation. Think about signedness.
                    }
                },
                types.TypeTag.constant => {
                    result = switch (op.tokenType) {
                        tt.EQL => if (xExp.t.constant.int == yExp.t.constant.int) "1" else "0",
                        tt.LT => if (xExp.t.constant.int < yExp.t.constant.int) "1" else "0",
                        tt.GT => if (xExp.t.constant.int > yExp.t.constant.int) "1" else "0",
                        tt.NEQ => if (xExp.t.constant.int != yExp.t.constant.int) "1" else "0",
                        tt.LEQ => if (xExp.t.constant.int <= yExp.t.constant.int) "1" else "0",
                        tt.GEQ => if (xExp.t.constant.int >= yExp.t.constant.int) "1" else "0",
                        else => unreachable,
                    };
                },
                types.TypeTag.array => {
                    // TODO: Generate IR for tt.EQL and tt.NEQ to just compare pointers.
                },
                // The only way we get here is if the common least supertype is function and that cannot happen.
                else => unreachable,
            }

            if (!std.mem.eql(u8, accResult, "")) {
                // Combine current result with previous results.
                // TODO: Generate IR and update `accResult`.
            } else {
                accResult = result;
            }

            // xExp = yExp.clone(); // And destroy yExp?
        }

        // TODO: Set expression attributes: type to bool, rValue available etc.
        return xExp;
    }

    // E^6, +, -, >>, <<, &, ^, |
    fn parseArithmeticExpression(self: *Parser) SyntaxError!Expression {
        var xExp = try self.parseUnaryOperators();
        errdefer xExp.t.destroy(self.alloc);

        var tok = self.scanner.peek();
        if (!tt.isLowerPrioArithmetic(tok.tokenType)) {
            return xExp;
        }

        if (xExp.t.isArray() or xExp.t.equals(types.Type{ .simple = types.SimpleType.UNIT })) {
            self.report(tok, reporter.Level.ERROR, "cannot perform '{s}' on '{s}'", .{ tok.symbol, xExp.t.str() }, true, true);
            // TODO: Does it make sense in this case to continue parsing?
            return SyntaxError.TypeError;
        }

        var result = "";
        _ = result;
        while (tt.isLowerPrioArithmetic(self.scanner.peek().tokenType)) {
            var op = self.scanner.next();

            var yExp = try self.parseUnaryOperators();
            errdefer yExp.t.destroy(self.alloc);

            if (yExp.t.isArray() or yExp.t.equals(types.Type{ .simple = types.SimpleType.UNIT })) {
                self.report(op, reporter.Level.ERROR, "cannot perform '{s}' on '{s}'", .{ tok.symbol, xExp.t.str() }, true, true);
                // TODO: Does it make sense in this case to continue parsing?
                return SyntaxError.TypeError;
            }

            // TODO: Get supertype of xExp and yExp and store it in t
            // If there is no common supertype, throw TypeError
            var t: *types.Type = undefined;

            if (t.equals(types.Type{ .simple = types.SimpleType.BOOL }) and op.tokenType != tt.B_AND and op.tokenType != tt.B_OR and op.tokenType != tt.B_XOR) {
                self.report(op, reporter.Level.ERROR, "cannot perform '{s}' on '{s}'", .{ tok.symbol, t.str() }, true, true);
                // TODO: Does it make sense in this case to continue parsing?
                return SyntaxError.TypeError;
            }

            if ((t.equals(types.Type{ .simple = types.SimpleType.DOUBLE }) or t.equals(types.Type{ .simple = types.SimpleType.FLOAT })) and (op.tokenType != tt.ADD or op.tokenType != tt.SUB)) {
                self.report(op, reporter.Level.ERROR, "cannot perform '{s}' on '{s}'", .{ tok.symbol, t.str() }, true, true);
                // TODO: Does it make sense in this case to continue parsing?
                return SyntaxError.TypeError;
            }

            // TODO: This fails for `i32 main() { 5 + 3; }` since we've not implemented the finding of common supertype,
            // therefore `t` is undefined.
            if (t.isConstant()) {
                t.constant.int = switch (op.tokenType) {
                    tt.ADD => xExp.t.constant.int + yExp.t.constant.int,
                    tt.SUB => xExp.t.constant.int - yExp.t.constant.int,
                    // TODO: i128 << i128 and i128 >> i128 overflows, figure out wrapping around?
                    // compiler error: error: expected type 'u7', found 'i128'
                    // tt.B_RSH => xExp.t.constant.int >> yExp.t.constant.int,
                    // tt.B_LSH => xExp.t.constant.int << yExp.t.constant.int,
                    tt.B_AND => xExp.t.constant.int & yExp.t.constant.int,
                    tt.B_OR => xExp.t.constant.int | yExp.t.constant.int,
                    tt.B_XOR => xExp.t.constant.int ^ yExp.t.constant.int,
                    else => unreachable,
                };
                // TODO: Set expression attributes.
            } else {
                var xVal = "";
                var yVal = "";
                _ = xVal;
                _ = yVal;
                // TODO: Type conversion for xVal and yVal
                // TODO: Emit IR depending on the operation and expression type.
                // TODO: Set expression attributes.
            }
        }
        // TODO: Set expression attributes.
        return xExp;
    }

    // E^7, -, !
    fn parseUnaryOperators(self: *Parser) SyntaxError!Expression {
        // TODO: handle exps (type checking)
        const tok = self.scanner.peek();
        return switch (tok.tokenType) {
            tt.SUB => {
                self.consume(tt.SUB) catch unreachable;

                var exp = try self.parseUnaryOperators();
                errdefer exp.t.destroy(self.alloc);

                if (!exp.t.isIntegral() or exp.t.equals(types.Type{ .simple = types.SimpleType.U64 })) {
                    self.report(tok, reporter.Level.ERROR, "cannot perform unary minus on '{s}'", .{exp.t.str()}, true, true);
                    // TODO: Does it make sense in this case to continue parsing?
                    return SyntaxError.TypeError;
                }

                if (exp.t.isConstant()) {
                    exp.t.constant.int = -exp.t.constant.int;
                } else {
                    // TODO: Emit IR depending on the operation and expression type.
                    // Watch out, this is more convoluted - unary minus can be done on u8, u16 and u32,
                    // resulting in i16, i32 and i64, respectively. Unary minus cannot be applied on u64
                    // since there is no i128.
                }
                return exp;
            },
            tt.NEG => {
                self.consume(tt.NEG) catch unreachable;

                var exp = try self.parseUnaryOperators();
                errdefer exp.t.destroy(self.alloc);

                if (!exp.t.isIntegral()) {
                    self.report(tok, reporter.Level.ERROR, "cannot perform bitwise not on '{s}'", .{exp.t.str()}, true, true);
                    // TODO: Does it make sense in this case to continue parsing?
                    return SyntaxError.TypeError;
                }

                if (exp.t.isConstant()) {
                    exp.t.constant.int = ~exp.t.constant.int;
                } else {
                    // TODO: Emit IR depending on the operation and expression type.
                    // Watch out, this is more convoluted - unary minus can be done on u8, u16 and u32,
                    // resulting in i16, i32 and i64, respectively. Unary minus cannot be applied on u64
                    // since there is no i128.
                }
                return exp;
            },
            else => {
                var exp = try self.parseArithmeticExpressionsLower();
                errdefer exp.t.destroy(self.alloc);
                return exp;
            },
        };
    }

    // E^8, *, /, %
    fn parseArithmeticExpressionsLower(self: *Parser) SyntaxError!Expression {
        var xExp = try self.parseArrayIndexingAndPrefixExpressions();
        errdefer xExp.t.destroy(self.alloc);

        var tok = self.scanner.peek();
        if (!tt.isHigherPrioArithmetic(tok.tokenType)) {
            return xExp;
        }

        if (!xExp.t.isArray() or xExp.t.equals(types.Type{ .simple = types.SimpleType.UNIT })) {
            self.report(tok, reporter.Level.ERROR, "cannot perform '{s}' on '{s}'", .{ tok.symbol, xExp.t.str() }, true, true);
            // TODO: Does it make sense in this case to continue parsing?
            return SyntaxError.TypeError;
        }

        var result: []const u8 = "";
        _ = result;
        while (tt.isHigherPrioArithmetic(self.scanner.peek().tokenType)) {
            var op = self.scanner.next();
            var yExp = try self.parseArrayIndexingAndPrefixExpressions();
            errdefer yExp.t.destroy(self.alloc);

            if (!yExp.t.isArray() or yExp.t.equals(types.Type{ .simple = types.SimpleType.UNIT })) {
                self.report(tok, reporter.Level.ERROR, "cannot perform '{s}' on '{s}'", .{ op.symbol, yExp.t.str() }, true, true);
                // TODO: Does it make sense in this case to continue parsing?
                return SyntaxError.TypeError;
            }

            // TODO: Get supertype of xExp and yExp and store it in t
            // If there is no common supertype, throw TypeError
            var t: *types.Type = undefined;

            if (t.isConstant()) {
                t.constant.int = switch (op.tokenType) {
                    tt.MUL => xExp.t.constant.int * yExp.t.constant.int,
                    tt.QUO => if (yExp.t.constant.int == 0) blk: {
                        self.report(tok, reporter.Level.ERROR, "cannot divide by 0", .{}, true, true);
                        // Instead of dividing, just return dummy value 1 to continue with the parsing.
                        break :blk 1;
                    } else @divExact(xExp.t.constant.int, yExp.t.constant.int),
                    tt.REM => if (yExp.t.constant.int == 0) blk: {
                        self.report(tok, reporter.Level.ERROR, "cannot compute remainder after dividing by 0", .{}, true, true);
                        // Instead of dividing, just return dummy value 1 to continue with the parsing.
                        break :blk 1;
                    } else @rem(xExp.t.constant.int, yExp.t.constant.int),
                    else => unreachable,
                };
            } else {
                // TODO: Emit IR depending on the operation and expression type.
                // Watch out, this is more convoluted - unary minus can be done on u8, u16 and u32,
                // resulting in i16, i32 and i64, respectively. Unary minus cannot be applied on u64
                // since there is no i128.
            }
        }
        return xExp;
    }

    // E^9, array indexing and ++, --, #
    fn parseArrayIndexingAndPrefixExpressions(self: *Parser) SyntaxError!Expression {
        var exp = try self.parseI();
        errdefer exp.t.destroy(self.alloc);

        const tok = self.scanner.peek();
        while (true) {
            switch (tok.tokenType) {
                tt.INC, tt.DEC => {
                    self.consume(tok.tokenType) catch unreachable;

                    // TODO: Do all the type conversions.

                    if (!exp.hasLValue) {
                        self.report(tok, reporter.Level.ERROR, "must be l-value in order to be incremented or decremented", .{}, true, true);
                        return SyntaxError.TypeError;
                    }

                    if (!exp.lt.isIntegral()) {
                        self.report(tok, reporter.Level.ERROR, "value of type '{s}' cannot be incremented or decremented", .{exp.t.str()}, true, true);
                        return SyntaxError.TypeError;
                    }

                    // TODO: Emit IR.
                    // TODO: Set expression attributes.
                },
                tt.HASH => {
                    self.consume(tt.HASH) catch unreachable;
                    if (!exp.hasLValue) {
                        self.report(tok, reporter.Level.ERROR, "must be l-value in order to be dereferenced", .{}, true, true);
                        return SyntaxError.TypeError;
                    }
                    // TODO: Set expression attributes.
                },
                tt.LBRACK => {
                    self.consume(tt.LBRACK) catch unreachable;
                    if (!exp.t.isArray()) {
                        self.report(tok, reporter.Level.ERROR, "'{s}' cannot be indexed", .{exp.t.str()}, true, true);
                        return SyntaxError.TypeError;
                    }

                    if (self.scanner.peek().tokenType == tt.RBRACK) {
                        // Zero-dimensional array.
                        self.consume(tt.RBRACK) catch unreachable;
                        if (exp.t.array.dimensions != 0) {
                            self.report(tok, reporter.Level.ERROR, "indexing to array must be {d}-dimensional instead of 0-dimensional", .{exp.t.array.dimensions}, true, true);
                            return SyntaxError.TypeError;
                        }
                        // TODO: Set expression attributes.
                        continue;
                    }

                    var indexes = std.ArrayList([]const u8).init(self.alloc);
                    var next = self.scanner.peek();
                    while (next.tokenType != tt.RBRACK) {
                        var iExp = try self.parseExpression();
                        errdefer iExp.t.destroy(self.alloc);

                        if (iExp.t.equals(types.Type{ .simple = types.SimpleType.I64 }) or iExp.t.equals(types.Type{ .simple = types.SimpleType.U64 })) {
                            indexes.append(iExp.rValue) catch unreachable;
                        } else {
                            // TODO: Convert iExp to i64
                        }

                        next = self.scanner.peek();
                        if (next.tokenType == tt.COMMA) {
                            self.consume(tt.COMMA) catch unreachable;

                            next = self.scanner.peek();
                            if (next.tokenType == tt.RBRACK) {
                                self.report(next, reporter.Level.ERROR, "expected index but found '{s}' instead", .{next.symbol}, true, true);
                                return SyntaxError.TypeError;
                            }
                        } else {
                            try self.consume(tt.RBRACK);
                            break;
                        }
                    }

                    if (indexes.items.len != exp.t.array.dimensions) {
                        self.report(next, reporter.Level.ERROR, "cannot index {d}-dimensional array with {d}-dimensional index", .{ exp.t.array.dimensions, indexes.items.len }, true, true);
                        return SyntaxError.TypeError;
                    }

                    if (exp.t.equals(types.Type{ .simple = types.SimpleType.UNIT })) {
                        // TODO: Set expression attributes.
                        continue;
                    }

                    // TODO: Emit IR with pointer arithmetic.
                    // TODO: Set expression attributes.
                },
                else => return exp,
            }
        }
    }

    fn parseI(self: *Parser) SyntaxError!Expression {
        var next = self.scanner.peek();
        switch (next.tokenType) {
            tt.LBRACE => {
                return self.parseBody();
            },
            tt.LPAREN => {
                // The pre-prepared expression is not needed.

                self.consume(tt.LPAREN) catch unreachable;

                var exp = try self.parseExpression();
                errdefer exp.t.destroy(self.alloc);

                exp.semiMustFollow = true;
                try self.consume(tt.RPAREN);
                return exp;
            },
            tt.IDENT => {
                // The pre-prepared expression is not needed.

                const ident = self.consumeGet(tt.IDENT) catch unreachable;
                if (self.st.functionDefined(ident.symbol)) {
                    return try self.parseFunctionCall(ident);
                } else {
                    var s = self.st.get(ident.symbol) orelse {
                        self.report(next, reporter.Level.ERROR, "no variable '{s}' is defined", .{next.symbol}, true, true);
                        return SyntaxError.TypeError;
                    };

                    if (s.t.isUnit()) {
                        return Expression{ .t = s.t, .hasLValue = true };
                    }
                    // TODO: Set expression attributes.
                    return Expression{};
                }
            },
            tt.C_INT => {
                const int = self.consumeGet(tt.C_INT) catch unreachable;
                var value = std.fmt.parseInt(i128, int.symbol, 0) catch unreachable;

                var exp = Expression{ .t = try self.alloc.create(types.Type) };
                exp.t.* = types.Type{ .constant = types.Constant{ .int = value } };
                return exp;
            },
            tt.C_FLOAT => {
                const int = self.consumeGet(tt.C_INT) catch unreachable;
                var value = std.fmt.parseFloat(f64, int.symbol) catch unreachable;

                var exp = Expression{ .t = try self.alloc.create(types.Type) };
                exp.t.* = types.Type{ .constant = types.Constant{ .float = value } };
                return exp;
            },
            tt.C_NULL => {
                self.consume(tt.C_NULL) catch unreachable;

                var exp = Expression{ .t = try self.alloc.create(types.Type) };
                exp.t.* = types.Type{ .array = types.Array{} };
                return exp;
            },
            tt.C_BOOL => {
                const boolean = self.consumeGet(tt.C_BOOL) catch unreachable;
                var value = if (std.mem.eql(u8, boolean.symbol, "true")) "1" else if (std.mem.eql(u8, boolean.symbol, "false")) "0" else unreachable;

                var exp = Expression{ .t = try self.alloc.create(types.Type) };
                exp.t.* = types.Type{ .simple = types.SimpleType.BOOL };
                exp.rValue = value;
                return exp;
            },
            tt.AT => {
                self.consume(tt.AT) catch unreachable;
                if (types.startsType(self.scanner.peek().symbol)) {
                    // We're dealing with explicit type conversion.
                    var newT = try self.parseType();
                    errdefer newT.destroy(self.alloc);

                    // TODO: Figure out arrays (starts on line 1892).
                    var cExp = try self.parseI();
                    errdefer cExp.t.destroy(self.alloc);

                    // TODO: Do the type conversion.
                    // TODO: Set expression attributes.
                    return cExp;
                }
                // We're dealing with builtin functions.
                const ident = try self.consume(tt.IDENT);
                _ = ident;
                // TODO: Emit IR for calling builtin functions.
                // TODO: Set expression attributes.
                return Expression{};
            },
            tt.C_CHAR => {
                const char = self.consumeGet(tt.C_CHAR) catch unreachable;
                if (char.symbol[0] < 0 or char.symbol[0] > 255) {
                    self.report(char, reporter.Level.ERROR, "'{s}' is not ASCII-encoded", .{char.symbol}, true, true);
                    return SyntaxError.TypeError;
                }
                var exp = Expression{ .t = try self.alloc.create(types.Type) };
                exp.t.* = types.Type{ .simple = types.SimpleType.U8 };
                exp.rValue = char.symbol;
                return exp;
            },
            tt.C_STRING => {
                const string = self.consumeGet(tt.C_STRING) catch unreachable;
                _ = string;
                // TODO: Emit IR to store the string.
                var exp = Expression{ .t = try self.alloc.create(types.Type) };
                exp.t.* = types.Type{ .array = types.Array{ .dimensions = 1 } };
                exp.t.array.ofType = self.alloc.create(types.Type) catch unreachable;
                exp.t.array.ofType.* = types.Type{ .simple = types.SimpleType.U8 };
                // TODO: Set expression attributes.
                return exp;
            },
            else => {
                self.report(next, reporter.Level.ERROR, "expected '{s}', '(', identifier, function call or a constant value but got '{s}' instead", .{ tt.LBRACE.str(), next.tokenType.str() }, true, true);
                return SyntaxError.UnexpectedToken;
            },
        }
        unreachable;
    }

    fn parseFunctionCall(self: *Parser, ident: token.Token) SyntaxError!Expression {
        try self.consume(tt.LPAREN);

        var funcSymbol = self.st.get(ident.symbol) orelse unreachable;

        for (0..funcSymbol.t.func.args.items.len, funcSymbol.t.func.args.items) |i, arg| {
            var exp = try self.parseExpression();
            if (!exp.t.equals(types.Type{ .simple = types.SimpleType.UNIT })) {
                // TODO: Prepare LLVM argument by converting types if necessary
            } else if (!arg.t.equals(types.Type{ .simple = types.SimpleType.UNIT })) {
                self.report(arg.location, reporter.Level.ERROR, "argument '{s}' must be of type '{s}' instead of '{s}'", .{ arg.name, types.SimpleType.UNIT.str(), arg.t.str() }, true, true);
                return SyntaxError.TypeError;
            }

            if (i != funcSymbol.t.func.args.items.len - 1) try self.consume(tt.COMMA);
        }

        try self.consume(tt.RPAREN);
        // TODO: Emit IR.
        // TODO: Set expression attributes.

        return Expression{};
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
        // TODO: When reporting, denote in what stage the error happened - scanning, parsing, typechecking, code generation?
        const msg = std.fmt.allocPrint(self.alloc, fmt, args) catch unreachable;
        defer self.alloc.free(msg);

        if (level == reporter.Level.ERROR) {
            self.emit = false;
        }

        if (self.rep) |rep| {
            if (space) {
                rep.space();
                rep.space();
            }

            rep.report(tok, level, msg, showLine);
        }
    }
};
