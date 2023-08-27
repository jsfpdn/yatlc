const std = @import("std");

const types = @import("types.zig");
const scanner = @import("scanner.zig");
const token = @import("token.zig");
const reporter = @import("reporter.zig");
const symbols = @import("symbols.zig");

const tt = token.TokenType;

pub const SyntaxError = error{ UnexpectedToken, TypeError, OutOfMemory, RecoverableError };

pub const Expression = struct {
    t: ?*types.Type = null,
    lt: ?*types.Type = null,

    rValue: []const u8 = "",

    hasLValue: bool = false,
    lValueComputed: bool = false,
    lValue: []const u8 = "",

    semiMustFollow: bool = false,
    endsWithReturn: bool = false,

    pub fn destroy(self: Expression, alloc: std.mem.Allocator) void {
        if (self.t) |t| t.destroy(alloc);
        if (self.lt) |lt| lt.destroy(alloc);
    }

    pub fn clone(self: Expression, alloc: std.mem.Allocator) Expression {
        return Expression{
            .t = if (self.t) |t| t.clone(alloc) else null,
            .lt = if (self.lt) |lt| lt.clone(alloc) else null,
            .rValue = self.rValue,
            .hasLValue = self.hasLValue,
            .lValueComputed = self.lValueComputed,
            .lValue = self.lValue,
            .semiMustFollow = self.semiMustFollow,
            .endsWithReturn = self.endsWithReturn,
        };
    }
};

pub const Parser = struct {
    scanner: scanner.Scanner,
    rep: ?reporter.Reporter,

    alloc: std.mem.Allocator,

    st: symbols.SymbolTable,

    emit: bool,

    breakStack: std.ArrayList([]const u8),
    contStack: std.ArrayList([]const u8),

    returnType: ?*types.Type = null,

    // TODO:
    // * IR emitter
    // * arithmetic expressions (chaining operators)
    // * set up returnType correctly when parsing function definition.
    // * type conversions
    // * constant folding
    // * handle comments properly
    // * global variables

    pub fn init(s: scanner.Scanner, r: ?reporter.Reporter, alloc: std.mem.Allocator) Parser {
        return .{
            .scanner = s,
            .rep = r,
            .alloc = alloc,
            .st = symbols.SymbolTable.init(alloc),
            .emit = true,
            .breakStack = std.ArrayList([]const u8).init(alloc),
            .contStack = std.ArrayList([]const u8).init(alloc),
        };
    }

    pub fn deinit(self: *Parser) void {
        self.st.deinit();
        self.breakStack.deinit();
        self.contStack.deinit();
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
                types.TypeTag.func => {
                    if (!s.defined) {
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
                    .defined = false,
                };

                // Try to parse the function body if it is a function definition.
                next = self.scanner.peek();
                if (next.tokenType == tt.LBRACE) {
                    // Open a new scope just for the function arguments. This way,
                    // arguments can be shadowed in the function body.
                    try self.st.open();

                    // Set the expected type of the returned value.
                    self.returnType = t;

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
                    // TODO: Expression is destroyed immediately to prevent memory leak.
                    // Once expressions are better handled, this should probably be deleted:
                    exp.destroy(self.alloc);

                    // Close the scope just for the function arguments.
                    self.st.close();

                    fs.defined = true;
                } else {
                    try self.consume(tt.SEMICOLON);
                }

                // Check that if there's a symbol with the same name, it must be a function with identical
                // argument types that has been declared but not defined yet.
                if (self.st.get(ident.symbol)) |s| {
                    if (s.defined) {
                        self.report(fs.location, reporter.Level.ERROR, "function '{s}' is already defined", .{fs.name}, true, true);
                        self.report(s.location, reporter.Level.NOTE, "'{s}' first defined here", .{fs.name}, false, false);
                        return SyntaxError.TypeError;
                    }

                    switch (s.t.*) {
                        types.TypeTag.func => |otherFunc| {
                            ft.func.defines(otherFunc) catch |err| {
                                switch (err) {
                                    types.Func.DefinitionErrors.ArgTypeMismatch => {
                                        if (fs.defined) {
                                            self.report(fs.location, reporter.Level.ERROR, "definition of '{s}' does not match its declaration", .{fs.name}, true, true);
                                        } else {
                                            self.report(fs.location, reporter.Level.ERROR, "redaclaration of '{s}' does not match the former declaration", .{fs.name}, true, true);
                                        }
                                        self.report(s.location, reporter.Level.NOTE, "'{s}' first declared here", .{fs.name}, false, false);
                                        // TODO: Make error reporting better - show where the mismatch is.
                                    },
                                }

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
                self.report(
                    next,
                    reporter.Level.ERROR,
                    "expected either function declaration, definition or global variable definiton",
                    .{},
                    true,
                    true,
                );
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
                .defined = true,
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
        // TODO: Fix leaked memory here.
        var exp: ?Expression = null;
        errdefer if (exp) |e| e.destroy(self.alloc);

        var tok = self.scanner.peek();
        if (tok.tokenType == tt.RBRACE) {
            return Expression{
                .t = types.SimpleType.create(self.alloc, types.SimpleType.UNIT),
                .lt = types.SimpleType.create(self.alloc, types.SimpleType.UNIT),
                .endsWithReturn = false,
                .semiMustFollow = false,
                .lValueComputed = false,
            };
        }

        while (!endParseExpression(tok.tokenType)) {
            if (tok.tokenType != tt.SEMICOLON) {
                if (exp) |e| e.destroy(self.alloc);

                exp = switch (tok.tokenType) {
                    tt.WHILE => try self.parseWhile(),
                    tt.DO => try self.parseDoWhile(),
                    tt.FOR => try self.parseFor(),
                    tt.IF => try self.parseIf(),
                    tt.RETURN => try self.parseReturn(),
                    tt.BREAK => try self.parseBreak(),
                    tt.CONTINUE => try self.parseContinue(),
                    tt.SEMICOLON => unreachable,
                    else => try self.parseSubExpression(),
                };
            }

            tok = self.scanner.peek();
            if (tok.tokenType == tt.SEMICOLON) {
                try self.consume(tt.SEMICOLON);
                tok = self.scanner.peek();
                continue;
            }

            if (exp.?.semiMustFollow and !endParseExpression(tok.tokenType)) {
                self.report(tok, reporter.Level.ERROR, "expected ';' but got '{s}' instead", .{tok.str()}, true, true);
                return SyntaxError.TypeError;
            }
        }

        return exp.?;
    }

    fn endParseExpression(tokenType: tt) bool {
        const enders = [_]tt{ tt.EOF, tt.RBRACE, tt.RBRACK, tt.RPAREN, tt.COMMA, tt.COLON };

        for (enders) |ender| if (ender == tokenType) return true;

        return false;
    }

    fn parseIf(self: *Parser) SyntaxError!Expression {
        try self.consume(tt.IF);
        const lparen = try self.consumeGet(tt.LPAREN);

        var ifExp = try self.parseExpression();
        defer ifExp.destroy(self.alloc);

        if (!ifExp.t.?.isBool()) {
            self.report(lparen, reporter.Level.ERROR, "non-boolean condition in if statement", .{}, true, true);
            return SyntaxError.TypeError;
        }

        try self.consume(tt.RPAREN);

        var body = try self.parseBody();
        defer body.destroy(self.alloc);

        if (self.scanner.peek().tokenType != tt.ELSE) {
            // TODO: implement this.
            return Expression{
                .t = types.SimpleType.create(self.alloc, types.SimpleType.UNIT),
                .endsWithReturn = false,
                .semiMustFollow = false,
                .hasLValue = false,
            };
        }

        var b = body.endsWithReturn;
        var c = false;

        self.consume(tt.ELSE) catch unreachable;

        if (self.scanner.peek().tokenType == tt.IF) {
            var anotherIfExp = try self.parseIf();
            defer anotherIfExp.destroy(self.alloc);

            c = anotherIfExp.endsWithReturn;
        } else {
            var anotherBody = try self.parseBody();
            defer anotherBody.destroy(self.alloc);

            c = anotherBody.endsWithReturn;
        }

        if (b and c) {
            return Expression{
                .t = types.SimpleType.create(self.alloc, types.SimpleType.UNIT),
                .endsWithReturn = true,
                .semiMustFollow = false,
                .hasLValue = false,
            };
        }

        if (!b) {
            if (!c) {
                // TODO: Emit IR.
            }
            // TODO: Emit IR.
        }

        return Expression{
            .t = types.SimpleType.create(self.alloc, types.SimpleType.UNIT),
            .endsWithReturn = false,
            .semiMustFollow = false,
            .hasLValue = false,
        };
    }

    fn parseFor(self: *Parser) SyntaxError!Expression {
        // TODO: Decide whether to allow loops such as `for (,,) {...}`.
        self.st.open() catch unreachable;
        defer self.st.close();

        self.consume(tt.FOR) catch unreachable;
        try self.consume(tt.LPAREN);

        var initExp = try self.parseExpression();
        defer initExp.destroy(self.alloc);

        const comma = try self.consumeGet(tt.COMMA);

        var condExp = try self.parseExpression();
        defer condExp.destroy(self.alloc);
        if (!condExp.t.?.isBool()) {
            self.report(comma, reporter.Level.ERROR, "condition of for loop must be boolean", .{}, true, true);
            return SyntaxError.TypeError;
        }

        try self.consume(tt.COMMA);

        // Follows the increment (or 'step') block where the iterating variable gets incremented.
        // For this reason, the continue should in front of it.
        self.breakStack.append("TODO: label for the end of body") catch unreachable;
        defer _ = self.breakStack.pop();
        self.contStack.append("TODO: label for the beginning of the increment block") catch unreachable;
        defer _ = self.contStack.pop();

        var stepExp = try self.parseExpression();
        defer stepExp.destroy(self.alloc);

        try self.consume(tt.RPAREN);

        var body = try self.parseBody();
        defer body.destroy(self.alloc);

        return Expression{
            .t = types.SimpleType.create(self.alloc, types.SimpleType.UNIT),
            .semiMustFollow = false,
            .endsWithReturn = false,
            .hasLValue = false,
        };
    }

    fn parseWhile(self: *Parser) SyntaxError!Expression {
        self.consume(tt.WHILE) catch unreachable;
        const lparen = try self.consumeGet(tt.LPAREN);

        var exp = try self.parseExpression();
        defer exp.destroy(self.alloc);

        if (!exp.t.?.isBool()) {
            self.report(lparen, reporter.Level.ERROR, "condition of while loop must be boolean", .{}, true, true);
            return SyntaxError.TypeError;
        }

        try self.consume(tt.RPAREN);

        self.breakStack.append("TODO: label for the end of body") catch unreachable;
        defer _ = self.breakStack.pop();
        self.contStack.append("TODO: label for the beginning of condition") catch unreachable;
        defer _ = self.contStack.pop();

        var body = try self.parseBody();
        defer body.destroy(self.alloc);

        return Expression{
            .t = types.SimpleType.create(self.alloc, types.SimpleType.UNIT),
            .semiMustFollow = false,
            .endsWithReturn = false,
            .hasLValue = false,
        };
    }

    fn parseDoWhile(self: *Parser) SyntaxError!Expression {
        self.consume(tt.DO) catch unreachable;

        {
            // Lexical scope to leverage defers to pop labels immediately after parsing the body.
            self.breakStack.append("TODO: label for the end of body") catch unreachable;
            defer _ = self.breakStack.pop();
            self.contStack.append("TODO: label for the beginning of condition") catch unreachable;
            defer _ = self.contStack.pop();

            var body = try self.parseBody();
            defer body.destroy(self.alloc);
        }

        try self.consume(tt.WHILE);
        const lparen = try self.consumeGet(tt.LPAREN);

        var exp = try self.parseExpression();
        defer exp.destroy(self.alloc);
        if (!exp.t.?.isBool()) {
            self.report(lparen, reporter.Level.ERROR, "condition of do-while must be boolean", .{}, true, true);
            return SyntaxError.TypeError;
        }

        try self.consume(tt.RPAREN);
        return Expression{
            .t = types.SimpleType.create(self.alloc, types.SimpleType.UNIT),
            .semiMustFollow = false,
            .endsWithReturn = false,
            .hasLValue = false,
        };
    }

    fn parseBody(self: *Parser) SyntaxError!Expression {
        self.st.open() catch unreachable;
        defer self.st.close();
        try self.consume(tt.LBRACE);

        var exp = try self.parseExpression();
        errdefer exp.destroy(self.alloc);

        try self.consume(tt.RBRACE);

        return Expression{
            .t = exp.t,
            .lt = exp.lt,
            .rValue = exp.rValue,
            .hasLValue = exp.hasLValue,
            .lValue = exp.lValue,
            .semiMustFollow = false,
            .endsWithReturn = exp.endsWithReturn,
        };
    }

    fn parseReturn(self: *Parser) SyntaxError!Expression {
        const ret = self.consumeGet(tt.RETURN) catch unreachable;

        return switch (self.scanner.peek().tokenType) {
            tt.SEMICOLON, tt.RPAREN, tt.RBRACK, tt.RBRACE, tt.COMMA, tt.COLON => blk: {
                if (self.returnType) |rt| {
                    if (!rt.isUnit()) {
                        self.report(ret, reporter.Level.ERROR, "function must return value of type '{s}' instead of unit", .{rt.str()}, true, true);
                        return SyntaxError.TypeError;
                    }
                }

                break :blk Expression{
                    .t = types.SimpleType.create(self.alloc, types.SimpleType.UNIT),
                    .endsWithReturn = true,
                    .semiMustFollow = true,
                };
            },
            else => blk: {
                var retExp = try self.parseSubExpression();
                defer retExp.destroy(self.alloc);

                const value = self.convert(self.returnType.?, retExp.t.?, ConvMode.IMPLICIT, "TODO") catch |err| {
                    switch (err) {
                        ConvErr.Overflow => self.report(
                            ret,
                            reporter.Level.ERROR,
                            "cannot cast value of type {s} to {s} due to possible overflow",
                            .{ retExp.t.?.str(), self.returnType.?.str() },
                            true,
                            true,
                        ),
                        ConvErr.NoImplicit => self.report(
                            ret,
                            reporter.Level.ERROR,
                            "cannot cast returned value of type {s} to {s}",
                            .{ retExp.t.?.str(), self.returnType.?.str() },
                            true,
                            true,
                        ),
                    }

                    return SyntaxError.TypeError;
                };
                _ = value;

                // TODO: Try to convert the type of retExp to self.returnType and throw type error if
                // not convertible.

                break :blk Expression{
                    .t = types.SimpleType.create(self.alloc, types.SimpleType.UNIT),
                    .endsWithReturn = true,
                    .semiMustFollow = true,
                };
            },
        };
    }

    fn parseBreak(self: *Parser) SyntaxError!Expression {
        const br = self.consumeGet(tt.BREAK) catch unreachable;

        if (self.breakStack.items.len == 0) {
            self.report(br, reporter.Level.ERROR, "there is nothing to break out of", .{}, true, true);
            return SyntaxError.TypeError;
        }

        const label = self.breakStack.getLast();
        _ = label;

        // TODO: Emit `br $label`.
        return Expression{
            .t = types.SimpleType.create(self.alloc, types.SimpleType.UNIT),
            .semiMustFollow = true,
        };
    }

    fn parseContinue(self: *Parser) SyntaxError!Expression {
        const cont = self.consumeGet(tt.CONTINUE) catch unreachable;

        if (self.contStack.items.len == 0) {
            self.report(cont, reporter.Level.ERROR, "there is no loop to continue next iteration in", .{}, true, true);
            return SyntaxError.TypeError;
        }

        const label = self.contStack.getLast();
        _ = label;

        // TODO: Emit `br $label`.
        return Expression{
            .t = types.SimpleType.create(self.alloc, types.SimpleType.UNIT),
            .semiMustFollow = true,
        };
    }

    // E^1
    fn parseSubExpression(self: *Parser) SyntaxError!Expression {
        var exp = Expression{};
        errdefer exp.destroy(self.alloc);
        var declaration = true;

        var identTok: ?token.Token = null;
        var identType: ?*types.Type = null;

        var tok = self.scanner.peek();
        if (types.startsType(tok.symbol)) {
            // Current token under the cursor is either a simple type or an array type.
            // TODO: Be careful when typechecking when identType is UNIT.
            identType = try self.parseType();
            identTok = try self.consumeGet(tt.IDENT);

            try self.declare(identTok.?.symbol, identType.?, identTok.?, false);
            exp.semiMustFollow = true;
        } else {
            declaration = false;
            exp = try self.parseTernaryExpression();
        }

        var assignTok = self.scanner.peek();
        if (!token.TokenType.isAssignment(assignTok.tokenType)) {
            return exp;
        }

        // TODO: Check that expression has lvalue, error and return otherwise.

        switch (assignTok.tokenType) {
            tt.ASSIGN => {
                self.consume(tt.ASSIGN) catch unreachable;
                var subExp = try self.parseSubExpression();
                defer subExp.destroy(self.alloc);

                const value = self.convert(subExp.t.?, identType.?, ConvMode.IMPLICIT, "TODO") catch |err| {
                    switch (err) {
                        ConvErr.Overflow => self.report(
                            identTok.?,
                            reporter.Level.ERROR,
                            "cannot cast value of type {s} to {s} due to possible overflow",
                            .{ subExp.t.?.str(), identType.?.str() },
                            true,
                            true,
                        ),
                        ConvErr.NoImplicit => self.report(
                            identTok.?,
                            reporter.Level.ERROR,
                            "cannot cast returned value of type {s} to {s}",
                            .{ subExp.t.?.str(), identType.?.str() },
                            true,
                            true,
                        ),
                    }

                    return SyntaxError.TypeError;
                };
                _ = value;

                // TODO: Refactor this.
                exp.semiMustFollow = true;
                exp.t = subExp.t.?.clone(self.alloc);
                if (subExp.lt) |lt| exp.lt = lt.clone(self.alloc);
                exp.rValue = subExp.rValue;
                exp.hasLValue = subExp.hasLValue;
                exp.lValue = subExp.lValue;
                exp.semiMustFollow = true;
                exp.endsWithReturn = false;
            },
            tt.ADD_ASSIGN, tt.SUB_ASSIGN, tt.MUL_ASSIGN, tt.QUO_ASSIGN, tt.REM_ASSIGN, tt.LSH_ASSIGN, tt.RSH_ASSIGN, tt.AND_ASSIGN, tt.OR_ASSIGN, tt.XOR_ASSIGN => {
                // TODO: When generating IR, do not forget about signedness for tt.QUO_ASSIGN.
                // TODO: Create a single function for emitting all the necessary IR (including type conversions) depending on the operation.

                self.consume(assignTok.tokenType) catch unreachable;
                // TODO: Destroy type of either rhsExp or exp.
                var rhsExp = try self.parseSubExpression();
                defer rhsExp.destroy(self.alloc);

                // TODO: AND_ASSIGN, OR_ASSIGN and XOR_ASSIGN should pass when exp.lt is boolean
                if (!exp.lt.?.isIntegral()) {
                    self.report(tok, reporter.Level.ERROR, "{s} is allowed only for integral types, not for {s}", .{ assignTok.symbol, exp.lt.?.str() }, true, true);
                    return SyntaxError.TypeError;
                }

                if (!rhsExp.t.?.isIntegral()) {
                    self.report(tok, reporter.Level.ERROR, "{s} is allowed only for integral types, not for {s}", .{ assignTok.symbol, exp.lt.?.str() }, true, true);
                    return SyntaxError.TypeError;
                }

                const value = self.convert(rhsExp.t.?, exp.lt.?, ConvMode.IMPLICIT, "TODO") catch |err| {
                    switch (err) {
                        ConvErr.Overflow => self.report(
                            assignTok,
                            reporter.Level.ERROR,
                            "cannot cast value of type {s} to {s} due to possible overflow",
                            .{ rhsExp.t.?.str(), exp.t.?.str() },
                            true,
                            true,
                        ),
                        ConvErr.NoImplicit => self.report(
                            assignTok,
                            reporter.Level.ERROR,
                            "cannot cast value of type {s} to {s}",
                            .{ rhsExp.t.?.str(), exp.t.?.str() },
                            true,
                            true,
                        ),
                    }

                    return SyntaxError.TypeError;
                };
                _ = value;

                exp.semiMustFollow = true;
                exp.rValue = rhsExp.rValue;
                exp.hasLValue = true;
                exp.semiMustFollow = true;
                exp.endsWithReturn = false;
            },
            // Assignments:tt.LAND_ASSIGN and tt.LOR_ASSIGN are special due to the fact that they are lazily evaluated.
            tt.LAND_ASSIGN, tt.LOR_ASSIGN => {
                // TODO: Create a single function for emitting all the necessary IR (including type conversions) for the lazy operations.
                self.consume(tt.LAND_ASSIGN) catch unreachable;
                if (!exp.lt.?.equals(types.Type{ .simple = types.SimpleType.BOOL })) {
                    self.report(tok, reporter.Level.ERROR, "{s} is allowed only for booleans, not for {s}", .{ assignTok.symbol, exp.lt.?.str() }, true, true);
                    return SyntaxError.TypeError;
                }
                // TODO: Destroy type of either rhsExp or exp.
                var rhsExp = try self.parseSubExpression();
                errdefer rhsExp.destroy(self.alloc);

                if (!rhsExp.t.?.equals(types.Type{ .simple = types.SimpleType.BOOL })) {
                    self.report(tok, reporter.Level.ERROR, "{s} is allowed only for booleans, not for {s}", .{ assignTok.symbol, exp.lt.?.str() }, true, true);
                    return SyntaxError.TypeError;
                }

                exp.rValue = rhsExp.rValue;
                exp.hasLValue = true;
                exp.semiMustFollow = true;
                exp.endsWithReturn = false;
            },
            else => @panic("ICE: exhausted all the possible assignments"),
        }

        return exp;
    }

    // Defines an identifier in te currently open scope. If a symbol with the same identifier
    // is already defined in the current scope, error is logged instead.
    fn declare(self: *Parser, ident: []const u8, t: *types.Type, at: token.Token, defined: bool) SyntaxError!void {
        if (self.st.declared(ident)) {
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
            .defined = defined,
        }) catch unreachable;

        return;
    }

    // E^2
    fn parseTernaryExpression(self: *Parser) SyntaxError!Expression {
        // exp is potentially a boolen condition for the ternary operator.
        var exp = try self.parseLogicExpressions();
        errdefer exp.destroy(self.alloc);

        var tok = self.scanner.peek();
        if (tok.tokenType != tt.QUESTION_MARK and tok.tokenType != tt.D_QUESTION_MARK) {
            return exp;
        }

        if (!exp.t.?.equals(types.Type{ .simple = types.SimpleType.BOOL })) {
            self.report(tok, reporter.Level.ERROR, "non-boolean condition in a ternary operator statement", .{}, true, true);
            // We can try and continue with the parsing.
        }

        return switch (tok.tokenType) {
            tt.D_QUESTION_MARK => blk: {
                self.consume(tt.D_QUESTION_MARK) catch unreachable;
                // TODO: What's selection bool?

                var exp2 = try self.parseExpression();
                defer exp2.destroy(self.alloc);

                try self.consume(tt.COLON);

                var exp3 = try self.parseTernaryExpression();
                defer exp3.destroy(self.alloc);

                // TODO: Fix memory leak.
                var t: *types.Type = types.leastSupertype(self.alloc, exp2.t.?, exp3.t.?) orelse {
                    self.report(tok, reporter.Level.ERROR, "no common supertype for '{s}' and '{s}'", .{ exp2.t.?.str(), exp3.t.?.str() }, true, true);
                    return SyntaxError.TypeError;
                };

                // TODO: Continue here with the type checking.
                // TODO: exp2.t and exp3.t must have common supertype, TypeError otherwise.
                if (exp2.hasLValue and exp3.hasLValue and exp2.lt.?.equals(exp3.lt.?.*)) {
                    if (!exp2.lt.?.equals(types.Type{ .simple = types.SimpleType.UNIT })) {}
                }

                // Destroy the previous type and replace it with the new supertype.
                exp.destroy(self.alloc);
                exp.t = t;
                exp.lt = if (exp2.lt.?.equals(exp3.lt.?.*)) exp2.lt.?.clone(self.alloc) else null;
                // TODO: Expression attribute rValue: result of `select` instruction.
                exp.hasLValue = if (exp2.lt.?.equals(exp3.lt.?.*)) true else false;
                // TODO: exp.lValue = ...
                exp.semiMustFollow = true;
                exp.endsWithReturn = false;
                break :blk exp;
            },
            tt.QUESTION_MARK => blk: {
                self.consume(tt.QUESTION_MARK) catch unreachable;

                var exp2 = try self.parseExpression();
                defer exp2.destroy(self.alloc);

                try self.consume(tt.COLON);

                var exp3 = try self.parseTernaryExpression();
                defer exp3.destroy(self.alloc);

                var t: *types.Type = types.leastSupertype(self.alloc, exp2.t.?, exp3.t.?) orelse {
                    self.report(tok, reporter.Level.ERROR, "no common supertype for '{s}' and '{s}'", .{ exp2.t.?.str(), exp3.t.?.str() }, true, true);
                    return SyntaxError.TypeError;
                };

                // TODO: Continue here with the type checking.
                // TODO: exp2.t and exp3.t must have common supertype, TypeError otherwise.
                if (exp2.hasLValue and exp3.hasLValue and exp2.lt.?.equals(exp3.lt.?.*)) {
                    if (!exp2.lt.?.equals(types.Type{ .simple = types.SimpleType.UNIT })) {}
                }

                // Destroy the previous type and replace it with the new supertype.
                exp.destroy(self.alloc);
                exp.t = t;
                exp.lt = if (exp2.lt.?.equals(exp3.lt.?.*)) exp2.lt.?.clone(self.alloc) else null;
                // TODO: Expression attribute rValue: result of `phi` instruction.
                exp.hasLValue = if (exp2.lt.?.equals(exp3.lt.?.*)) true else false;
                // TODO: exp.lValue = ...
                exp.semiMustFollow = true;
                exp.endsWithReturn = false;

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

        if (!exp.t.?.equals(types.Type{ .simple = types.SimpleType.BOOL })) {
            self.report(tok, reporter.Level.ERROR, "cannot perform '{s}' on non-boolean values", .{tok.str()}, true, true);
            // We can try and continue with the parsing.
        }

        // TODO: Figure this part out.

        // Jan: This function body is incomplete, but the attributes will basically look like this:
        //      t = BOOL, rvalue = ..., hasLValue = false, semiMustFollow = true, endsWithReturn = false
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
            errdefer exp.destroy(self.alloc);

            if (!exp.t.?.equals(types.Type{ .simple = types.SimpleType.BOOL })) {
                self.report(tok, reporter.Level.ERROR, "can negate only booleans", .{}, true, true);
                // We can try and continue with the parsing.
            }

            exp.hasLValue = false;
            exp.semiMustFollow = true;
            exp.endsWithReturn = false;
            return exp;
        }

        return self.parseRelationalExpression();
    }

    // E^5, ==, !=, >, <, >=, =<
    fn parseRelationalExpression(self: *Parser) SyntaxError!Expression {
        var xExp = try self.parseArithmeticExpression();
        errdefer xExp.destroy(self.alloc);

        if (!tt.isRelational(self.scanner.peek().tokenType)) {
            return xExp;
        }

        var accResult: []const u8 = ""; // Accumulated result of all the comparisons together.
        var result: []const u8 = ""; // Current result.
        while (tt.isRelational(self.scanner.peek().tokenType)) {
            var op = self.scanner.next();
            var yExp = try self.parseArithmeticExpression();
            errdefer yExp.destroy(self.alloc);

            // TODO: Fix memory leak.
            var t: *types.Type = types.leastSupertype(self.alloc, xExp.t.?, yExp.t.?) orelse {
                self.report(op, reporter.Level.ERROR, "no common supertype for '{s}' and '{s}'", .{ xExp.t.?.str(), yExp.t.?.str() }, true, true);
                return SyntaxError.TypeError;
            };
            defer t.destroy(self.alloc);

            var valX: ?[]const u8 = null;
            _ = valX;
            var valY: ?[]const u8 = null;
            _ = valY;

            // TODO: Continue here with the type checking.
            // valX = self.convert(t, xExp.t.?, ...);
            // valY = self.convert(t, yExp.t.?, ...);

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
                    // TODO: The actual value of the constant is not stored in the type anymore
                    // but MUST be in the expression itself. Create a helper function that will do
                    // the appropriate comparisons and return the result, "0" or "1".
                    // result = switch (op.tokenType) {
                    //     tt.EQL => if (xExp.t.constant.int == yExp.t.constant.int) "1" else "0",
                    //     tt.LT => if (xExp.t.constant.int < yExp.t.constant.int) "1" else "0",
                    //     tt.GT => if (xExp.t.constant.int > yExp.t.constant.int) "1" else "0",
                    //     tt.NEQ => if (xExp.t.constant.int != yExp.t.constant.int) "1" else "0",
                    //     tt.LEQ => if (xExp.t.constant.int <= yExp.t.constant.int) "1" else "0",
                    //     tt.GEQ => if (xExp.t.constant.int >= yExp.t.constant.int) "1" else "0",
                    //     else => unreachable,
                    // };
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

            // Replace the type of the expression with the new supertype.
            xExp.destroy(self.alloc);
            xExp = yExp.clone(self.alloc);
            yExp.destroy(self.alloc);
        }

        xExp.t.?.destroy(self.alloc);
        xExp.t = types.SimpleType.create(self.alloc, types.SimpleType.BOOL);
        // xExp.rValue = something to do with the accResult
        xExp.hasLValue = false;
        xExp.semiMustFollow = true;
        xExp.endsWithReturn = false;
        return xExp;
    }

    // E^6, +, -, >>, <<, &, ^, |
    fn parseArithmeticExpression(self: *Parser) SyntaxError!Expression {
        var xExp = try self.parseUnaryOperators();
        errdefer xExp.destroy(self.alloc);

        var tok = self.scanner.peek();
        if (!tt.isLowerPrioArithmetic(tok.tokenType)) {
            return xExp;
        }

        if (xExp.t.?.isArray() or xExp.t.?.equals(types.Type{ .simple = types.SimpleType.UNIT })) {
            self.report(tok, reporter.Level.ERROR, "cannot perform '{s}' on '{s}'", .{ tok.symbol, xExp.t.?.str() }, true, true);
            // TODO: Does it make sense in this case to continue parsing?
            return SyntaxError.TypeError;
        }

        var result = "";
        _ = result;
        while (tt.isLowerPrioArithmetic(self.scanner.peek().tokenType)) {
            var op = self.scanner.next();

            var yExp = try self.parseUnaryOperators();
            defer yExp.destroy(self.alloc);

            if (yExp.t.?.isArray() or yExp.t.?.equals(types.Type{ .simple = types.SimpleType.UNIT })) {
                self.report(op, reporter.Level.ERROR, "cannot perform '{s}' on '{s}'", .{ tok.symbol, xExp.t.?.str() }, true, true);
                // TODO: Does it make sense in this case to continue parsing?
                return SyntaxError.TypeError;
            }

            var t: *types.Type = types.leastSupertype(self.alloc, xExp.t.?, yExp.t.?) orelse {
                self.report(op, reporter.Level.ERROR, "no common supertype for '{s}' and '{s}'", .{ xExp.t.?.str(), yExp.t.?.str() }, true, true);
                return SyntaxError.TypeError;
            };

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

            if (t.isConstant()) {
                // TODO: The actual value of the constant is not stored in the type anymore
                // but MUST be in the expression itself. Create a helper function that will do
                // the appropriate comparisons and return the result.
                // t.constant.int = switch (op.tokenType) {
                //     tt.ADD => xExp.t.constant.int + yExp.t.constant.int,
                //     tt.SUB => xExp.t.constant.int - yExp.t.constant.int,
                //     // TODO: i128 << i128 and i128 >> i128 overflows, figure out wrapping around?
                //     // compiler error: error: expected type 'u7', found 'i128'
                //     // tt.B_RSH => xExp.t.constant.int >> yExp.t.constant.int,
                //     // tt.B_LSH => xExp.t.constant.int << yExp.t.constant.int,
                //     tt.B_AND => xExp.t.constant.int & yExp.t.constant.int,
                //     tt.B_OR => xExp.t.constant.int | yExp.t.constant.int,
                //     tt.B_XOR => xExp.t.constant.int ^ yExp.t.constant.int,
                //     else => unreachable,
                // };
                // TODO: Set expression attributes.
            } else {
                var xVal = "";
                var yVal = "";
                _ = xVal;
                _ = yVal;

                // TODO: Continue here with the type checking.
                // valX = self.convert(t, xExp.t.?, ...);
                // valY = self.convert(t, yExp.t.?, ...);

                // TODO: Emit IR depending on the operation and expression type.
                // TODO: Set expression attributes.
            }
            // Replace the type of the expression with the new supertype.
            xExp.t.?.destroy(self.alloc);
            xExp.t = t;
        }

        // TODO: xExp.rValue
        xExp.hasLValue = false;
        xExp.semiMustFollow = true;
        xExp.endsWithReturn = false;
        return xExp;
    }

    // E^7, -, !
    fn parseUnaryOperators(self: *Parser) SyntaxError!Expression {
        // TODO: handle exps (type checking)
        const tok = self.scanner.peek();
        switch (tok.tokenType) {
            tt.SUB => {
                self.consume(tt.SUB) catch unreachable;

                var exp = try self.parseUnaryOperators();
                errdefer exp.destroy(self.alloc);

                if (!exp.t.?.isIntegral() or exp.t.?.equals(types.Type{ .simple = types.SimpleType.U64 })) {
                    self.report(tok, reporter.Level.ERROR, "cannot perform unary minus on '{s}'", .{exp.t.?.str()}, true, true);
                    // TODO: Does it make sense in this case to continue parsing?
                    return SyntaxError.TypeError;
                }

                if (exp.t.?.isConstant()) {
                    // TODO: The actual value of the constant is not stored in the type anymore
                    // but MUST be in the expression itself.
                    // exp.t.constant.int = -exp.t.constant.int;
                } else {
                    // TODO: Emit IR depending on the operation and expression type.
                    // Watch out, this is more convoluted - unary minus can be done on u8, u16 and u32,
                    // resulting in i16, i32 and i64, respectively. Unary minus cannot be applied on u64
                    // since there is no i128.
                }
                // exp.rValue = ...
                exp.hasLValue = false;
                exp.semiMustFollow = true;
                exp.endsWithReturn = false;
                return exp;
            },
            tt.NEG => {
                self.consume(tt.NEG) catch unreachable;

                var exp = try self.parseUnaryOperators();
                errdefer exp.destroy(self.alloc);

                if (!exp.t.?.isIntegral()) {
                    self.report(tok, reporter.Level.ERROR, "cannot perform bitwise not on '{s}'", .{exp.t.?.str()}, true, true);
                    // TODO: Does it make sense in this case to continue parsing?
                    return SyntaxError.TypeError;
                }

                if (exp.t.?.isConstant()) {
                    // TODO: The actual value of the constant is not stored in the type anymore
                    // but MUST be in the expression itself.
                    // exp.t.constant.int = ~exp.t.constant.int;
                } else {
                    // TODO: Emit IR depending on the operation and expression type.
                    // Watch out, this is more convoluted - unary minus can be done on u8, u16 and u32,
                    // resulting in i16, i32 and i64, respectively. Unary minus cannot be applied on u64
                    // since there is no i128.
                }

                // exp.rValue = ...

                exp.hasLValue = false;
                exp.semiMustFollow = true;
                exp.endsWithReturn = false;
                return exp;
            },
            else => return self.parseArithmeticExpressionsLower(),
        }
    }

    // E^8, *, /, %
    fn parseArithmeticExpressionsLower(self: *Parser) SyntaxError!Expression {
        var xExp = try self.parseArrayIndexingAndPrefixExpressions();
        errdefer xExp.destroy(self.alloc);

        var tok = self.scanner.peek();
        if (!tt.isHigherPrioArithmetic(tok.tokenType)) {
            return xExp;
        }

        if (xExp.t.?.isArray() or xExp.t.?.equals(types.Type{ .simple = types.SimpleType.UNIT }) or xExp.t.?.equals(types.Type{ .simple = types.SimpleType.BOOL })) {
            self.report(tok, reporter.Level.ERROR, "cannot perform '{s}' on '{s}'", .{ tok.symbol, xExp.t.?.str() }, true, true);
            // TODO: Does it make sense in this case to continue parsing?
            return SyntaxError.TypeError;
        }

        var result: []const u8 = "";
        _ = result;
        while (tt.isHigherPrioArithmetic(self.scanner.peek().tokenType)) {
            var op = self.scanner.next();
            var yExp = try self.parseArrayIndexingAndPrefixExpressions();
            defer yExp.destroy(self.alloc);

            if (yExp.t.?.isArray() or yExp.t.?.equals(types.Type{ .simple = types.SimpleType.UNIT }) or yExp.t.?.equals(types.Type{ .simple = types.SimpleType.BOOL })) {
                self.report(tok, reporter.Level.ERROR, "cannot perform '{s}' on '{s}'", .{ op.symbol, yExp.t.?.str() }, true, true);
                // TODO: Does it make sense in this case to continue parsing?
                return SyntaxError.TypeError;
            }

            var t: *types.Type = types.leastSupertype(self.alloc, xExp.t.?, yExp.t.?) orelse {
                self.report(op, reporter.Level.ERROR, "no common supertype for '{s}' and '{s}'", .{ xExp.t.?.str(), yExp.t.?.str() }, true, true);
                return SyntaxError.TypeError;
            };

            if (t.isConstant()) {
                // TODO: The actual value of the constant is not stored in the type anymore
                // but MUST be in the expression itself.
                // t.constant.int = switch (op.tokenType) {
                //     tt.MUL => xExp.t.constant.int * yExp.t.constant.int,
                //     tt.QUO => if (yExp.t.constant.int == 0) blk: {
                //         self.report(tok, reporter.Level.ERROR, "cannot divide by 0", .{}, true, true);
                //         // Instead of dividing, just return dummy value 1 to continue with the parsing.
                //         break :blk 1;
                //     } else @divExact(xExp.t.constant.int, yExp.t.constant.int),
                //     tt.REM => if (yExp.t.constant.int == 0) blk: {
                //         self.report(tok, reporter.Level.ERROR, "cannot compute remainder after dividing by 0", .{}, true, true);
                //         // Instead of dividing, just return dummy value 1 to continue with the parsing.
                //         break :blk 1;
                //     } else @rem(xExp.t.constant.int, yExp.t.constant.int),
                //     else => unreachable,
                // };
            } else {
                // TODO: Continue here with the type checking.
                // valX = self.convert(t, xExp.t.?, ...);
                // valY = self.convert(t, yExp.t.?, ...);

                // TODO: Emit IR depending on the operation and expression type.
                // Watch out, this is more convoluted - unary minus can be done on u8, u16 and u32,
                // resulting in i16, i32 and i64, respectively. Unary minus cannot be applied on u64
                // since there is no i128.
            }

            // Replace the type of the expression with the new supertype.
            xExp.t.?.destroy(self.alloc);
            xExp.t = t;
        }

        // TODO: xExp.rValue = ...
        xExp.hasLValue = false;
        xExp.semiMustFollow = true;
        xExp.endsWithReturn = false;
        return xExp;
    }

    // E^9, array indexing and ++, --, #
    fn parseArrayIndexingAndPrefixExpressions(self: *Parser) SyntaxError!Expression {
        var exp = try self.parseI();
        errdefer exp.destroy(self.alloc);

        var tok = self.scanner.peek();
        while (true) {
            switch (tok.tokenType) {
                tt.INC, tt.DEC => {
                    self.consume(tok.tokenType) catch unreachable;

                    // TODO: Do all the type conversions.

                    if (!exp.hasLValue) {
                        self.report(tok, reporter.Level.ERROR, "must be l-value in order to be incremented or decremented", .{}, true, true);
                        return SyntaxError.TypeError;
                    }
                    // TODO: Continue here with the type checking.
                    // valX = self.convert(t, xExp.t.?, ...);

                    if (!exp.lt.?.isIntegral()) {
                        self.report(tok, reporter.Level.ERROR, "value of type '{s}' cannot be incremented or decremented", .{exp.t.?.str()}, true, true);
                        return SyntaxError.TypeError;
                    }

                    // TODO: Emit IR.
                    // TODO: Set expression attributes.
                    exp.semiMustFollow = true;
                    exp.hasLValue = true;
                    exp.semiMustFollow = true;
                    exp.endsWithReturn = false;
                },
                tt.HASH => {
                    self.consume(tt.HASH) catch unreachable;
                    if (!exp.hasLValue) {
                        self.report(tok, reporter.Level.ERROR, "must be l-value in order to be dereferenced", .{}, true, true);
                        return SyntaxError.TypeError;
                    }

                    exp.t = types.Array.create(self.alloc, 0, exp.t.?);
                    // TODO: What about exp.lt here?
                    exp.rValue = exp.lValue;
                    exp.hasLValue = false;
                    exp.semiMustFollow = true;
                    exp.endsWithReturn = false;
                },
                tt.LBRACK => {
                    // TODO: Type of the expression is the first thing to be done here.

                    self.consume(tt.LBRACK) catch unreachable;
                    if (!exp.t.?.isArray()) {
                        self.report(tok, reporter.Level.ERROR, "'{s}' cannot be indexed", .{exp.t.?.str()}, true, true);
                        return SyntaxError.TypeError;
                    }

                    if (self.scanner.peek().tokenType == tt.RBRACK) {
                        // Zero-dimensional array.
                        self.consume(tt.RBRACK) catch unreachable;
                        if (exp.t.?.array.dimensions != 0) {
                            self.report(tok, reporter.Level.ERROR, "indexing to array must be {d}-dimensional instead of 0-dimensional", .{exp.t.?.array.dimensions}, true, true);
                            return SyntaxError.TypeError;
                        }
                        exp.lt = exp.t.?.array.ofType.clone(self.alloc);
                        exp.t = blk: {
                            var ofType = exp.t.?.array.ofType.clone(self.alloc);
                            exp.t.?.destroy(self.alloc);
                            break :blk ofType;
                        };
                        // TODO: exp.rValue = ...
                        exp.hasLValue = true;
                        exp.lValue = exp.rValue;
                        exp.semiMustFollow = true;
                        exp.endsWithReturn = false;

                        continue;
                    }

                    var indexes = std.ArrayList([]const u8).init(self.alloc);
                    defer indexes.deinit();

                    var next = self.scanner.peek();
                    while (next.tokenType != tt.RBRACK) {
                        var iExp = try self.parseExpression();
                        defer iExp.destroy(self.alloc);

                        if (iExp.t.?.equals(types.Type{ .simple = types.SimpleType.I64 }) or iExp.t.?.equals(types.Type{ .simple = types.SimpleType.U64 })) {
                            indexes.append(iExp.rValue) catch unreachable;
                        } else {
                            const t = types.SimpleType.create(self.alloc, types.SimpleType.I64);
                            defer t.destroy(self.alloc);
                            const value = self.convert(iExp.t.?, t, ConvMode.IMPLICIT, iExp.rValue) catch {
                                return SyntaxError.TypeError;
                            };
                            indexes.append(value) catch unreachable;
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

                    if (indexes.items.len != exp.t.?.array.dimensions) {
                        self.report(next, reporter.Level.ERROR, "cannot index {d}-dimensional array with {d}-dimensional index", .{ exp.t.?.array.dimensions, indexes.items.len }, true, true);
                        return SyntaxError.TypeError;
                    }

                    if (exp.t.?.array.ofType.isUnit()) {
                        // TODO: Memory leak over here...
                        exp.destroy(self.alloc);
                        exp.t = types.SimpleType.create(self.alloc, types.SimpleType.UNIT);
                        exp.lt = exp.t.?.clone(self.alloc);
                        exp.lValue = "null"; // TODO: Is this correct?
                        exp.hasLValue = true;
                        exp.semiMustFollow = true;
                        exp.endsWithReturn = false;
                        continue;
                    }

                    // TODO: Emit IR with pointer arithmetic.
                    exp.lt.?.destroy(self.alloc);
                    exp.lt = exp.t.?.array.ofType.clone(self.alloc);
                    exp.t = blk: {
                        var ofType = exp.t.?.array.ofType.clone(self.alloc);
                        exp.t.?.destroy(self.alloc);
                        break :blk ofType;
                    };
                    // TODO: exp.rValue = ...
                    exp.hasLValue = true;
                    // TODO: exp.lValue = ;
                    exp.semiMustFollow = true;
                    exp.endsWithReturn = false;
                },
                else => return exp,
            }
            tok = self.scanner.peek();
        }
    }

    fn parseI(self: *Parser) SyntaxError!Expression {
        var next = self.scanner.peek();
        switch (next.tokenType) {
            tt.LBRACE => return self.parseBody(),
            tt.LPAREN => {
                self.consume(tt.LPAREN) catch unreachable;

                var exp = try self.parseExpression();
                errdefer exp.destroy(self.alloc);
                exp.semiMustFollow = true;

                try self.consume(tt.RPAREN);

                return exp;
            },
            tt.IDENT => {
                const ident = self.consumeGet(tt.IDENT) catch unreachable;
                if (self.st.functionDefined(ident.symbol)) {
                    return try self.parseFunctionCall(ident);
                } else {
                    var s = self.st.get(ident.symbol) orelse {
                        self.report(next, reporter.Level.ERROR, "no variable '{s}' is defined", .{next.symbol}, true, true);
                        return SyntaxError.TypeError;
                    };

                    if (s.t.isUnit()) {
                        return Expression{
                            .t = s.t.clone(self.alloc),
                            .lt = s.t.clone(self.alloc),
                            .hasLValue = true,
                            .rValue = "",
                            .lValue = "",
                            .semiMustFollow = true,
                            .endsWithReturn = false,
                        };
                    }
                    return Expression{
                        .t = s.t.clone(self.alloc),
                        .lt = s.t.clone(self.alloc),
                        // .rValue = ...,
                        .hasLValue = true,
                        // .lValue = ...,
                        .semiMustFollow = true,
                        .endsWithReturn = false,
                    };
                }
            },
            tt.C_INT => {
                const int = self.consumeGet(tt.C_INT) catch unreachable;
                var value = std.fmt.parseInt(i128, int.symbol, 0) catch unreachable;
                _ = value;

                return Expression{
                    .t = types.Constant.create(self.alloc, types.ConstantTag.int),
                    .lt = types.Constant.create(self.alloc, types.ConstantTag.int),
                    .rValue = int.symbol,
                    .hasLValue = false,
                    .semiMustFollow = true,
                    .endsWithReturn = false,
                };
            },
            tt.C_FLOAT => {
                const int = self.consumeGet(tt.C_INT) catch unreachable;
                var value = std.fmt.parseFloat(f64, int.symbol) catch unreachable;
                _ = value;

                return Expression{
                    .t = types.Constant.create(self.alloc, types.ConstantTag.float),
                    .lt = types.Constant.create(self.alloc, types.ConstantTag.float),
                    .rValue = int.symbol,
                    .hasLValue = false,
                    .semiMustFollow = true,
                    .endsWithReturn = false,
                };
            },
            tt.C_NULL => {
                self.consume(tt.C_NULL) catch unreachable;

                // TODO: What type should the array carry if we're talking about nulls?
                return Expression{
                    .t = types.Array.create(self.alloc, 0, types.SimpleType.create(self.alloc, types.SimpleType.UNIT)),
                    .lt = types.Array.create(self.alloc, 0, types.SimpleType.create(self.alloc, types.SimpleType.UNIT)),
                    .hasLValue = false,
                    .semiMustFollow = true,
                    .endsWithReturn = false,
                };
            },
            tt.C_BOOL => {
                const boolean = self.consumeGet(tt.C_BOOL) catch unreachable;
                var value = if (std.mem.eql(u8, boolean.symbol, "true")) "1" else if (std.mem.eql(u8, boolean.symbol, "false")) "0" else unreachable;

                return Expression{
                    .t = types.SimpleType.create(self.alloc, types.SimpleType.BOOL),
                    .lt = types.Constant.create(self.alloc, types.ConstantTag.bool),
                    .rValue = value,
                    .hasLValue = false,
                    .semiMustFollow = true,
                    .endsWithReturn = false,
                };
            },
            tt.AT => {
                self.consume(tt.AT) catch unreachable;
                if (types.startsType(self.scanner.peek().symbol)) {
                    // We're dealing with explicit type conversion.
                    var newT = try self.parseType();
                    errdefer newT.destroy(self.alloc);

                    // TODO: Figure out arrays (starts on line 1892).
                    var cExp = try self.parseI();
                    errdefer cExp.destroy(self.alloc);

                    // TODO: Convert the types
                    // self.convert(newT, cExp.t.?, ..., ConvMode.EXPLICIT);

                    cExp.destroy(self.alloc);
                    cExp.t = newT;
                    cExp.hasLValue = false;
                    // TODO: cExp.rValue = ...
                    cExp.semiMustFollow = true;
                    cExp.endsWithReturn = false;
                    return cExp;
                }
                // We're dealing with builtin functions.
                const ident = try self.consume(tt.IDENT);
                _ = ident;
                // TODO: Emit IR for calling builtin functions.
                return Expression{
                    .t = types.SimpleType.create(self.alloc, types.SimpleType.UNIT),
                    .hasLValue = false,
                    .rValue = "TODO: Name of the instruction",
                    .semiMustFollow = true,
                    .endsWithReturn = false,
                };
            },
            tt.C_CHAR => {
                const char = self.consumeGet(tt.C_CHAR) catch unreachable;
                if (char.symbol[0] < 0 or char.symbol[0] > 255) {
                    self.report(char, reporter.Level.ERROR, "'{s}' is not ASCII-encoded", .{char.symbol}, true, true);
                    return SyntaxError.TypeError;
                }

                return Expression{
                    .t = types.SimpleType.create(self.alloc, types.SimpleType.U8),
                    .rValue = char.symbol,
                    .hasLValue = false,
                    .semiMustFollow = true,
                    .endsWithReturn = false,
                };
            },
            tt.C_STRING => {
                const string = self.consumeGet(tt.C_STRING) catch unreachable;
                _ = string;
                // TODO: Emit IR to store the string.
                // TODO: Set expression attributes.
                return Expression{
                    .t = types.Array.create(self.alloc, 1, types.SimpleType.create(self.alloc, types.SimpleType.U8)),
                    .hasLValue = false,
                    .semiMustFollow = true,
                    .endsWithReturn = false,
                };
            },
            else => {
                self.report(next, reporter.Level.ERROR, "expected '{s}', '(', identifier, function call or a constant value but got '{s}' instead", .{ tt.LBRACE.str(), next.tokenType.str() }, true, true);
                return SyntaxError.UnexpectedToken;
            },
        }
        unreachable;
    }

    fn parseFunctionCall(self: *Parser, ident: token.Token) SyntaxError!Expression {
        // TODO: What if we parsed all the arguments first until we reach ')' and then
        // type check and emit? This way, we'll get friendlier error reporting when
        // the function call has wrong number of arguments.
        try self.consume(tt.LPAREN);

        var funcSymbol = self.st.get(ident.symbol) orelse unreachable;

        for (0..funcSymbol.t.func.args.items.len, funcSymbol.t.func.args.items) |i, arg| {
            var exp = try self.parseExpression();
            defer exp.destroy(self.alloc);
            if (!exp.t.?.equals(types.Type{ .simple = types.SimpleType.UNIT })) {
                // TODO: Prepare LLVM argument by converting types if necessary
            } else if (!arg.t.equals(types.Type{ .simple = types.SimpleType.UNIT })) {
                // TODO: Fix error reporting.
                return SyntaxError.TypeError;
            }

            if (i != funcSymbol.t.func.args.items.len - 1) try self.consume(tt.COMMA);
        }

        try self.consume(tt.RPAREN);
        // TODO: Emit IR.

        return Expression{
            .t = funcSymbol.t.func.retT.clone(self.alloc),
            .lt = funcSymbol.t.func.retT.clone(self.alloc),
            .rValue = "TODO: name of a call instruction",
            .hasLValue = false,
            .semiMustFollow = true,
            .endsWithReturn = false,
        };
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

    const ConvMode = enum { IMPLICIT, EXPLICIT, BITCAST };
    const ConvErr = error{ NoImplicit, Overflow };

    fn convert(
        self: *Parser,
        from: *types.Type,
        to: *types.Type,
        conv: ConvMode,
        what: []const u8,
    ) ConvErr![]const u8 {
        if (from.equals(to.*)) {
            return what;
        }

        if (from.isUnit()) {
            // Unit is not implicitely convertible to anything.
            if (conv == ConvMode.IMPLICIT) return ConvErr.NoImplicit;

            if (to.isArray()) return "null";
            if (to.isDouble() or to.isFloat()) return "0.0";
            return "0";
        }

        if (to.isUnit()) {
            // Cannot implicitely convert to unit.
            if (conv == ConvMode.IMPLICIT) return ConvErr.NoImplicit;
            return what;
        }

        if (from.isBool()) {
            if (conv == ConvMode.IMPLICIT) return ConvErr.NoImplicit;

            if (to.isNumeric()) {
                //
            } else if (to.isFloat() and conv == ConvMode.BITCAST) {
                //
            } else if (to.isDouble() and conv == ConvMode.BITCAST) {
                //
            } else if (to.isFloat() or to.isDouble()) {
                // Conversion must be explicit.
            } else if (to.isArray() and conv == ConvMode.EXPLICIT) {
                //
            } else if (to.isArray()) {
                // Coversion must be bitcast.
            } else unreachable;

            return "TODO";
        }

        if (from.isPointer()) {
            if (to.isArray()) return what;
            if (conv == ConvMode.IMPLICIT) return ConvErr.NoImplicit;
            if (conv == ConvMode.EXPLICIT) {
                if (to.isDouble() or to.isFloat()) return "0.0";
                return "0";
            }

            // Conversion must be bitcast.
            if (to.isNumeric() or to.isBool()) {
                //
            } else if (to.isFloat()) {
                //
            } else if (to.isDouble()) {
                //
            } else unreachable;
        }

        if (from.isArray()) {
            if (conv == ConvMode.IMPLICIT) return ConvErr.NoImplicit;
            if (to.isArray()) return what;
            if (to.isBool() and conv == ConvMode.EXPLICIT) {
                //
            } else if (to.isBool() or to.isNumeric()) {
                //
            } else if (to.isFloat()) {
                //
            } else if (to.isDouble()) {
                //
            } else unreachable;

            return "TODO";
        }

        if (from.isConstant()) {
            if (to.isNumeric()) {
                // TODO: Check bounds and return null if it does not fit.
                if (true) {
                    return "CONSTANT";
                }
                return ConvErr.Overflow;
            }

            if (to.isFloat()) {
                if (conv != ConvMode.BITCAST) return "constant to float in hexadecimal";
                return "TODO";
            }

            if (to.isDouble()) {
                if (conv != ConvMode.BITCAST) return "constant to double in hexadecimal";
                return "TODO";
            }

            if (conv == ConvMode.IMPLICIT) return ConvErr.NoImplicit;
            if (to.isBool() and conv == ConvMode.EXPLICIT) {
                // FIXME: Condition should be std.mem.eql(u8, from.rvalue, "0")
                if (false) return "false";
                return "true";
            }

            if (to.isBool()) {
                // Conversion must be bitcast.
                // FIXME: Condition should be parse from.rvalue to int and do the modulo.
                if (false) return "false";
                return "true";
            }

            if (to.isArray()) {
                return "TODO";
            }

            unreachable;
        }

        if (!from.isSigned()) {
            if (false) {
                // If `to` has greater bit width than `from`.
                return "TODO";
            }

            if (to.isFloat() or to.isDouble()) {
                if (conv != ConvMode.BITCAST) {
                    return "TODO";
                }

                var t = if (to.isFloat())
                    types.SimpleType.create(self.alloc, types.SimpleType.U32)
                else
                    types.SimpleType.create(self.alloc, types.SimpleType.U64);
                defer t.destroy(self.alloc);

                if (false) {
                    // If `from` has greater bit width than `t`.
                } else if (false) {
                    // If `from` has smaller bit width than `t`.
                } else {
                    // temp = what;
                }

                return "TODO";
            }

            if (conv == ConvMode.IMPLICIT) return ConvErr.NoImplicit;
            if (to.isNumeric()) {
                // `to` is numeric and has the same bit width as from.
                return what;
            }

            if (to.isBool() and conv == ConvMode.EXPLICIT) {
                //
            } else if (to.isBool() or to.isNumeric()) {
                //
            } else if (to.isArray()) {
                //
            } else unreachable;

            return "TODO";
        }

        if (from.isSigned()) {
            if (false) {
                // `to` is numeric and has greater bit width than `from`
                if (conv != ConvMode.BITCAST) {} else {}
                return "TODO";
            }

            if (to.isFloat() or to.isDouble()) {
                if (conv != ConvMode.BITCAST) {
                    return "TODO";
                }

                var t = if (to.isFloat())
                    types.SimpleType.create(self.alloc, types.SimpleType.U32)
                else
                    types.SimpleType.create(self.alloc, types.SimpleType.U64);
                defer t.destroy(self.alloc);

                if (false) {
                    // `from` has bit width greater than `t`.
                } else if (false) {
                    // `from` has bit width smaller than `t`.
                } else {
                    // `from` and `t` have the same bid width.
                    // temp = what;
                }

                return "TODO";
            }

            if (conv == ConvMode.IMPLICIT) return ConvErr.NoImplicit;

            if (to.isNumeric()) {
                // `to` is numeric and has greater bit width than `from`.
                return "TODO";
            }

            if (to.isNumeric()) {
                // `to` is numeric and has the same bit width as `from`.
                return what;
            }

            if (to.isBool() and conv == ConvMode.EXPLICIT) {
                //
            } else if (to.isBool() or to.isNumeric()) {
                //
            } else if (to.isArray()) {
                //
            } else unreachable;

            return "TODO";
        }

        if (from.isFloat() or from.isDouble()) {
            if (to.isFloat()) {
                if (conv != ConvMode.BITCAST) {
                    return "TODO";
                }
                return "TODO";
            }

            if (to.isDouble()) {
                if (conv != ConvMode.BITCAST) {
                    return "TODO";
                }
                return "TODO";
            }

            if (conv == ConvMode.IMPLICIT) return ConvErr.NoImplicit;

            if (to.isNumeric() or (conv == ConvMode.BITCAST) and (to.isBool())) {
                if (conv != ConvMode.BITCAST) {
                    if (!to.isSigned()) {
                        //
                    } else {
                        //
                    }
                    return "TODO";
                }

                var t = if (from.isFloat())
                    types.SimpleType.create(self.alloc, types.SimpleType.U32)
                else
                    types.SimpleType.create(self.alloc, types.SimpleType.U64);
                defer t.destroy(self.alloc);

                if (false) {
                    // `to` has smaller bit width than `t`.
                } else if (false) {
                    // `to` has greater bit width than `t`.
                } else {
                    // result = temp;
                }
            } else if (to.isBool()) {
                //
            } else if (to.isArray()) {
                //
                //
                var t = if (from.isFloat())
                    types.SimpleType.create(self.alloc, types.SimpleType.U32)
                else
                    types.SimpleType.create(self.alloc, types.SimpleType.U64);
                defer t.destroy(self.alloc);
            } else unreachable;

            return "TODO";
        }

        unreachable;
    }
};
