const std = @import("std");

const codegen = @import("codegen.zig");
const reporter = @import("reporter.zig");
const scanner = @import("scanner.zig");
const symbols = @import("symbols.zig");
const token = @import("token.zig");
const types = @import("types.zig");

const tt = token.TokenType;

pub const SyntaxError = error{ UnexpectedToken, TypeError, OutOfMemory, RecoverableError };

pub const Expression = struct {
    t: ?*types.Type = null,
    lt: ?*types.Type = null,

    rValue: ?[]const u8 = null,

    hasLValue: bool = false,
    lValueComputed: bool = false,
    lValue: ?[]const u8 = null,

    semiMustFollow: bool = false,
    endsWithReturn: bool = false,

    callsFunction: bool = false,

    pub fn destroy(self: Expression, alloc: std.mem.Allocator) void {
        if (self.t) |t| t.destroy(alloc);
        if (self.lt) |lt| lt.destroy(alloc);

        if (self.rValue) |rv| alloc.free(rv);
        if (self.hasLValue) if (self.lValue) |lv| alloc.free(lv);
    }

    pub fn destroyRValue(self: *Expression, alloc: std.mem.Allocator) void {
        if (self.rValue) |rv| alloc.free(rv);
        self.rValue = null;
    }

    pub fn destroyLValue(self: *Expression, alloc: std.mem.Allocator) void {
        if (self.lValue) |lv| alloc.free(lv);
        self.lValue = null;
        self.hasLValue = false;
    }

    pub fn setLValue(self: *Expression, alloc: std.mem.Allocator, lvalue: []const u8) void {
        self.destroyLValue(alloc);
        self.lValue = lvalue;
        self.hasLValue = true;
    }

    pub fn clone(self: Expression, alloc: std.mem.Allocator) Expression {
        var exp = Expression{
            .t = if (self.t) |t| t.clone(alloc) else null,
            .lt = if (self.lt) |lt| lt.clone(alloc) else null,
            .hasLValue = self.hasLValue,
            .lValueComputed = self.lValueComputed,
            .semiMustFollow = self.semiMustFollow,
            .endsWithReturn = self.endsWithReturn,
            .callsFunction = self.callsFunction,
        };

        if (self.rValue) |rv| exp.rValue = std.fmt.allocPrint(alloc, "{s}", .{rv}) catch unreachable;
        if (self.lValue) |lv| exp.lValue = std.fmt.allocPrint(alloc, "{s}", .{lv}) catch unreachable;

        return exp;
    }
};

pub const Parser = struct {
    s: scanner.Scanner,
    r: ?reporter.Reporter,
    c: codegen.CodeGen,

    alloc: std.mem.Allocator,

    st: symbols.SymbolTable,

    breakStack: std.ArrayList([]const u8),
    contStack: std.ArrayList([]const u8),

    returnType: ?*types.Type = null,

    // TODO:
    // * constant folding
    // * IR emitter
    //      * fix potential issue in the convert function w/ memory leak when `result = temp;`
    //      * handling multiple functions

    pub fn init(
        alloc: std.mem.Allocator,
        s: scanner.Scanner,
        r: ?reporter.Reporter,
        c: codegen.CodeGen,
    ) Parser {
        return .{
            .alloc = alloc,

            .s = s,
            .r = r,
            .c = c,

            .st = symbols.SymbolTable.init(alloc),

            .breakStack = std.ArrayList([]const u8).init(alloc),
            .contStack = std.ArrayList([]const u8).init(alloc),
        };
    }

    pub fn deinit(self: *Parser) void {
        self.st.deinit();
        self.breakStack.deinit();
        self.contStack.deinit();
    }

    pub fn parse(self: *Parser, writer: std.fs.File.Writer) !void {
        defer self.c.deinit();

        // Open the global scope.
        try self.st.open();

        var next = self.s.peek();
        while (next.tokenType != tt.EOF) {
            self.parseTopLevelStatement() catch {
                return SyntaxError.UnexpectedToken;
            };

            next = self.s.peek();
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

        try self.c.write(writer);
    }

    fn parseTopLevelStatement(self: *Parser) SyntaxError!void {
        // 1) global variable definition: <type> <ident> = <expr>;
        // 2) (forward) function declaration: <type> <ident>(<arglist>);
        // 3) function definition: <type> <ident>(<arglist>) <body>

        const t = try self.parseType();
        const ident = try self.consumeGet(tt.IDENT);

        var next = self.s.next();
        switch (next.tokenType) {
            tt.ASSIGN => {
                // TODO: Emit code for global variable declaration.
                errdefer t.destroy(self.alloc);

                var exp = try self.parseSubExpression();
                defer exp.destroy(self.alloc);

                if (exp.callsFunction) {
                    self.report(next, reporter.Level.ERROR, "global constant definition cannot call a function", .{}, true, true);
                    return SyntaxError.TypeError;
                }

                const value = self.convert(exp.t.?, t, ConvMode.IMPLICIT, "TODO", self.c.lastBlockIndex()) catch |err| {
                    switch (err) {
                        ConvErr.Overflow => self.report(ident, reporter.Level.ERROR, "cannot cast value of type {s} to {s} due to possible overflow", .{ exp.t.?.str(), t.str() }, true, true),
                        ConvErr.NoImplicit => self.report(ident, reporter.Level.ERROR, "cannot cast value of type {s} to {s}", .{ exp.t.?.str(), t.str() }, true, true),
                    }

                    return SyntaxError.TypeError;
                };
                _ = value;

                _ = try self.declare(ident.symbol, t, ident, true);

                try self.consume(tt.SEMICOLON);
            },
            tt.LPAREN => {
                self.c.newSegment();

                var ft = self.alloc.create(types.Type) catch unreachable;
                ft.* = types.Type{ .func = types.Func.init(self.alloc, t) };
                errdefer ft.destroy(self.alloc);
                ft.func.args = try self.parseArgList();

                var llvmName = if (std.mem.eql(u8, "main", ident.symbol))
                    std.fmt.allocPrint(self.alloc, "@main", .{}) catch unreachable
                else
                    self.c.genLLVMName(ident.symbol);
                errdefer self.alloc.free(llvmName);

                var fs = symbols.Symbol{
                    .name = ident.symbol,
                    .llvmName = llvmName,
                    .location = ident,
                    .t = ft,
                    .defined = false,
                };

                const sig = codegen.signature(self.alloc, llvmName, ft.func.args, t.*);
                self.c.emitA(sig);

                // Try to parse the function body if it is a function definition.
                next = self.s.peek();
                if (next.tokenType == tt.LBRACE) {
                    self.c.emitInit(std.fmt.allocPrint(self.alloc, "{{", .{}) catch unreachable);
                    // Open a new scope just for the function arguments. This way,
                    // arguments can be shadowed in the function body.
                    try self.st.open();

                    // Set the expected type of the returned value.
                    self.returnType = t.clone(self.alloc);
                    defer self.returnType.?.destroy(self.alloc);

                    // Add the arguments to the new scope.
                    fs.t.func.namedParams = true;
                    for (fs.t.func.args.items) |arg| {
                        if (std.mem.eql(u8, arg.name, "")) {
                            if (fs.t.func.namedParams) {
                                self.report(fs.location, reporter.Level.ERROR, "function definition of '{s}' must have named arguments", .{fs.name}, true, true);
                            }
                            fs.t.func.namedParams = false;
                        } else {
                            var newArg = arg.clone(self.alloc);
                            self.st.insert(newArg) catch |err| switch (err) {
                                symbols.SymbolError.SymbolAlreadyExists => {
                                    self.report(fs.location, reporter.Level.ERROR, "cannot define 2 function arguments with the same name", .{}, true, true);
                                    // Destroy the new argument immediately since we need to move on with the parsing.
                                    newArg.destroy(self.alloc);
                                },
                                symbols.SymbolError.OutOfMemory => return SyntaxError.OutOfMemory,
                            };
                        }

                        var llvmType = codegen.llvmType(arg.t.*);
                        var paramAlloc = arg.llvmName;

                        self.c.emitInit(std.fmt.allocPrint(self.alloc, "{s} = alloca {s}", .{ paramAlloc, llvmType }) catch unreachable);
                        self.c.emitInit(std.fmt.allocPrint(self.alloc, "store {s} {s}, ptr {s}", .{ llvmType, arg.llvmName, paramAlloc }) catch unreachable);
                    }

                    self.c.newSegment();

                    var bl = self.c.genLLVMName("label");
                    defer self.alloc.free(bl);

                    self.c.emit(
                        std.fmt.allocPrint(self.alloc, "br label {s}", .{bl}) catch unreachable,
                        self.c.lastBlockIndex(),
                    );

                    self.c.emitBlock(bl);

                    var exp = try self.parseBody();
                    defer exp.destroy(self.alloc);

                    if (!exp.endsWithReturn) {
                        // TODO: Fix endsWithReturn synthesized attribute?
                        if (self.returnType.?.isUnit()) {
                            self.c.emit(
                                std.fmt.allocPrint(self.alloc, "ret void", .{}) catch unreachable,
                                self.c.lastBlockIndex(),
                            );
                        } else {
                            var retVal = self.convert(exp.t.?, self.returnType.?, ConvMode.IMPLICIT, exp.rValue.?, self.c.lastBlockIndex()) catch {
                                self.report(fs.location, reporter.Level.ERROR, "cannot return value of type {s} from function that must return {s}", .{ exp.t.?.str(), self.returnType.?.str() }, true, true);
                                return SyntaxError.TypeError;
                            };

                            self.c.emit(
                                std.fmt.allocPrint(self.alloc, "ret {s} {s}", .{ codegen.llvmType(self.returnType.?.*), retVal }) catch unreachable,
                                self.c.lastBlockIndex(),
                            );
                        }
                    }

                    // Close the scope just for the function arguments.
                    self.st.close();

                    self.c.emitA(std.fmt.allocPrint(self.alloc, "}}\n", .{}) catch unreachable);
                    self.c.waits = false;

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
        var next = self.s.peek();

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
                ((self.s.peek().tokenType == tt.COMMA or self.s.peek().tokenType == tt.RPAREN) and argNames == argNamesE.DoNotKnow))
            {
                argNames = argNamesE.Unnamed;
                s.location = undefined;
                s.llvmName = std.fmt.allocPrint(self.alloc, "TBD", .{}) catch unreachable;
            } else {
                argNames = argNamesE.Named;
                const id = try self.consumeGet(tt.IDENT);
                s.name = id.symbol;
                s.llvmName = self.c.genLLVMName(id.symbol);
                s.location = id;
            }

            if (std.mem.eql(u8, s.name, "") and types.SimpleType.isType(s.name)) {
                self.report(next, reporter.Level.ERROR, "function argument must not be named as type", .{}, true, true);
            } else {
                try args.append(s);
            }

            expectComma = true;
            next = self.s.peek();
        }

        self.consume(tt.RPAREN) catch unreachable;
        return args;
    }

    /// Parses a type.
    pub fn parseType(self: *Parser) SyntaxError!*types.Type {
        var tok = self.s.next();

        var tp = try self.alloc.create(types.Type);
        errdefer self.alloc.destroy(tp);

        return switch (tok.tokenType) {
            tt.IDENT => if (types.SimpleType.getType(tok.symbol)) |t| {
                tp.* = types.Type{ .simple = t };
                return tp;
            } else {
                self.report(tok, reporter.Level.ERROR, "'{s}' is not a type", .{tok.symbol}, true, true);
                return SyntaxError.UnexpectedToken;
            },
            tt.LBRACK => {
                var array: types.Array = .{ .alloc = self.alloc };
                while (tok.tokenType != tt.RBRACK) {
                    try self.consume(tt.SUB);
                    array.dimensions += 1;

                    tok = self.s.next();
                    if (tok.tokenType == tt.COMMA) {}
                }
                array.ofType = try self.parseType();
                tp.* = types.Type{ .array = array };
                return tp;
            },
            else => {
                self.report(tok, reporter.Level.ERROR, "expected type, found '{s}' instead", .{tok.symbol}, true, true);
                return SyntaxError.UnexpectedToken;
            },
        };
    }

    // E
    pub fn parseExpression(self: *Parser) SyntaxError!Expression {
        var exp: ?Expression = null;
        errdefer if (exp) |e| e.destroy(self.alloc);

        var tok = self.s.peek();
        if (tok.tokenType == tt.RBRACE) {
            // TODO: Is this condition even correct?
            return Expression{
                .t = types.SimpleType.create(self.alloc, types.SimpleType.UNIT),
                .lt = types.SimpleType.create(self.alloc, types.SimpleType.UNIT),
                .endsWithReturn = false,
                .semiMustFollow = false,
                .lValueComputed = false,
            };
        }

        while (!endParseExpression(tok.tokenType)) {
            var semi = false;

            if (tok.tokenType != tt.SEMICOLON) {
                var exp1 = switch (tok.tokenType) {
                    tt.WHILE => try self.parseWhile(),
                    tt.DO => try self.parseDoWhile(),
                    tt.FOR => try self.parseFor(),
                    tt.IF => try self.parseIf(),
                    tt.RETURN => blk: {
                        var exp2 = try self.parseReturn();
                        semi = true;
                        break :blk exp2;
                    },
                    tt.BREAK => blk: {
                        var exp2 = try self.parseBreak();
                        semi = true;
                        break :blk exp2;
                    },
                    tt.CONTINUE => blk: {
                        var exp2 = try self.parseContinue();
                        semi = true;
                        break :blk exp2;
                    },
                    tt.SEMICOLON => unreachable,
                    else => blk: {
                        var exp2 = try self.parseSubExpression();
                        semi = exp2.semiMustFollow;
                        break :blk exp2;
                    },
                };

                if (exp) |e| e.destroy(self.alloc);
                exp = exp1;
            }

            tok = self.s.peek();
            if (tok.tokenType == tt.SEMICOLON) {
                if (!semi) {
                    self.report(tok, reporter.Level.ERROR, "unexpected ';'", .{}, true, true);
                    return SyntaxError.TypeError;
                }
                try self.consume(tt.SEMICOLON);
                tok = self.s.peek();
                continue;
            }

            if (semi and !endParseExpression(tok.tokenType)) {
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

        var callsFunction = body.callsFunction or ifExp.callsFunction;

        if (self.s.peek().tokenType != tt.ELSE) {
            return Expression{
                .t = types.SimpleType.create(self.alloc, types.SimpleType.UNIT),
                .endsWithReturn = false,
                .semiMustFollow = false,
                .hasLValue = false,
                .callsFunction = callsFunction,
            };
        }

        var b = body.endsWithReturn;
        var c = false;

        self.consume(tt.ELSE) catch unreachable;

        if (self.s.peek().tokenType == tt.IF) {
            var anotherIfExp = try self.parseIf();
            defer anotherIfExp.destroy(self.alloc);

            c = anotherIfExp.endsWithReturn;
            callsFunction = callsFunction or anotherIfExp.callsFunction;
        } else {
            var anotherBody = try self.parseBody();
            defer anotherBody.destroy(self.alloc);

            c = anotherBody.endsWithReturn;
            callsFunction = callsFunction or anotherBody.callsFunction;
        }

        if (b and c) {
            return Expression{
                .t = types.SimpleType.create(self.alloc, types.SimpleType.UNIT),
                .endsWithReturn = true,
                .semiMustFollow = false,
                .hasLValue = false,
                .callsFunction = callsFunction,
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
            .callsFunction = callsFunction,
        };
    }

    fn parseFor(self: *Parser) SyntaxError!Expression {
        // TODO: Decide whether to allow loops such as `for (,,) {...}`.
        self.st.open() catch unreachable;
        defer self.st.close();

        var condBlockName = self.c.genLLVMName("ForCondition");
        var incBlockName = self.c.genLLVMName("ForIncrement");
        var bodyBlockName = self.c.genLLVMName("ForBody");
        var endBlockName = self.c.genLLVMName("ForEnd");

        defer self.alloc.free(condBlockName);
        defer self.alloc.free(incBlockName);
        defer self.alloc.free(bodyBlockName);
        defer self.alloc.free(endBlockName);

        self.consume(tt.FOR) catch unreachable;
        try self.consume(tt.LPAREN);

        var initExp = try self.parseExpression();
        defer initExp.destroy(self.alloc);

        self.c.emit(
            std.fmt.allocPrint(self.alloc, "br label {s}", .{condBlockName}) catch unreachable,
            self.c.lastBlockIndex(),
        );

        const comma = try self.consumeGet(tt.COMMA);

        self.c.emitBlock(condBlockName);

        var condExp = try self.parseExpression();
        defer condExp.destroy(self.alloc);
        if (!condExp.t.?.isBool()) {
            self.report(comma, reporter.Level.ERROR, "condition of for loop must be boolean", .{}, true, true);
            return SyntaxError.TypeError;
        }

        self.c.emit(
            std.fmt.allocPrint(self.alloc, "br i1 {s}, label {s}, label {s}", .{
                condExp.rValue.?,
                bodyBlockName,
                endBlockName,
            }) catch unreachable,
            self.c.lastBlockIndex(),
        );

        try self.consume(tt.COMMA);

        self.c.newSegment();
        self.c.emitBlock(incBlockName);

        var stepExp = try self.parseExpression();
        defer stepExp.destroy(self.alloc);

        self.c.emit(
            std.fmt.allocPrint(self.alloc, "br label {s}", .{condBlockName}) catch unreachable,
            self.c.lastBlockIndex(),
        );

        try self.consume(tt.RPAREN);

        self.c.emitBlock(bodyBlockName);

        self.breakStack.append(endBlockName) catch unreachable;
        self.contStack.append(incBlockName) catch unreachable;

        defer _ = self.contStack.pop();
        defer _ = self.breakStack.pop();

        var body = try self.parseBody();
        defer body.destroy(self.alloc);

        self.c.emit(
            std.fmt.allocPrint(self.alloc, "br label {s}", .{incBlockName}) catch unreachable,
            self.c.lastBlockIndex(),
        );

        self.c.emitBlock(endBlockName);

        return Expression{
            .t = types.SimpleType.create(self.alloc, types.SimpleType.UNIT),
            .semiMustFollow = false,
            .endsWithReturn = false,
            .hasLValue = false,
            .callsFunction = body.callsFunction or stepExp.callsFunction or condExp.callsFunction or initExp.callsFunction,
        };
    }

    fn parseWhile(self: *Parser) SyntaxError!Expression {
        self.consume(tt.WHILE) catch unreachable;

        var condBlockName = self.c.genLLVMName("WhileCondition");
        var bodyBlockName = self.c.genLLVMName("WhileBody");
        var endBlockName = self.c.genLLVMName("WhileEnd");

        defer self.alloc.free(condBlockName);
        defer self.alloc.free(bodyBlockName);
        defer self.alloc.free(endBlockName);

        self.c.emit(
            std.fmt.allocPrint(self.alloc, "br label {s}", .{condBlockName}) catch unreachable,
            self.c.lastBlockIndex(),
        );
        self.c.emitBlock(condBlockName);

        const lparen = try self.consumeGet(tt.LPAREN);

        var exp = try self.parseExpression();
        defer exp.destroy(self.alloc);

        if (!exp.t.?.isBool()) {
            self.report(lparen, reporter.Level.ERROR, "condition of while loop must be boolean", .{}, true, true);
            return SyntaxError.TypeError;
        }

        try self.consume(tt.RPAREN);

        self.c.emit(
            std.fmt.allocPrint(
                self.alloc,
                "br i1 {s}, label {s}, label {s}",
                .{ exp.rValue.?, bodyBlockName, endBlockName },
            ) catch unreachable,
            self.c.lastBlockIndex(),
        );

        self.c.emitBlock(bodyBlockName);

        self.breakStack.append(endBlockName) catch unreachable;
        self.contStack.append(condBlockName) catch unreachable;

        defer _ = self.breakStack.pop();
        defer _ = self.contStack.pop();

        var body = try self.parseBody();
        defer body.destroy(self.alloc);

        self.c.emit(
            std.fmt.allocPrint(self.alloc, "br label {s}", .{condBlockName}) catch unreachable,
            self.c.lastBlockIndex(),
        );
        self.c.emitBlock(endBlockName);

        return Expression{
            .t = types.SimpleType.create(self.alloc, types.SimpleType.UNIT),
            .semiMustFollow = false,
            .endsWithReturn = false,
            .hasLValue = false,
            .callsFunction = body.callsFunction or exp.callsFunction,
        };
    }

    fn parseDoWhile(self: *Parser) SyntaxError!Expression {
        self.consume(tt.DO) catch unreachable;

        var condBlockName = self.c.genLLVMName("DowhileCondition");
        var bodyBlockName = self.c.genLLVMName("DowhileBody");
        var endBlockName = self.c.genLLVMName("DowhileEnd");

        defer self.alloc.free(condBlockName);
        defer self.alloc.free(bodyBlockName);
        defer self.alloc.free(endBlockName);

        var bodyCallsFunction = false;
        {
            // Lexical scope to leverage defers to pop labels immediately after parsing the body.

            self.c.emit(
                std.fmt.allocPrint(self.alloc, "br label {s}", .{bodyBlockName}) catch unreachable,
                self.c.lastBlockIndex(),
            );
            self.c.emitBlock(bodyBlockName);

            self.breakStack.append(endBlockName) catch unreachable;
            self.contStack.append(condBlockName) catch unreachable;

            defer _ = self.contStack.pop();
            defer _ = self.breakStack.pop();

            var body = try self.parseBody();
            defer body.destroy(self.alloc);

            bodyCallsFunction = body.callsFunction;

            self.c.emit(
                std.fmt.allocPrint(self.alloc, "br label {s}", .{condBlockName}) catch unreachable,
                self.c.lastBlockIndex(),
            );
            self.c.emitBlock(condBlockName);
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

        self.c.emit(
            std.fmt.allocPrint(self.alloc, "br i1 {s}, label {s}, label {s}", .{
                exp.rValue.?,
                bodyBlockName,
                endBlockName,
            }) catch unreachable,
            self.c.lastBlockIndex(),
        );
        self.c.emitBlock(endBlockName);

        return Expression{
            .t = types.SimpleType.create(self.alloc, types.SimpleType.UNIT),
            .semiMustFollow = false,
            .endsWithReturn = false,
            .hasLValue = false,
            .callsFunction = exp.callsFunction or bodyCallsFunction,
        };
    }

    fn parseBody(self: *Parser) SyntaxError!Expression {
        self.st.open() catch unreachable;
        defer self.st.close();
        try self.consume(tt.LBRACE);

        var exp = try self.parseExpression();
        errdefer exp.destroy(self.alloc);

        try self.consume(tt.RBRACE);

        exp.semiMustFollow = false;
        return exp;
    }

    fn parseReturn(self: *Parser) SyntaxError!Expression {
        const ret = self.consumeGet(tt.RETURN) catch unreachable;

        var nextBlock = self.c.genLLVMName("block");
        defer self.alloc.free(nextBlock);

        return switch (self.s.peek().tokenType) {
            tt.SEMICOLON, tt.RPAREN, tt.RBRACK, tt.RBRACE, tt.COMMA, tt.COLON => blk: {
                if (!self.returnType.?.isUnit()) {
                    self.report(
                        ret,
                        reporter.Level.ERROR,
                        "function must return value of type '{s}' instead of unit",
                        .{self.returnType.?.str()},
                        true,
                        true,
                    );
                    return SyntaxError.TypeError;
                }
                self.c.emit(
                    std.fmt.allocPrint(self.alloc, "ret void", .{}) catch unreachable,
                    self.c.lastBlockIndex(),
                );
                break :blk Expression{
                    .t = types.SimpleType.create(self.alloc, types.SimpleType.UNIT),
                    .endsWithReturn = true,
                    .semiMustFollow = true,
                };
            },
            else => blk: {
                var retExp = try self.parseSubExpression();
                defer retExp.destroy(self.alloc);

                const value = self.convert(
                    retExp.t.?,
                    self.returnType.?,
                    ConvMode.IMPLICIT,
                    retExp.rValue.?,
                    self.c.lastBlockIndex(),
                ) catch |err| {
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
                            "cannot cast value of type {s} to {s}",
                            .{ retExp.t.?.str(), self.returnType.?.str() },
                            true,
                            true,
                        ),
                    }

                    return SyntaxError.TypeError;
                };

                defer self.alloc.free(value);

                if (self.returnType.?.isUnit()) {
                    self.c.emit(
                        std.fmt.allocPrint(self.alloc, "ret void", .{}) catch unreachable,
                        self.c.lastBlockIndex(),
                    );
                } else {
                    self.c.emit(
                        std.fmt.allocPrint(self.alloc, "ret {s} {s}", .{
                            codegen.llvmType(self.returnType.?.*),
                            value,
                        }) catch unreachable,
                        self.c.lastBlockIndex(),
                    );
                }

                self.c.emitWaitingBlock(nextBlock);

                break :blk Expression{
                    .t = types.SimpleType.create(self.alloc, types.SimpleType.UNIT),
                    .endsWithReturn = true,
                    .semiMustFollow = true,
                    .callsFunction = retExp.callsFunction,
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

        self.c.emit(
            std.fmt.allocPrint(self.alloc, "br label {s}", .{label}) catch unreachable,
            self.c.lastBlockIndex(),
        );

        var nBlock = self.c.genLLVMName("block");
        defer self.alloc.free(nBlock);

        self.c.emitBlock(nBlock);

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

        self.c.emit(
            std.fmt.allocPrint(self.alloc, "br label {s}", .{label}) catch unreachable,
            self.c.lastBlockIndex(),
        );

        var nBlock = self.c.genLLVMName("block");
        defer self.alloc.free(nBlock);

        self.c.emitBlock(nBlock);

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

        var tok = self.s.peek();
        if (types.startsType(tok.symbol)) {
            // Current token under the cursor is either a simple type or an array type.
            identType = try self.parseType();
            errdefer identType.?.destroy(self.alloc);

            identTok = try self.consumeGet(tt.IDENT);
            defer identType.?.destroy(self.alloc);

            var llvmName = try self.declare(identTok.?.symbol, identType.?.clone(self.alloc), identTok.?, false);

            exp.t = identType.?.clone(self.alloc);
            exp.semiMustFollow = true;
            exp.hasLValue = true;
            exp.lValue = llvmName;

            if (!identType.?.isUnit()) {
                self.c.emitInit(
                    std.fmt.allocPrint(self.alloc, "{s} = alloca {s}", .{
                        llvmName,
                        codegen.llvmType(identType.?.*),
                    }) catch unreachable,
                );
            }
        } else {
            declaration = false;
            // TODO: Bus error here?
            exp = try self.parseTernaryExpression();
        }

        var assignTok = self.s.peek();
        if (!token.TokenType.isAssignment(assignTok.tokenType)) {
            return exp;
        }

        if (!exp.hasLValue) {
            self.report(assignTok, reporter.Level.ERROR, "invalid left-hand side to assignment", .{}, true, true);
            return SyntaxError.TypeError;
        }

        switch (assignTok.tokenType) {
            tt.ASSIGN => {
                self.consume(tt.ASSIGN) catch unreachable;
                var subExp = try self.parseSubExpression();
                defer subExp.destroy(self.alloc);

                if (exp.t.?.isUnit() and subExp.t.?.isUnit()) {} else {
                    var value = self.convert(subExp.t.?, exp.t.?, ConvMode.IMPLICIT, subExp.rValue.?, self.c.lastBlockIndex()) catch |err| {
                        switch (err) {
                            ConvErr.Overflow => self.report(
                                assignTok,
                                reporter.Level.ERROR,
                                "cannot cast value of type {s} to {s} due to possible overflow",
                                .{ subExp.t.?.str(), exp.t.?.str() },
                                true,
                                true,
                            ),
                            ConvErr.NoImplicit => self.report(
                                assignTok,
                                reporter.Level.ERROR,
                                "cannot cast value of type {s} to {s}",
                                .{ subExp.t.?.str(), exp.t.?.str() },
                                true,
                                true,
                            ),
                        }

                        return SyntaxError.TypeError;
                    };

                    self.c.emit(
                        std.fmt.allocPrint(self.alloc, "store {s} {s}, ptr {s}", .{
                            codegen.llvmType(exp.t.?.*),
                            value,
                            exp.lValue.?,
                        }) catch unreachable,
                        self.c.lastBlockIndex(),
                    );

                    self.alloc.free(value);
                }

                exp.t.?.destroy(self.alloc);
                exp.t = subExp.t.?.clone(self.alloc);

                exp.destroyRValue(self.alloc);
                exp.rValue = self.createString("{s}", .{subExp.rValue.?});

                // exp.lValue does not change.
                exp.hasLValue = true;
                exp.semiMustFollow = true;
                exp.callsFunction = exp.callsFunction or subExp.callsFunction;
            },
            tt.ADD_ASSIGN, tt.SUB_ASSIGN, tt.MUL_ASSIGN, tt.QUO_ASSIGN, tt.REM_ASSIGN, tt.LSH_ASSIGN, tt.RSH_ASSIGN, tt.AND_ASSIGN, tt.OR_ASSIGN, tt.XOR_ASSIGN => {
                self.consume(assignTok.tokenType) catch unreachable;
                var rhsExp = try self.parseSubExpression();
                defer rhsExp.destroy(self.alloc);

                if (!exp.lt.?.isIntegral() and !exp.lt.?.isBool() or exp.lt.?.isConstant()) {
                    self.report(tok, reporter.Level.ERROR, "{s} is allowed only for integral types, not for {s}", .{ assignTok.symbol, exp.lt.?.str() }, true, true);
                    return SyntaxError.TypeError;
                }

                if (!rhsExp.t.?.isIntegral() and !rhsExp.t.?.isBool()) {
                    self.report(tok, reporter.Level.ERROR, "{s} is allowed only for integral types, not for {s}", .{ assignTok.symbol, exp.lt.?.str() }, true, true);
                    return SyntaxError.TypeError;
                }

                const value = self.convert(rhsExp.t.?, exp.lt.?, ConvMode.IMPLICIT, rhsExp.rValue.?, self.c.lastBlockIndex()) catch |err| {
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
                defer self.alloc.free(value);

                var rvalue = self.emitOpAssign(assignTok.tokenType, exp, rhsExp, value);

                exp.semiMustFollow = true;

                exp.destroyRValue(self.alloc);
                exp.rValue = rvalue;

                exp.hasLValue = true;
                exp.semiMustFollow = true;
                exp.endsWithReturn = false;
                exp.callsFunction = exp.callsFunction or rhsExp.callsFunction;
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
                defer rhsExp.destroy(self.alloc);

                if (!rhsExp.t.?.equals(types.Type{ .simple = types.SimpleType.BOOL })) {
                    self.report(tok, reporter.Level.ERROR, "{s} is allowed only for booleans, not for {s}", .{ assignTok.symbol, exp.lt.?.str() }, true, true);
                    return SyntaxError.TypeError;
                }

                exp.destroyRValue(self.alloc);
                exp.rValue = self.createString("{s}", .{rhsExp.rValue.?});

                exp.hasLValue = true;
                exp.semiMustFollow = true;
                exp.endsWithReturn = false;
                exp.callsFunction = exp.callsFunction or rhsExp.callsFunction;
            },
            else => @panic("ICE: exhausted all the possible assignments"),
        }

        return exp;
    }

    // Defines an identifier in te currently open scope. If a symbol with the same identifier
    // is already defined in the current scope, error is logged instead. Returns the name of the identifier
    // in the LLVM IR.
    fn declare(self: *Parser, ident: []const u8, t: *types.Type, at: token.Token, defined: bool) SyntaxError![]const u8 {
        if (self.st.declared(ident)) {
            var prevDef = self.st.get(ident).?;
            self.report(
                at,
                reporter.Level.ERROR,
                "'{s}' already declared at {d}:{d}",
                .{ ident, prevDef.location.sourceLoc.line, prevDef.location.sourceLoc.column },
                true,
                true,
            );
            return SyntaxError.TypeError;
        }
        var s = symbols.Symbol{
            .name = ident,
            .llvmName = self.c.genLLVMName(ident),
            .location = at,
            .t = t,
            .defined = defined,
        };
        self.st.insert(s) catch unreachable;

        return self.createString("{s}", .{s.llvmName});
    }

    // E^2
    fn parseTernaryExpression(self: *Parser) SyntaxError!Expression {
        // exp is potentially a boolen condition for the ternary operator.
        var exp = try self.parseLogicExpressions();
        errdefer exp.destroy(self.alloc);

        var tok = self.s.peek();
        if (tok.tokenType != tt.QUESTION_MARK and tok.tokenType != tt.D_QUESTION_MARK) {
            return exp;
        }

        if (!exp.t.?.isBool()) {
            self.report(tok, reporter.Level.ERROR, "non-boolean condition in a ternary operator statement", .{}, true, true);
            return SyntaxError.TypeError;
        }

        return switch (tok.tokenType) {
            tt.D_QUESTION_MARK => blk: {
                self.consume(tt.D_QUESTION_MARK) catch unreachable;

                var exp2 = try self.parseExpression();
                defer exp2.destroy(self.alloc);

                try self.consume(tt.COLON);

                var exp3 = try self.parseTernaryExpression();
                defer exp3.destroy(self.alloc);

                var t: *types.Type = types.leastSupertype(self.alloc, exp2.t.?, exp3.t.?) orelse {
                    self.report(tok, reporter.Level.ERROR, "no common supertype for '{s}' and '{s}'", .{ exp2.t.?.str(), exp3.t.?.str() }, true, true);
                    return SyntaxError.TypeError;
                };

                if (exp2.hasLValue and exp3.hasLValue and exp2.lt.?.equals(exp3.lt.?.*)) {
                    if (!exp2.lt.?.equals(types.Type{ .simple = types.SimpleType.UNIT })) {}
                }

                // Destroy the previous type and replace it with the new supertype.
                exp.destroy(self.alloc);
                exp.t = t;
                exp.lt = if (exp2.lt.?.equals(exp3.lt.?.*)) exp2.lt.?.clone(self.alloc) else null;
                // TODO: Expression attribute rValue: result of `select` instruction.
                exp.hasLValue = if (exp2.lt.?.equals(exp3.lt.?.*)) true else false;
                // TODO: exp.hasLValue = ...
                // TODO: exp.lValue = ...
                exp.semiMustFollow = true;
                exp.endsWithReturn = false;
                exp.callsFunction = exp.callsFunction or exp2.callsFunction or exp3.callsFunction;
                break :blk exp;
            },
            tt.QUESTION_MARK => blk: {
                self.consume(tt.QUESTION_MARK) catch unreachable;

                var thenBlockName = self.c.genLLVMName("then");
                var elseBlockName = self.c.genLLVMName("else");
                var nextBlockName = self.c.genLLVMName("next");

                defer self.alloc.free(thenBlockName);
                defer self.alloc.free(elseBlockName);
                defer self.alloc.free(nextBlockName);

                self.c.emit(
                    std.fmt.allocPrint(self.alloc, "br i1 {s}, label {s}, label {s}", .{
                        exp.rValue.?,
                        thenBlockName,
                        elseBlockName,
                    }) catch unreachable,
                    self.c.lastBlockIndex(),
                );

                self.c.emitBlock(thenBlockName);

                var exp2 = try self.parseExpression();
                defer exp2.destroy(self.alloc);

                var thenBackpatchingIndex = self.c.segments.items.len - 1;
                self.c.newSegment();
                self.c.emit(
                    std.fmt.allocPrint(self.alloc, "br label {s}", .{nextBlockName}) catch unreachable,
                    self.c.lastBlockIndex(),
                );

                var jumpFromThen = std.fmt.allocPrint(self.alloc, "{s}", .{self.c.currentBlock}) catch unreachable;
                self.alloc.free(jumpFromThen);

                try self.consume(tt.COLON);

                self.c.emitBlock(elseBlockName);

                var exp3 = try self.parseTernaryExpression();
                defer exp3.destroy(self.alloc);

                var elseBackpatchingIndex = self.c.segments.items.len - 1;

                self.c.newSegment();
                self.c.emit(
                    std.fmt.allocPrint(self.alloc, "br label {s}", .{nextBlockName}) catch unreachable,
                    self.c.lastBlockIndex(),
                );

                var jumpFromElse = std.fmt.allocPrint(self.alloc, "{s}", .{self.c.currentBlock}) catch unreachable;
                self.alloc.free(jumpFromElse);

                self.c.emitBlock(nextBlockName);

                var t: *types.Type = types.leastSupertype(self.alloc, exp2.t.?, exp3.t.?) orelse {
                    self.report(tok, reporter.Level.ERROR, "no common supertype for '{s}' and '{s}'", .{ exp2.t.?.str(), exp3.t.?.str() }, true, true);
                    return SyntaxError.TypeError;
                };

                // TODO: Improve error handling.
                var thenConv = self.convert(exp2.t.?, t, ConvMode.IMPLICIT, exp2.rValue.?, thenBackpatchingIndex) catch return SyntaxError.TypeError;
                defer self.alloc.free(thenConv);

                var elseConv = self.convert(exp3.t.?, t, ConvMode.IMPLICIT, exp3.rValue.?, elseBackpatchingIndex) catch return SyntaxError.TypeError;
                defer self.alloc.free(elseConv);

                var result: ?[]const u8 = null;
                if (exp2.hasLValue and exp3.hasLValue and exp2.lt.?.equals(exp3.lt.?.*)) {
                    if (!exp2.lt.?.isUnit()) {
                        result = self.c.genLLVMNameEmpty();
                        self.c.emit(
                            std.fmt.allocPrint(self.alloc, "{s} = phi ptr [ {s}, {s} ], [ {s}, {s} ]", .{
                                result.?,
                                exp2.lValue.?,
                                jumpFromThen,
                                exp3.lValue.?,
                                jumpFromElse,
                            }) catch unreachable,
                            self.c.lastBlockIndex(),
                        );
                    }
                }

                if (t.isConstant()) {
                    var newT = types.minimumSignedType(self.alloc, t.constant.value);
                    t.destroy(self.alloc);
                    t = newT;
                }

                if (!t.isUnit()) {
                    result = self.c.genLLVMNameEmpty();

                    self.c.emit(
                        std.fmt.allocPrint(self.alloc, "{s} = phi {s} [ {s}, {s} ], [ {s}, {s} ]", .{
                            result.?,
                            codegen.llvmType(t.*),
                            thenConv,
                            jumpFromThen,
                            elseConv,
                            jumpFromElse,
                        }) catch unreachable,
                        self.c.lastBlockIndex(),
                    );
                }

                // Destroy the previous type and replace it with the new supertype.
                exp.t.?.destroy(self.alloc);
                exp.lt.?.destroy(self.alloc);
                exp.t = t;
                exp.lt = if (exp2.lt.?.equals(exp3.lt.?.*)) exp2.lt.?.clone(self.alloc) else null;

                // TODO: exp.lValue = ...
                // TODO: exp.hasLValue = ...
                exp.hasLValue = if (exp2.lt.?.equals(exp3.lt.?.*)) true else false;

                exp.destroyRValue(self.alloc);
                exp.rValue = result;

                // TODO: exp.lValue = ...
                exp.semiMustFollow = true;
                exp.endsWithReturn = false;
                exp.callsFunction = exp.callsFunction or exp2.callsFunction or exp3.callsFunction;

                break :blk exp;
            },
            else => unreachable,
        };
    }

    // E^3, binary and, or, ||, &&
    fn parseLogicExpressions(self: *Parser) SyntaxError!Expression {
        var exp = try self.parseNot();

        var tok = self.s.peek();
        switch (tok.tokenType) {
            tt.LAND, tt.LOR, tt.AND, tt.OR => {},
            else => return exp,
        }
        errdefer exp.destroy(self.alloc);

        if (!exp.t.?.equals(types.Type{ .simple = types.SimpleType.BOOL })) {
            self.report(tok, reporter.Level.ERROR, "cannot perform '{s}' on non-boolean values", .{tok.str()}, true, true);
            return SyntaxError.TypeError;
        }

        var lastValue: []const u8 = std.fmt.allocPrint(self.alloc, "{s}", .{exp.rValue.?}) catch unreachable;
        var inheritedEnd: ?[]const u8 = null;
        var end: ?[]const u8 = null;
        var intermediateResult: ?[]const u8 = null;
        var nextBlock: ?[]const u8 = null;
        var lastBlock: ?[]const u8 = null;

        defer {
            self.alloc.free(lastValue);
            if (nextBlock) |nb| self.alloc.free(nb);
            if (lastBlock) |lb| self.alloc.free(lb);
            if (end) |e| self.alloc.free(e);
            if (inheritedEnd) |ie| self.alloc.free(ie);
            if (intermediateResult) |ir| self.alloc.free(ir);
        }

        var toEnd = std.ArrayList([]const u8).init(self.alloc);
        defer {
            for (toEnd.items) |te| self.alloc.free(te);
            toEnd.deinit();
        }

        while (true) {
            tok = self.s.peek();
            switch (tok.tokenType) {
                tt.LAND => {
                    if (inheritedEnd) |iEnd| {
                        if (end) |e| self.alloc.free(e);
                        end = std.fmt.allocPrint(self.alloc, "{s}", .{iEnd}) catch unreachable;

                        self.consume(tt.LAND) catch unreachable;

                        var nExp = try self.parseNot();
                        defer nExp.destroy(self.alloc);

                        if (!nExp.t.?.equals(types.Type{ .simple = types.SimpleType.BOOL })) {
                            self.report(tok, reporter.Level.ERROR, "cannot perform '{s}' on non-boolean values", .{tok.str()}, true, true);
                            return SyntaxError.TypeError;
                        }

                        exp.callsFunction = exp.callsFunction or nExp.callsFunction;

                        self.alloc.free(lastValue);
                        lastValue = std.fmt.allocPrint(self.alloc, "{s}", .{exp.rValue.?}) catch unreachable;
                    } else {
                        for (toEnd.items) |te| self.alloc.free(te);
                        toEnd.resize(0) catch unreachable;

                        if (end) |e| self.alloc.free(e);
                        end = self.c.genLLVMNameEmpty();
                    }

                    while (self.s.peek().tokenType == tok.tokenType) {
                        toEnd.append(
                            std.fmt.allocPrint(self.alloc, "{s}", .{self.c.currentBlock}) catch unreachable,
                        ) catch unreachable;

                        if (nextBlock) |nb| self.alloc.free(nb);
                        nextBlock = self.c.genLLVMNameEmpty();

                        self.c.emit(
                            std.fmt.allocPrint(self.alloc, "br i1 {s}, label {s}, label {s}", .{
                                lastValue,
                                nextBlock.?,
                                end.?,
                            }) catch unreachable,
                            self.c.lastBlockIndex(),
                        );

                        self.consume(tok.tokenType) catch unreachable;

                        self.c.emitBlock(nextBlock.?);

                        var nExp = try self.parseNot();
                        defer nExp.destroy(self.alloc);

                        exp.callsFunction = exp.callsFunction or nExp.callsFunction;

                        self.alloc.free(lastValue);
                        lastValue = std.fmt.allocPrint(self.alloc, "{s}", .{nExp.rValue.?}) catch unreachable;

                        if (!nExp.t.?.equals(types.Type{ .simple = types.SimpleType.BOOL })) {
                            self.report(tok, reporter.Level.ERROR, "cannot perform '{s}' on non-boolean values", .{tok.str()}, true, true);
                            return SyntaxError.TypeError;
                        }
                    }

                    if (lastBlock) |lb| self.alloc.free(lb);
                    lastBlock = std.fmt.allocPrint(self.alloc, "{s}", .{self.c.currentBlock}) catch unreachable;

                    if (self.s.peek().tokenType == tt.LOR) {
                        if (inheritedEnd) |ie| self.alloc.free(ie);
                        inheritedEnd = self.c.genLLVMNameEmpty();

                        for (toEnd.items) |te| self.alloc.free(te);
                        toEnd.resize(0) catch unreachable;
                        toEnd.append(
                            std.fmt.allocPrint(self.alloc, "{s}", .{self.c.currentBlock}) catch unreachable,
                        ) catch unreachable;

                        self.c.emit(
                            std.fmt.allocPrint(self.alloc, "br i1 {s}, label {s}, label {s}", .{
                                lastValue,
                                inheritedEnd.?,
                                end.?,
                            }) catch unreachable,
                            self.c.lastBlockIndex(),
                        );

                        self.c.emitBlock(end.?);

                        self.alloc.free(lastValue);
                        lastValue = std.fmt.allocPrint(self.alloc, "0", .{}) catch unreachable;

                        continue;
                    }

                    self.c.emit(
                        std.fmt.allocPrint(self.alloc, "br label {s}", .{end.?}) catch unreachable,
                        self.c.lastBlockIndex(),
                    );
                    self.c.emitBlock(end.?);

                    if (intermediateResult) |ir| self.alloc.free(ir);
                    intermediateResult = self.c.genLLVMNameEmpty();

                    self.c.emitA(std.fmt.allocPrint(
                        self.alloc,
                        "  {s} = phi i1 [ {s}, {s} ]",
                        .{ intermediateResult.?, lastValue, lastBlock.? },
                    ) catch unreachable);

                    for (toEnd.items) |e|
                        self.c.emitA(std.fmt.allocPrint(self.alloc, ", [ 0, {s} ]", .{e}) catch unreachable);

                    self.c.emitA(std.fmt.allocPrint(self.alloc, "\n", .{}) catch unreachable);

                    self.alloc.free(lastValue);
                    lastValue = std.fmt.allocPrint(self.alloc, "{s}", .{intermediateResult.?}) catch unreachable;

                    if (inheritedEnd) |ie| self.alloc.free(ie);
                    inheritedEnd = null;
                },
                tt.LOR => {
                    if (inheritedEnd) |iEnd| {
                        if (end) |e| self.alloc.free(e);
                        end = std.fmt.allocPrint(self.alloc, "{s}", .{iEnd}) catch unreachable;

                        self.consume(tt.LOR) catch unreachable;

                        var nExp = try self.parseNot();
                        defer nExp.destroy(self.alloc);

                        if (!nExp.t.?.equals(types.Type{ .simple = types.SimpleType.BOOL })) {
                            self.report(tok, reporter.Level.ERROR, "cannot perform '{s}' on non-boolean values", .{tok.str()}, true, true);
                            return SyntaxError.TypeError;
                        }

                        exp.callsFunction = exp.callsFunction or nExp.callsFunction;

                        self.alloc.free(lastValue);
                        lastValue = std.fmt.allocPrint(self.alloc, "{s}", .{exp.rValue.?}) catch unreachable;
                    } else {
                        for (toEnd.items) |te| self.alloc.free(te);
                        toEnd.resize(0) catch unreachable;

                        if (end) |e| self.alloc.free(e);
                        end = self.c.genLLVMNameEmpty();
                    }

                    while (self.s.peek().tokenType == tt.LOR) {
                        toEnd.append(
                            std.fmt.allocPrint(self.alloc, "{s}", .{self.c.currentBlock}) catch unreachable,
                        ) catch unreachable;

                        if (nextBlock) |nb| self.alloc.free(nb);
                        nextBlock = self.c.genLLVMNameEmpty();

                        self.c.emit(
                            std.fmt.allocPrint(self.alloc, "br i1 {s}, label {s}, label {s}", .{
                                lastValue,
                                end.?,
                                nextBlock.?,
                            }) catch unreachable,
                            self.c.lastBlockIndex(),
                        );

                        self.consume(tt.LOR) catch unreachable;

                        self.c.emitBlock(nextBlock.?);

                        var nExp = try self.parseNot();
                        defer nExp.destroy(self.alloc);

                        exp.callsFunction = exp.callsFunction or nExp.callsFunction;

                        self.alloc.free(lastValue);
                        lastValue = std.fmt.allocPrint(self.alloc, "{s}", .{nExp.rValue.?}) catch unreachable;

                        if (!nExp.t.?.equals(types.Type{ .simple = types.SimpleType.BOOL })) {
                            self.report(tok, reporter.Level.ERROR, "cannot perform '{s}' on non-boolean values", .{tok.str()}, true, true);
                            return SyntaxError.TypeError;
                        }
                    }

                    if (lastBlock) |lb| self.alloc.free(lb);
                    lastBlock = std.fmt.allocPrint(self.alloc, "{s}", .{self.c.currentBlock}) catch unreachable;

                    if (self.s.peek().tokenType == tt.LAND) {
                        if (inheritedEnd) |ie| self.alloc.free(ie);
                        inheritedEnd = self.c.genLLVMNameEmpty();

                        for (toEnd.items) |te| self.alloc.free(te);
                        toEnd.resize(0) catch unreachable;
                        toEnd.append(
                            std.fmt.allocPrint(self.alloc, "{s}", .{self.c.currentBlock}) catch unreachable,
                        ) catch unreachable;

                        self.c.emit(
                            std.fmt.allocPrint(self.alloc, "br i1 {s}, label {s}, label {s}", .{
                                lastValue,
                                end.?,
                                inheritedEnd.?,
                            }) catch unreachable,
                            self.c.lastBlockIndex(),
                        );

                        self.c.emitBlock(end.?);

                        self.alloc.free(lastValue);
                        lastValue = std.fmt.allocPrint(self.alloc, "1", .{}) catch unreachable;

                        continue;
                    }

                    self.c.emit(
                        std.fmt.allocPrint(self.alloc, "br label {s}", .{end.?}) catch unreachable,
                        self.c.lastBlockIndex(),
                    );
                    self.c.emitBlock(end.?);

                    if (intermediateResult) |ir| self.alloc.free(ir);
                    intermediateResult = self.c.genLLVMNameEmpty();

                    self.c.emitA(std.fmt.allocPrint(
                        self.alloc,
                        "  {s} = phi i1 [ {s}, {s} ]",
                        .{ intermediateResult.?, lastValue, lastBlock.? },
                    ) catch unreachable);

                    for (toEnd.items) |e|
                        self.c.emitA(std.fmt.allocPrint(self.alloc, ", [ 1, {s} ]", .{e}) catch unreachable);

                    self.c.emitA(std.fmt.allocPrint(self.alloc, "\n", .{}) catch unreachable);

                    self.alloc.free(lastValue);
                    lastValue = std.fmt.allocPrint(self.alloc, "{s}", .{intermediateResult.?}) catch unreachable;

                    if (inheritedEnd) |ie| self.alloc.free(ie);
                    inheritedEnd = null;
                },
                tt.OR, tt.AND => {
                    self.consume(tok.tokenType) catch unreachable;

                    var nExp = try self.parseNot();
                    defer nExp.destroy(self.alloc);

                    exp.callsFunction = exp.callsFunction or nExp.callsFunction;

                    if (!nExp.t.?.equals(types.Type{ .simple = types.SimpleType.BOOL })) {
                        self.report(tok, reporter.Level.ERROR, "cannot perform '{s}' on non-boolean values", .{tok.str()}, true, true);
                        return SyntaxError.TypeError;
                    }

                    intermediateResult = self.c.genLLVMNameEmpty();
                    self.c.emit(
                        std.fmt.allocPrint(self.alloc, "{s} = {s} i1 {s}, {s}", .{
                            intermediateResult.?,
                            if (tok.tokenType == tt.OR) "or" else "and",
                            lastValue,
                            nExp.rValue.?,
                        }) catch unreachable,
                        self.c.lastBlockIndex(),
                    );

                    self.alloc.free(lastValue);
                    lastValue = std.fmt.allocPrint(self.alloc, "{s}", .{intermediateResult.?}) catch unreachable;
                },
                else => {
                    exp.destroy(self.alloc);
                    return Expression{
                        .t = types.SimpleType.create(self.alloc, types.SimpleType.BOOL),
                        .rValue = std.fmt.allocPrint(self.alloc, "{s}", .{lastValue}) catch unreachable,
                        .hasLValue = false,
                        .semiMustFollow = true,
                        .endsWithReturn = false,
                        .callsFunction = exp.callsFunction,
                    };
                },
            }
        }
    }

    // E^4, unary not
    fn parseNot(self: *Parser) SyntaxError!Expression {
        const tok = self.s.peek();
        if (tok.tokenType == tt.NOT) {
            self.consume(tt.NOT) catch unreachable;

            var exp = try self.parseNot();
            errdefer exp.destroy(self.alloc);

            if (!exp.t.?.equals(types.Type{ .simple = types.SimpleType.BOOL })) {
                self.report(tok, reporter.Level.ERROR, "can negate only booleans", .{}, true, true);
                return SyntaxError.TypeError;
            }

            var result = self.c.genLLVMNameEmpty();

            self.c.emit(
                std.fmt.allocPrint(self.alloc, "{s} = xor i1 {s}, -1", .{ result, exp.rValue.? }) catch unreachable,
                self.c.lastBlockIndex(),
            );

            exp.destroyRValue(self.alloc);
            exp.rValue = result;

            exp.destroyLValue(self.alloc);

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

        if (!tt.isRelational(self.s.peek().tokenType)) {
            return xExp;
        }

        var accResult: ?[]const u8 = null; // Accumulated result of all the comparisons together.
        var result: ?[]const u8 = null; // Current result.
        defer if (result) |res| self.alloc.free(res);
        defer if (accResult) |ar| self.alloc.free(ar);

        while (tt.isRelational(self.s.peek().tokenType)) {
            var op = self.s.next();
            var yExp = try self.parseArithmeticExpression();
            defer yExp.destroy(self.alloc);

            // FIXME: No common supertype for array and array is found.
            var t: *types.Type = types.leastSupertype(self.alloc, xExp.t.?, yExp.t.?) orelse {
                self.report(op, reporter.Level.ERROR, "no common supertype for '{s}' and '{s}'", .{ xExp.t.?.str(), yExp.t.?.str() }, true, true);
                return SyntaxError.TypeError;
            };
            defer t.destroy(self.alloc);

            const valX = self.convert(xExp.t.?, t, ConvMode.IMPLICIT, xExp.rValue.?, self.c.lastBlockIndex()) catch |err| {
                // TODO: Error is reported at the wrong token. Fix this by adding token pointing to the start of an expression.
                switch (err) {
                    ConvErr.Overflow => self.report(op, reporter.Level.ERROR, "cannot cast value of type {s} to {s} due to possible overflow", .{ xExp.t.?.str(), t.str() }, true, true),
                    ConvErr.NoImplicit => self.report(op, reporter.Level.ERROR, "cannot cast value value of type {s} to {s}", .{ xExp.t.?.str(), t.str() }, true, true),
                }
                return SyntaxError.TypeError;
            };
            defer self.alloc.free(valX);

            const valY = self.convert(yExp.t.?, t, ConvMode.IMPLICIT, yExp.rValue.?, self.c.lastBlockIndex()) catch |err| {
                // TODO: Error is reported at the wrong token. Fix this by adding token pointing to the start of an expression.
                switch (err) {
                    ConvErr.Overflow => self.report(op, reporter.Level.ERROR, "cannot cast value of type {s} to {s} due to possible overflow", .{ yExp.t.?.str(), t.str() }, true, true),
                    ConvErr.NoImplicit => self.report(op, reporter.Level.ERROR, "cannot cast value of type {s} to {s}", .{ yExp.t.?.str(), t.str() }, true, true),
                }
                return SyntaxError.TypeError;
            };
            defer self.alloc.free(valY);

            switch (t.*) {
                types.TypeTag.simple => |simpleType| {
                    if (simpleType == types.SimpleType.UNIT) {
                        switch (op.tokenType) {
                            tt.EQL => result = self.createString("1", .{}),
                            tt.NEQ => result = self.createString("0", .{}),
                            // Even though we hit type error, we can continue with the parsing.
                            else => self.report(op, reporter.Level.ERROR, "unit type can be only tested for equality and inequality", .{}, true, true),
                        }
                    } else if (t.isFloat() or t.isDouble()) {
                        const inst = codegen.llvmFloatOp(op.tokenType);
                        if (result) |res| self.alloc.free(res);
                        result = self.c.genLLVMNameEmpty();

                        self.c.emit(
                            self.createString("{s} = fcmp {s} {s} {s}, {s}", .{
                                result.?,
                                inst,
                                codegen.llvmType(t.*),
                                valX,
                                valY,
                            }),
                            self.c.lastBlockIndex(),
                        );
                    } else {
                        // Type must be one of [u64...u8, i64...i8, bool].
                        if (result) |res| self.alloc.free(res);
                        result = self.c.genLLVMNameEmpty();

                        const prefix = if (op.tokenType == tt.EQL or op.tokenType == tt.NEQ) "" else if (t.isSigned()) "s" else "u";
                        const llvmOp = codegen.llvmIntOp(op.tokenType);

                        self.c.emit(
                            self.createString("{s} = icmp {s}{s} {s} {s}, {s}", .{
                                result.?,
                                prefix,
                                llvmOp,
                                codegen.llvmType(t.*),
                                valX,
                                valY,
                            }),
                            self.c.lastBlockIndex(),
                        );
                    }
                },
                types.TypeTag.constant => {
                    if (result) |res| self.alloc.free(res);
                    result = self.evalConstant(op.tokenType, xExp.rValue.?, yExp.rValue.?);
                },
                types.TypeTag.array => {
                    if (result) |res| self.alloc.free(res);
                    result = self.c.genLLVMNameEmpty();

                    switch (op.tokenType) {
                        tt.EQL => self.c.emit(
                            self.createString("{s} = icmp eq ptr {s}, {s}", .{ result.?, valX, valY }),
                            self.c.lastBlockIndex(),
                        ),
                        tt.NEQ => self.c.emit(
                            self.createString("{s} = icmp ne ptr {s}, {s}", .{ result.?, valX, valY }),
                            self.c.lastBlockIndex(),
                        ),
                        else => self.report(op, reporter.Level.ERROR, "pointer type can be only tested for equality and inequality", .{}, true, true),
                    }
                },
                // The only way we get here is if the common least supertype is function and that cannot happen.
                else => unreachable,
            }

            if (accResult) |ar| {
                // Combine current result with previous results.
                var newAccRes = self.c.genLLVMNameEmpty();
                self.c.emit(
                    self.createString("{s} = and i1 {s}, {s}", .{ newAccRes, accResult.?, result.? }),
                    self.c.lastBlockIndex(),
                );
                self.alloc.free(ar);
                accResult = newAccRes;
            } else {
                if (accResult) |ar| self.alloc.free(ar);
                accResult = self.createString("{s}", .{result.?});
            }

            // Replace the type of the expression with the new supertype.
            xExp.destroy(self.alloc);
            xExp = yExp.clone(self.alloc);
            xExp.callsFunction = xExp.callsFunction or yExp.callsFunction;
        }

        xExp.t.?.destroy(self.alloc);
        xExp.t = types.SimpleType.create(self.alloc, types.SimpleType.BOOL);

        xExp.destroyRValue(self.alloc);
        xExp.rValue = self.createString("{s}", .{accResult.?});

        xExp.destroyLValue(self.alloc);

        xExp.semiMustFollow = true;
        xExp.endsWithReturn = false;
        return xExp;
    }

    // E^6, +, -, >>, <<, &, ^, |
    fn parseArithmeticExpression(self: *Parser) SyntaxError!Expression {
        var xExp = try self.parseUnaryOperators();
        errdefer xExp.destroy(self.alloc);

        var tok = self.s.peek();
        if (!tt.isLowerPrioArithmetic(tok.tokenType)) {
            return xExp;
        }

        if (xExp.t.?.isArray() or xExp.t.?.equals(types.Type{ .simple = types.SimpleType.UNIT })) {
            self.report(tok, reporter.Level.ERROR, "cannot perform '{s}' on '{s}'", .{ tok.symbol, xExp.t.?.str() }, true, true);
            return SyntaxError.TypeError;
        }

        var result: ?[]const u8 = null;
        while (tt.isLowerPrioArithmetic(self.s.peek().tokenType)) {
            var op = self.s.next();

            var yExp = try self.parseUnaryOperators();
            defer yExp.destroy(self.alloc);

            if (yExp.t.?.isArray() or yExp.t.?.equals(types.Type{ .simple = types.SimpleType.UNIT })) {
                self.report(op, reporter.Level.ERROR, "cannot perform '{s}' on '{s}'", .{ tok.symbol, xExp.t.?.str() }, true, true);
                return SyntaxError.TypeError;
            }

            var t: *types.Type = types.leastSupertype(self.alloc, xExp.t.?, yExp.t.?) orelse {
                self.report(op, reporter.Level.ERROR, "no common supertype for '{s}' and '{s}'", .{ xExp.t.?.str(), yExp.t.?.str() }, true, true);
                return SyntaxError.TypeError;
            };

            if (t.equals(types.Type{ .simple = types.SimpleType.BOOL }) and op.tokenType != tt.B_AND and op.tokenType != tt.B_OR and op.tokenType != tt.B_XOR) {
                self.report(op, reporter.Level.ERROR, "cannot perform '{s}' on '{s}'", .{ tok.symbol, t.str() }, true, true);
                return SyntaxError.TypeError;
            }

            if ((t.equals(types.Type{ .simple = types.SimpleType.DOUBLE }) or t.equals(types.Type{ .simple = types.SimpleType.FLOAT })) and (op.tokenType != tt.ADD or op.tokenType != tt.SUB)) {
                self.report(op, reporter.Level.ERROR, "cannot perform '{s}' on '{s}'", .{ tok.symbol, t.str() }, true, true);
                return SyntaxError.TypeError;
            }

            if (t.isConstant()) {
                var a = self.evalConstant(op.tokenType, xExp.lValue.?, yExp.lValue.?);
                self.alloc.free(a);
                // TODO: Set expression attributes.
            } else {
                const valX = self.convert(xExp.t.?, t, ConvMode.IMPLICIT, xExp.rValue.?, self.c.lastBlockIndex()) catch |err| {
                    // TODO: Error is reported at the wrong token. Fix this by adding token pointing to the start of an expression.
                    switch (err) {
                        ConvErr.Overflow => self.report(tok, reporter.Level.ERROR, "cannot cast value of type {s} to {s} due to possible overflow", .{ xExp.t.?.str(), t.str() }, true, true),
                        ConvErr.NoImplicit => self.report(tok, reporter.Level.ERROR, "cannot cast value value of type {s} to {s}", .{ xExp.t.?.str(), t.str() }, true, true),
                    }
                    return SyntaxError.TypeError;
                };
                defer self.alloc.free(valX);

                const valY = self.convert(yExp.t.?, t, ConvMode.IMPLICIT, yExp.rValue.?, self.c.lastBlockIndex()) catch |err| {
                    // TODO: Error is reported at the wrong token. Fix this by adding token pointing to the start of an expression.
                    switch (err) {
                        ConvErr.Overflow => self.report(op, reporter.Level.ERROR, "cannot cast value of type {s} to {s} due to possible overflow", .{ yExp.t.?.str(), t.str() }, true, true),
                        ConvErr.NoImplicit => self.report(op, reporter.Level.ERROR, "cannot cast value value of type {s} to {s}", .{ yExp.t.?.str(), t.str() }, true, true),
                    }
                    return SyntaxError.TypeError;
                };
                defer self.alloc.free(valY);

                if (result) |res| self.alloc.free(res);
                result = self.c.genLLVMNameEmpty();

                const opPrefix = if (op.tokenType == tt.B_RSH) if (t.isSigned()) "a" else "l" else "";
                const llvmType = codegen.llvmType(t.*);
                const llvmOp = if (t.isFloat() or t.isDouble())
                    codegen.llvmFloatOp(op.tokenType)
                else
                    codegen.llvmIntOp(op.tokenType);

                self.c.emit(
                    self.createString("{s} = {s}{s} {s} {s}, {s}", .{ result.?, opPrefix, llvmOp, llvmType, valX, valY }),
                    self.c.lastBlockIndex(),
                );
            }
            // Replace the type of the expression with the new supertype.
            xExp.t.?.destroy(self.alloc);
            xExp.t = t;
            xExp.callsFunction = xExp.callsFunction or yExp.callsFunction;
        }

        xExp.destroyRValue(self.alloc);
        xExp.rValue = result.?;

        xExp.destroyLValue(self.alloc);

        xExp.semiMustFollow = true;
        xExp.endsWithReturn = false;
        return xExp;
    }

    // E^7, -, !
    fn parseUnaryOperators(self: *Parser) SyntaxError!Expression {
        const tok = self.s.peek();
        switch (tok.tokenType) {
            tt.SUB => {
                self.consume(tt.SUB) catch unreachable;

                var exp = try self.parseUnaryOperators();
                errdefer exp.destroy(self.alloc);

                if (!exp.t.?.isIntegral() or exp.t.?.equals(types.Type{ .simple = types.SimpleType.U64 })) {
                    self.report(tok, reporter.Level.ERROR, "cannot perform unary minus on '{s}'", .{exp.t.?.str()}, true, true);
                    return SyntaxError.TypeError;
                }

                if (exp.t.?.isConstant()) {
                    exp.t.?.constant.value = -exp.t.?.constant.value;
                } else {
                    var result = self.c.genLLVMNameEmpty();
                    defer self.alloc.free(result);

                    if (exp.t.?.isDouble() or exp.t.?.isFloat()) {
                        self.c.emit(
                            self.createString("{s} = fneg {s} {s}", .{ result, codegen.llvmType(exp.t.?.*), exp.rValue.? }),
                            self.c.lastBlockIndex(),
                        );
                    } else if (exp.t.?.isSigned()) {
                        self.c.emit(
                            self.createString("{s} = sub {s} 0, {s}", .{ result, codegen.llvmType(exp.t.?.*), exp.rValue.? }),
                            self.c.lastBlockIndex(),
                        );
                    } else if (!exp.t.?.isSigned()) {
                        var t = if (exp.t.?.equals(types.Type{ .simple = types.SimpleType.U8 }))
                            types.SimpleType.create(self.alloc, types.SimpleType.I16)
                        else if (exp.t.?.equals(types.Type{ .simple = types.SimpleType.U16 }))
                            types.SimpleType.create(self.alloc, types.SimpleType.I32)
                        else
                            types.SimpleType.create(self.alloc, types.SimpleType.I64);

                        var tmp = self.c.genLLVMNameEmpty();
                        defer self.alloc.free(tmp);

                        self.c.emit(
                            self.createString("{s} = zext {s} {s} to {s}", .{
                                tmp,
                                codegen.llvmType(exp.t.?.*),
                                exp.rValue.?,
                                codegen.llvmType(t.*),
                            }),
                            self.c.lastBlockIndex(),
                        );

                        self.c.emit(
                            self.createString("{s} = sub {s} 0, {s}", .{ result, codegen.llvmType(t.*), tmp }),
                            self.c.lastBlockIndex(),
                        );

                        exp.t.?.destroy(self.alloc);
                        exp.t = t;

                        exp.destroyRValue(self.alloc);
                        exp.rValue = self.createString("{s}", .{result});
                    } else unreachable;
                }

                exp.destroyLValue(self.alloc);

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
                    return SyntaxError.TypeError;
                }

                if (exp.t.?.isConstant()) {
                    exp.t.?.constant.value = ~exp.t.?.constant.value;
                } else {
                    var result = self.c.genLLVMNameEmpty();
                    self.c.emit(
                        self.createString("{s} = xor {s} {s}, -1", .{
                            result,
                            codegen.llvmType(exp.t.?.*),
                            exp.rValue.?,
                        }),
                        self.c.lastBlockIndex(),
                    );

                    exp.destroyRValue(self.alloc);
                    exp.rValue = result;
                }

                exp.destroyLValue(self.alloc);

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

        var tok = self.s.peek();
        if (!tt.isHigherPrioArithmetic(tok.tokenType)) {
            return xExp;
        }

        if (xExp.t.?.isArray() or xExp.t.?.isUnit() or xExp.t.?.isBool()) {
            self.report(tok, reporter.Level.ERROR, "cannot perform '{s}' on '{s}'", .{ tok.symbol, xExp.t.?.str() }, true, true);
            return SyntaxError.TypeError;
        }

        var result: ?[]const u8 = null;
        defer if (result) |res| self.alloc.free(res);

        while (tt.isHigherPrioArithmetic(self.s.peek().tokenType)) {
            var op = self.s.next();
            var yExp = try self.parseArrayIndexingAndPrefixExpressions();
            defer yExp.destroy(self.alloc);

            if (yExp.t.?.isArray() or yExp.t.?.isUnit() or yExp.t.?.isBool()) {
                self.report(tok, reporter.Level.ERROR, "cannot perform '{s}' on '{s}'", .{ op.symbol, yExp.t.?.str() }, true, true);
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
                const valX = self.convert(xExp.t.?, t, ConvMode.IMPLICIT, xExp.rValue.?, self.c.lastBlockIndex()) catch |err| {
                    // TODO: Error is reported at the wrong token. Fix this by adding token pointing to the start of an expression.
                    switch (err) {
                        ConvErr.Overflow => self.report(tok, reporter.Level.ERROR, "cannot cast value of type {s} to {s} due to possible overflow", .{ xExp.t.?.str(), t.str() }, true, true),
                        ConvErr.NoImplicit => self.report(tok, reporter.Level.ERROR, "cannot cast value value of type {s} to {s}", .{ xExp.t.?.str(), t.str() }, true, true),
                    }
                    return SyntaxError.TypeError;
                };
                defer self.alloc.free(valX);

                const valY = self.convert(yExp.t.?, t, ConvMode.IMPLICIT, yExp.rValue.?, self.c.lastBlockIndex()) catch |err| {
                    // TODO: Error is reported at the wrong token. Fix this by adding token pointing to the start of an expression.
                    switch (err) {
                        ConvErr.Overflow => self.report(op, reporter.Level.ERROR, "cannot cast value of type {s} to {s} due to possible overflow", .{ yExp.t.?.str(), t.str() }, true, true),
                        ConvErr.NoImplicit => self.report(op, reporter.Level.ERROR, "cannot cast value value of type {s} to {s}", .{ yExp.t.?.str(), t.str() }, true, true),
                    }
                    return SyntaxError.TypeError;
                };
                defer self.alloc.free(valY);

                // TODO: Emit IR depending on the operation and expression type.
                // Watch out, this is more convoluted - unary minus can be done on u8, u16 and u32,
                // resulting in i16, i32 and i64, respectively. Unary minus cannot be applied on u64
                // since there is no i128.
                if (result) |res| self.alloc.free(res);
                result = self.c.genLLVMNameEmpty();

                const opPrefix = if (!t.isDouble() and !t.isFloat()) if (t.isSigned()) "s" else "u" else "";
                const llvmType = codegen.llvmType(t.*);
                const llvmOp = if (t.isFloat() or t.isDouble())
                    codegen.llvmFloatOp(op.tokenType)
                else
                    codegen.llvmIntOp(op.tokenType);

                self.c.emit(
                    self.createString("{s} = {s}{s} {s} {s}, {s}", .{
                        result.?,
                        opPrefix,
                        llvmOp,
                        llvmType,
                        valX,
                        valY,
                    }),
                    self.c.lastBlockIndex(),
                );

                xExp.destroyRValue(self.alloc);
                xExp.rValue = self.createString("{s}", .{result.?});
            }

            // Replace the type of the expression with the new supertype.
            xExp.t.?.destroy(self.alloc);
            xExp.t = t;
            xExp.callsFunction = xExp.callsFunction or yExp.callsFunction;
        }

        xExp.destroyLValue(self.alloc);

        xExp.semiMustFollow = true;
        xExp.endsWithReturn = false;
        return xExp;
    }

    // E^9, array indexing and ++, --, #
    fn parseArrayIndexingAndPrefixExpressions(self: *Parser) SyntaxError!Expression {
        var exp = try self.parseI();
        errdefer exp.destroy(self.alloc);

        var result = self.c.genLLVMNameEmpty();
        defer self.alloc.free(result);

        var tok = self.s.peek();
        while (true) {
            switch (tok.tokenType) {
                tt.INC, tt.DEC => {
                    self.consume(tok.tokenType) catch unreachable;

                    if (!exp.hasLValue) {
                        self.report(tok, reporter.Level.ERROR, "must be l-value in order to be incremented or decremented", .{}, true, true);
                        return SyntaxError.TypeError;
                    }

                    const valX = self.convert(exp.lt.?, exp.t.?, ConvMode.IMPLICIT, exp.rValue.?, self.c.lastBlockIndex()) catch {
                        self.report(
                            tok,
                            reporter.Level.ERROR,
                            "cannot cast value of type {s} to {s}",
                            .{ exp.lt.?.str(), exp.t.?.str() },
                            true,
                            true,
                        );
                        return SyntaxError.TypeError;
                    };
                    defer self.alloc.free(valX);

                    if (!exp.lt.?.isIntegral()) {
                        self.report(tok, reporter.Level.ERROR, "value of type '{s}' cannot be incremented or decremented", .{exp.t.?.str()}, true, true);
                        return SyntaxError.TypeError;
                    }

                    const llvmType = codegen.llvmType(exp.lt.?.*);
                    const llvmOp = if (exp.lt.?.isDouble() or exp.lt.?.isFloat()) "fadd" else "add";
                    const llvmInc = if (tok.tokenType == tt.DEC)
                        (if (exp.lt.?.isDouble() or exp.lt.?.isFloat()) "-1.0" else "-1")
                    else
                        (if (exp.lt.?.isDouble() or exp.lt.?.isFloat()) "1.0" else "1");

                    self.c.emit(
                        self.createString("{s} = {s} {s} {s} {s}", .{ result, llvmOp, llvmType, valX, llvmInc }),
                        self.c.lastBlockIndex(),
                    );
                    self.c.emit(
                        self.createString("store {s} {s}, ptr {s}", .{ llvmType, result, exp.lValue.? }),
                        self.c.lastBlockIndex(),
                    );

                    exp.semiMustFollow = true;
                    exp.hasLValue = true;
                    exp.semiMustFollow = true;
                    exp.endsWithReturn = false;
                },
                tt.HASH => {
                    self.consume(tt.HASH) catch unreachable;
                    if (!exp.hasLValue) {
                        self.report(tok, reporter.Level.ERROR, "must be l-value in order to be referenced", .{}, true, true);
                        return SyntaxError.TypeError;
                    }

                    var t = types.Array.create(self.alloc, 0, exp.t.?);
                    exp.t.?.destroy(self.alloc);
                    exp.t = t;

                    // TODO: Is exp.rValue set correctly?
                    exp.destroyRValue(self.alloc);
                    exp.rValue = self.createString("{s}", .{exp.lValue.?});

                    exp.destroyLValue(self.alloc);

                    exp.semiMustFollow = true;
                    exp.endsWithReturn = false;
                },
                tt.LBRACK => {
                    self.consume(tt.LBRACK) catch unreachable;
                    if (!exp.t.?.isArray()) {
                        self.report(tok, reporter.Level.ERROR, "'{s}' cannot be indexed", .{exp.t.?.str()}, true, true);
                        return SyntaxError.TypeError;
                    }

                    if (self.s.peek().tokenType == tt.RBRACK) {
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
                        exp.hasLValue = true;
                        if (exp.lValue) |lv| self.alloc.free(lv);
                        exp.lValue = self.createString("{s}", .{exp.rValue.?});
                        // TODO: exp.rValue = name of the load instruction.

                        exp.semiMustFollow = true;
                        exp.endsWithReturn = false;
                        continue;
                    }

                    var indexes = std.ArrayList([]const u8).init(self.alloc);
                    defer {
                        for (indexes.items) |i| self.alloc.free(i);
                        indexes.deinit();
                    }

                    var next = self.s.peek();
                    while (next.tokenType != tt.RBRACK) {
                        var iExp = try self.parseExpression();
                        defer iExp.destroy(self.alloc);

                        if (iExp.t.?.equals(types.Type{ .simple = types.SimpleType.I64 }) or iExp.t.?.equals(types.Type{ .simple = types.SimpleType.U64 })) {
                            indexes.append(self.createString("{s}", .{iExp.rValue.?})) catch unreachable;
                        } else {
                            const t = types.SimpleType.create(self.alloc, types.SimpleType.I64);
                            defer t.destroy(self.alloc);
                            const value = self.convert(iExp.t.?, t, ConvMode.IMPLICIT, iExp.rValue.?, self.c.lastBlockIndex()) catch {
                                self.report(
                                    tok,
                                    reporter.Level.ERROR,
                                    "cannot cast value of type {s} to {s}",
                                    .{ iExp.t.?.str(), t.str() },
                                    true,
                                    true,
                                );

                                return SyntaxError.TypeError;
                            };
                            indexes.append(value) catch unreachable;
                        }

                        next = self.s.peek();
                        if (next.tokenType == tt.COMMA) {
                            self.consume(tt.COMMA) catch unreachable;

                            next = self.s.peek();
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

                        exp.setLValue(self.alloc, self.createString("TODO1", .{}));
                        exp.destroyLValue(self.alloc);

                        exp.hasLValue = true;
                        exp.semiMustFollow = true;
                        exp.endsWithReturn = false;
                        continue;
                    }

                    var where = self.createString("{s}", .{indexes.items[0]});
                    defer self.alloc.free(where);

                    for (1..exp.t.?.array.dimensions) |i| {
                        var dimension = self.c.genLLVMNameEmpty();
                        defer self.alloc.free(dimension);

                        self.c.emit(
                            self.createString("{s} = getelementptr i64, ptr {s}, i64 {d}", .{
                                dimension,
                                exp.rValue.?,
                                -@as(i128, @intCast(exp.t.?.array.dimensions)) + i,
                            }),
                            self.c.lastBlockIndex(),
                        );

                        var whereNew = self.c.genLLVMNameEmpty();
                        var tmp = self.c.genLLVMNameEmpty();

                        defer self.alloc.free(whereNew);
                        defer self.alloc.free(tmp);

                        self.c.emit(
                            self.createString("{s} = mul i64 {s}, {s}", .{ tmp, where, dimension }),
                            self.c.lastBlockIndex(),
                        );

                        self.c.emit(
                            self.createString("{s} = add i64 {s}, {s}", .{ whereNew, tmp, indexes.items[i] }),
                            self.c.lastBlockIndex(),
                        );

                        self.alloc.free(where);
                        where = self.createString("{s}", .{whereNew});
                    }

                    var wherePtr = self.c.genLLVMNameEmpty();
                    self.c.emit(
                        self.createString("{s} = getelementptr {s}, ptr {s}, i64 {s}", .{
                            wherePtr,
                            codegen.llvmType(exp.t.?.array.ofType.*),
                            exp.rValue.?,
                            where,
                        }),
                        self.c.lastBlockIndex(),
                    );

                    exp.lt.?.destroy(self.alloc);
                    exp.lt = exp.t.?.array.ofType.clone(self.alloc);
                    exp.t = blk: {
                        var ofType = exp.t.?.array.ofType.clone(self.alloc);
                        exp.t.?.destroy(self.alloc);
                        break :blk ofType;
                    };
                    // TODO: exp.rValue = ...

                    exp.destroyLValue(self.alloc);
                    exp.hasLValue = true;
                    exp.lValue = wherePtr;

                    exp.semiMustFollow = true;
                    exp.endsWithReturn = false;
                },
                else => return exp,
            }
            tok = self.s.peek();
        }
    }

    fn parseI(self: *Parser) SyntaxError!Expression {
        var next = self.s.peek();
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
                            .semiMustFollow = true,
                            .endsWithReturn = false,
                        };
                    }

                    var explicitLoad = self.c.genLLVMNameEmpty();
                    self.c.emit(
                        self.createString("{s} = load {s}, ptr {s}", .{
                            explicitLoad,
                            codegen.llvmType(s.t.*),
                            s.llvmName,
                        }),
                        self.c.lastBlockIndex(),
                    );

                    return Expression{
                        .t = s.t.clone(self.alloc),
                        .lt = s.t.clone(self.alloc),
                        .hasLValue = true,
                        .lValue = self.createString("{s}", .{s.llvmName}),
                        .rValue = explicitLoad,
                        .semiMustFollow = true,
                        .endsWithReturn = false,
                    };
                }
            },
            tt.C_INT => {
                const int = self.consumeGet(tt.C_INT) catch unreachable;
                const value = std.fmt.parseInt(i128, int.symbol, 0) catch unreachable;

                return Expression{
                    .t = types.Constant.create(self.alloc, value),
                    .lt = types.Constant.create(self.alloc, value),
                    .rValue = self.createString("{s}", .{int.symbol}),
                    .hasLValue = false,
                    .semiMustFollow = true,
                    .endsWithReturn = false,
                };
            },
            tt.C_FLOAT => {
                const int = self.consumeGet(tt.C_FLOAT) catch unreachable;

                return Expression{
                    .t = types.SimpleType.create(self.alloc, types.SimpleType.DOUBLE),
                    .lt = types.SimpleType.create(self.alloc, types.SimpleType.DOUBLE),
                    .rValue = self.createString("{s}", .{int.symbol}),
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
                    .lt = types.SimpleType.create(self.alloc, types.SimpleType.BOOL),
                    .rValue = self.createString("{s}", .{value}),
                    .hasLValue = false,
                    .semiMustFollow = true,
                    .endsWithReturn = false,
                };
            },
            tt.AT => {
                const at = self.consumeGet(tt.AT) catch unreachable;

                if (types.startsType(self.s.peek().symbol)) {
                    // We're dealing with explicit type conversion.

                    var newT = try self.parseType();
                    defer newT.destroy(self.alloc);

                    var cExp = try self.parseI();
                    errdefer cExp.destroy(self.alloc);

                    const result = self.convert(cExp.t.?, newT, ConvMode.EXPLICIT, cExp.rValue.?, self.c.lastBlockIndex()) catch |err| {
                        // TODO: Error is reported at the wrong token. Fix this by adding token pointing to the start of an expression.
                        switch (err) {
                            ConvErr.Overflow => self.report(at, reporter.Level.ERROR, "cannot cast value of type {s} to {s} due to possible overflow", .{ cExp.t.?.str(), newT.str() }, true, true),
                            ConvErr.NoImplicit => self.report(at, reporter.Level.ERROR, "cannot cast value value of type {s} to {s}", .{ cExp.t.?.str(), newT.str() }, true, true),
                        }

                        return SyntaxError.TypeError;
                    };

                    cExp.t.?.destroy(self.alloc);
                    cExp.t = newT.clone(self.alloc);

                    cExp.destroyLValue(self.alloc);

                    cExp.destroyRValue(self.alloc);
                    cExp.rValue = result;

                    cExp.semiMustFollow = true;
                    cExp.endsWithReturn = false;
                    return cExp;
                }
                // We're dealing with builtin functions.
                const ident = try self.consumeGet(tt.IDENT);
                _ = ident;
                // TODO: Emit IR for calling builtin functions.
                return Expression{
                    .t = types.SimpleType.create(self.alloc, types.SimpleType.UNIT),
                    .hasLValue = false,
                    .rValue = self.createString("TODO: Name of the instruction", .{}),
                    .semiMustFollow = true,
                    .endsWithReturn = false,
                    .callsFunction = true,
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
                    .rValue = self.createString("{s}", .{char.symbol}),
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
            .rValue = self.createString("TODO: name of a call instruction", .{}),
            .hasLValue = false,
            .semiMustFollow = true,
            .endsWithReturn = false,
            .callsFunction = true,
        };
    }

    fn consume(self: *Parser, want: tt) SyntaxError!void {
        _ = try self.consumeGet(want);
    }

    fn consumeGet(self: *Parser, want: tt) SyntaxError!scanner.Token {
        const next = self.s.next();

        if (next.tokenType == tt.ILLEGAL) {
            self.report(next, reporter.Level.ERROR, "encountered illegal symbol '{s}'", .{next.symbol}, true, true);
        }

        if (want != next.tokenType) {
            self.report(next, reporter.Level.ERROR, "expected '{s}' but found '{s}' instead", .{ tt.str(want), next.symbol }, true, true);
            return SyntaxError.UnexpectedToken;
        }

        return next;
    }

    fn report(self: *Parser, tok: token.Token, level: reporter.Level, comptime fmt: []const u8, args: anytype, showLine: bool, space: bool) void {
        // TODO: When reporting, denote in what stage the error happened - scanning, parsing, typechecking, code generation?
        const msg = std.fmt.allocPrint(self.alloc, fmt, args) catch unreachable;
        defer self.alloc.free(msg);

        if (self.r) |rep| {
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
        blockIndex: usize,
    ) ConvErr![]const u8 {
        if (from.equals(to.*)) {
            return self.createString("{s}", .{what});
        }

        if (from.isUnit()) {
            // Unit is not implicitely convertible to anything.
            if (conv == ConvMode.IMPLICIT) return ConvErr.NoImplicit;

            if (to.isArray()) return std.fmt.allocPrint(self.alloc, "null", .{}) catch unreachable;
            if (to.isDouble() or to.isFloat()) return std.fmt.allocPrint(self.alloc, "0.0", .{}) catch unreachable;
            return std.fmt.allocPrint(self.alloc, "0", .{}) catch unreachable;
        }

        if (to.isUnit()) {
            // Cannot implicitely convert to unit.
            if (conv == ConvMode.IMPLICIT) return ConvErr.NoImplicit;
            return std.fmt.allocPrint(self.alloc, "{s}", .{what}) catch unreachable;
        }

        var result = self.c.genLLVMNameEmpty();
        errdefer self.alloc.free(result);

        var temp = self.c.genLLVMNameEmpty();
        defer self.alloc.free(temp);

        if (from.isBool()) {
            if (conv == ConvMode.IMPLICIT) return ConvErr.NoImplicit;

            if (to.isNumeric()) {
                self.c.emit(
                    std.fmt.allocPrint(self.alloc, "{s} = zext i1 {s} to {s}", .{ result, what, codegen.llvmType(to.*) }) catch unreachable,
                    blockIndex,
                );
            } else if (to.isFloat() and conv == ConvMode.BITCAST) {
                self.c.emit(
                    std.fmt.allocPrint(self.alloc, "{s} = zext i1 {s} to i32", .{ temp, what }) catch unreachable,
                    blockIndex,
                );
                self.c.emit(
                    std.fmt.allocPrint(self.alloc, "{s} = bitcast i32 {s} to float", .{ result, temp }) catch unreachable,
                    blockIndex,
                );
            } else if (to.isDouble() and conv == ConvMode.BITCAST) {
                self.c.emit(
                    std.fmt.allocPrint(self.alloc, "{s} = zext i1 {s} to i64", .{ temp, what }) catch unreachable,
                    blockIndex,
                );
                self.c.emit(
                    std.fmt.allocPrint(self.alloc, "{s} = bitcast i64 {s} to double", .{ result, temp }) catch unreachable,
                    blockIndex,
                );
            } else if (to.isFloat() or to.isDouble()) {
                // Conversion must be explicit.
                self.c.emit(
                    std.fmt.allocPrint(self.alloc, "{s} = select i1 {s} to {s} 1.0 {s} 0.0", .{ result, what, codegen.llvmType(to.*), codegen.llvmType(to.*) }) catch unreachable,
                    blockIndex,
                );
            } else if (to.isArray() and conv == ConvMode.EXPLICIT) {
                self.c.emit(
                    std.fmt.allocPrint(self.alloc, "{s} = getelementptr i8, ptr null, i32 1", .{temp}) catch unreachable,
                    blockIndex,
                );
                self.c.emit(
                    std.fmt.allocPrint(self.alloc, "{s} = select i1 {s}, ptr {s}, ptr null", .{ result, what, temp }) catch unreachable,
                    blockIndex,
                );
            } else if (to.isArray()) {
                // Coversion must be bitcast.
                self.c.emit(
                    std.fmt.allocPrint(self.alloc, "{s} = inttoptr i1 {s} to ptr", .{ result, what }) catch unreachable,
                    blockIndex,
                );
            } else unreachable;

            return result;
        }

        if (from.isPointer()) {
            if (to.isArray()) {
                self.alloc.free(result);
                return std.fmt.allocPrint(self.alloc, "{s}", .{what}) catch unreachable;
            }
            if (conv == ConvMode.IMPLICIT) return ConvErr.NoImplicit;
            if (conv == ConvMode.EXPLICIT) {
                self.alloc.free(result);
                if (to.isDouble() or to.isFloat()) return std.fmt.allocPrint(self.alloc, "0.0", .{}) catch unreachable;
                return std.fmt.allocPrint(self.alloc, "0", .{}) catch unreachable;
            }

            // Conversion must be bitcast.
            if (to.isNumeric() or to.isBool()) {
                self.c.emit(
                    std.fmt.allocPrint(self.alloc, "{s} = ptrtoint ptr null to {s}", .{ result, codegen.llvmType(to.*) }) catch unreachable,
                    blockIndex,
                );
            } else if (to.isFloat()) {
                self.c.emit(
                    std.fmt.allocPrint(self.alloc, "{s} = ptrtoint ptr null to i32", .{temp}) catch unreachable,
                    blockIndex,
                );
                self.c.emit(
                    std.fmt.allocPrint(self.alloc, "{s} = bitcast i32 {s} to float", .{ result, temp }) catch unreachable,
                    blockIndex,
                );
            } else if (to.isDouble()) {
                self.c.emit(
                    std.fmt.allocPrint(self.alloc, "{s} = ptrtoint ptr null to i64", .{temp}) catch unreachable,
                    blockIndex,
                );
                self.c.emit(
                    std.fmt.allocPrint(self.alloc, "{s} = bitcast i64 {s} to double", .{ result, temp }) catch unreachable,
                    blockIndex,
                );
            } else unreachable;

            return result;
        }

        if (from.isArray() or from.isPointer()) {
            if (conv == ConvMode.IMPLICIT) return ConvErr.NoImplicit;

            if (to.isArray()) {
                self.alloc.free(result);
                return self.createString("{s}", .{what});
            }

            if (to.isBool() and conv == ConvMode.EXPLICIT) {
                self.c.emit(
                    std.fmt.allocPrint(self.alloc, "{s} = icmp ne ptr {s}, null ", .{ result, what }) catch unreachable,
                    blockIndex,
                );
            } else if (to.isBool() or to.isNumeric()) {
                self.c.emit(
                    std.fmt.allocPrint(self.alloc, "{s} = ptrtoint {s} to {s}", .{ result, what, codegen.llvmType(to.*) }) catch unreachable,
                    blockIndex,
                );
            } else if (to.isFloat()) {
                self.c.emit(
                    std.fmt.allocPrint(self.alloc, "{s} = ptrtoint ptr {s} to i32", .{ temp, what }) catch unreachable,
                    blockIndex,
                );
                self.c.emit(
                    std.fmt.allocPrint(self.alloc, "{s} = bitcast i32 {s} to float", .{ result, temp }) catch unreachable,
                    blockIndex,
                );
            } else if (to.isDouble()) {
                self.c.emit(
                    std.fmt.allocPrint(self.alloc, "{s} = ptrtoint ptr {s} to i64", .{ temp, what }) catch unreachable,
                    blockIndex,
                );
                self.c.emit(
                    std.fmt.allocPrint(self.alloc, "{s} = bitcast i32 {s} to double", .{ result, temp }) catch unreachable,
                    blockIndex,
                );
            } else unreachable;

            return result;
        }

        if (from.isConstant()) {
            if (to.isNumeric()) {
                if (types.checkBounds(to.simple, from.constant.value)) {
                    self.alloc.free(result);
                    return self.createString("{d}", .{from.constant.value});
                }
                return ConvErr.Overflow;
            }

            if (to.isFloat()) {
                // TODO: To hexadecimal?
                if (conv != ConvMode.BITCAST) {
                    self.alloc.free(result);
                    return self.createString("{d}", .{from.constant.value});
                }

                // TODO: Truncate and cast properly.
                var tmp: i32 = @truncate(from.constant.value);
                self.c.emit(
                    self.createString("{s} = bitcast i32 {d} to float", .{ result, tmp }),
                    blockIndex,
                );

                return result;
            }

            if (to.isDouble()) {
                // TODO: To hexadecimal?
                if (conv != ConvMode.BITCAST) {
                    self.alloc.free(result);
                    return self.createString("{d}", .{from.constant.value});
                }

                // TODO: Truncate and cast properly.
                var tmp: i64 = @truncate(from.constant.value);
                self.c.emit(
                    self.createString("{s} = bitcast i32 {d} to float", .{ result, tmp }),
                    blockIndex,
                );

                return result;
            }

            if (conv == ConvMode.IMPLICIT) return ConvErr.NoImplicit;

            if (to.isBool() and conv == ConvMode.EXPLICIT) {
                self.alloc.free(result);

                if (from.constant.value == 0) return self.createString("false", .{});
                return self.createString("true", .{});
            }

            if (to.isBool()) {
                // Conversion must be bitcast.
                self.alloc.free(result);

                if (@mod(from.constant.value, 2) == 0) return self.createString("false", .{});
                return self.createString("true", .{});
            }

            if (to.isArray()) {
                var ptr: u64 = @intCast(from.constant.value);
                self.c.emit(
                    self.createString("{s} = inttoptr i64 {d} to ptr", .{ result, ptr }),
                    blockIndex,
                );

                return result;
            }

            unreachable;
        }

        if (!from.isSigned()) {
            if (to.simple.width() > from.simple.width()) {
                // If `to` has greater bit width than `from`.
                self.c.emit(
                    std.fmt.allocPrint(self.alloc, "{s} = zext {s} {s} to {s}", .{
                        result,
                        codegen.llvmType(from.*),
                        what,
                        codegen.llvmType(to.*),
                    }) catch unreachable,
                    blockIndex,
                );

                return result;
            }

            if (to.isFloat() or to.isDouble()) {
                if (conv != ConvMode.BITCAST) {
                    self.c.emit(
                        std.fmt.allocPrint(self.alloc, "{s} = uitofp {s} {s} to {s}", .{
                            result,
                            codegen.llvmType(from.*),
                            what,
                            codegen.llvmType(to.*),
                        }) catch unreachable,
                        blockIndex,
                    );

                    return result;
                }

                var t = if (to.isFloat())
                    types.SimpleType.create(self.alloc, types.SimpleType.U32)
                else
                    types.SimpleType.create(self.alloc, types.SimpleType.U64);
                defer t.destroy(self.alloc);

                if (from.simple.width() > t.simple.width()) {
                    // If `from` has greater bit width than `t`.
                    self.c.emit(
                        std.fmt.allocPrint(self.alloc, "{s} = trunc {s} {s} to {s}", .{
                            temp,
                            codegen.llvmType(from.*),
                            what,
                            codegen.llvmType(t.*),
                        }) catch unreachable,
                        blockIndex,
                    );
                } else if (from.simple.width() < t.simple.width()) {
                    // If `from` has smaller bit width than `t`.
                    self.c.emit(
                        std.fmt.allocPrint(self.alloc, "{s} = zext {s} {s} to {s}", .{
                            temp,
                            codegen.llvmType(from.*),
                            what,
                            codegen.llvmType(t.*),
                        }) catch unreachable,
                        blockIndex,
                    );
                } else {
                    self.alloc.free(temp);
                    temp = what;
                }
                self.c.emit(
                    std.fmt.allocPrint(self.alloc, "{s} = bitcast {s} {s} to {s}", .{
                        result,
                        codegen.llvmType(t.*),
                        temp,
                        codegen.llvmType(to.*),
                    }) catch unreachable,
                    blockIndex,
                );

                return result;
            }

            if (conv == ConvMode.IMPLICIT) return ConvErr.NoImplicit;
            if (to.isNumeric()) {
                // `to` is numeric and has the same bit width as from.
                return self.createString("{s}", .{what});
            }

            if (to.isBool() and conv == ConvMode.EXPLICIT) {
                self.c.emit(
                    std.fmt.allocPrint(self.alloc, "{s} = icmp ne {s} {s}, 0", .{
                        result,
                        codegen.llvmType(from.*),
                        what,
                    }) catch unreachable,
                    blockIndex,
                );
            } else if (to.isBool() or to.isNumeric()) {
                self.c.emit(
                    std.fmt.allocPrint(self.alloc, "{s} = trunc {s} {s} to {s}", .{
                        result,
                        codegen.llvmType(from.*),
                        what,
                        codegen.llvmType(to.*),
                    }) catch unreachable,
                    blockIndex,
                );
            } else if (to.isArray()) {
                self.c.emit(
                    std.fmt.allocPrint(self.alloc, "{s} = inttoptr {s} {s} to ptr", .{
                        result,
                        codegen.llvmType(from.*),
                        what,
                    }) catch unreachable,
                    blockIndex,
                );
            } else unreachable;

            return result;
        }

        if (from.isSigned()) {
            if (to.isSigned() and to.simple.width() > from.simple.width()) {
                // `to` is numeric and has greater bit width than `from`
                if (conv != ConvMode.BITCAST) {
                    self.c.emit(
                        std.fmt.allocPrint(self.alloc, "{s} = sext {s} {s} to {s}", .{
                            result,
                            codegen.llvmType(from.*),
                            what,
                            codegen.llvmType(to.*),
                        }) catch unreachable,
                        blockIndex,
                    );
                } else {
                    self.c.emit(
                        std.fmt.allocPrint(self.alloc, "{s} = zext {s} {s} to {s}", .{
                            result,
                            codegen.llvmType(from.*),
                            what,
                            codegen.llvmType(to.*),
                        }) catch unreachable,
                        blockIndex,
                    );
                }
                return result;
            }

            if (to.isFloat() or to.isDouble()) {
                if (conv != ConvMode.BITCAST) {
                    self.c.emit(
                        std.fmt.allocPrint(self.alloc, "{s} = sitofp {s} {s} to {s}", .{
                            result,
                            codegen.llvmType(from.*),
                            what,
                            codegen.llvmType(to.*),
                        }) catch unreachable,
                        blockIndex,
                    );

                    return result;
                }

                var t = if (to.isFloat())
                    types.SimpleType.create(self.alloc, types.SimpleType.U32)
                else
                    types.SimpleType.create(self.alloc, types.SimpleType.U64);
                defer t.destroy(self.alloc);

                if (from.simple.width() > t.simple.width()) {
                    // `from` has bit width greater than `t`.
                    self.c.emit(
                        std.fmt.allocPrint(self.alloc, "{s} = trunc {s} {s} to {s}", .{
                            temp,
                            codegen.llvmType(from.*),
                            what,
                            codegen.llvmType(t.*),
                        }) catch unreachable,
                        blockIndex,
                    );
                } else if (from.simple.width() < t.simple.width()) {
                    // `from` has bit width smaller than `t`.
                    self.c.emit(
                        std.fmt.allocPrint(self.alloc, "{s} = zext {s} {s} to {s}", .{
                            temp,
                            codegen.llvmType(from.*),
                            what,
                            codegen.llvmType(to.*),
                        }) catch unreachable,
                        blockIndex,
                    );
                } else {
                    // `from` and `t` have the same bid width.
                    temp = what;
                }

                self.c.emit(
                    std.fmt.allocPrint(self.alloc, "{s} = bitcast {s} {s} to {s}", .{
                        result,
                        codegen.llvmType(t.*),
                        temp,
                        codegen.llvmType(to.*),
                    }) catch unreachable,
                    blockIndex,
                );

                return result;
            }

            if (conv == ConvMode.IMPLICIT) return ConvErr.NoImplicit;

            if (to.isNumeric() and to.simple.width() > from.simple.width()) {
                // `to` is numeric and has greater bit width than `from`.
                self.c.emit(
                    std.fmt.allocPrint(self.alloc, "{s} = zext {s} {s} to {s}", .{
                        result,
                        codegen.llvmType(from.*),
                        what,
                        codegen.llvmType(to.*),
                    }) catch unreachable,
                    blockIndex,
                );

                return result;
            }

            if (to.isNumeric() and to.simple.width() == from.simple.width()) {
                // `to` is numeric and has the same bit width as `from`.
                self.alloc.free(result);
                return std.fmt.allocPrint(self.alloc, "{s}", .{what}) catch unreachable;
            }

            if (to.isBool() and conv == ConvMode.EXPLICIT) {
                self.c.emit(
                    std.fmt.allocPrint(self.alloc, "{s} = icmp ne {s} {s}, 0", .{
                        result,
                        codegen.llvmType(from.*),
                        what,
                    }) catch unreachable,
                    blockIndex,
                );
            } else if (to.isBool() or to.isNumeric()) {
                self.c.emit(
                    std.fmt.allocPrint(self.alloc, "{s} = trunc {s} {s} to {s}", .{
                        result,
                        codegen.llvmType(from.*),
                        what,
                        codegen.llvmType(to.*),
                    }) catch unreachable,
                    blockIndex,
                );
            } else if (to.isArray()) {
                self.c.emit(
                    std.fmt.allocPrint(self.alloc, "{s} = inttoptr {s} {s} to ptr", .{
                        result,
                        codegen.llvmType(from.*),
                        what,
                    }) catch unreachable,
                    blockIndex,
                );
            } else unreachable;

            return result;
        }

        if (from.isFloat() or from.isDouble()) {
            if (to.isFloat()) {
                if (conv != ConvMode.BITCAST) {
                    self.c.emit(
                        std.fmt.allocPrint(self.alloc, "{s} = fptrunc double {s} to float", .{
                            result,
                            what,
                        }) catch unreachable,
                        blockIndex,
                    );

                    return result;
                }

                self.c.emit(
                    std.fmt.allocPrint(self.alloc, "{s} = bitcast double {s} to i64", .{
                        temp,
                        what,
                    }) catch unreachable,
                    blockIndex,
                );

                var trunc = self.c.genLLVMNameEmpty();
                defer self.alloc.free(trunc);

                self.c.emit(
                    std.fmt.allocPrint(self.alloc, "{s} = trunc i64 {s} to i32", .{
                        trunc,
                        temp,
                    }) catch unreachable,
                    blockIndex,
                );

                self.c.emit(
                    std.fmt.allocPrint(self.alloc, "{s} = bitcast i32 {s} to float", .{
                        result,
                        trunc,
                    }) catch unreachable,
                    blockIndex,
                );

                return result;
            }

            if (to.isDouble()) {
                if (conv != ConvMode.BITCAST) {
                    self.c.emit(
                        std.fmt.allocPrint(self.alloc, "{s} = fpext float {s} to double", .{
                            result,
                            what,
                        }) catch unreachable,
                        blockIndex,
                    );

                    return result;
                }

                self.c.emit(
                    std.fmt.allocPrint(self.alloc, "{s} = bitcast float {s} to i32", .{
                        temp,
                        what,
                    }) catch unreachable,
                    blockIndex,
                );

                var ext = self.c.genLLVMNameEmpty();
                defer self.alloc.free(ext);

                self.c.emit(
                    std.fmt.allocPrint(self.alloc, "{s} = zext i32 {s} to i64", .{
                        ext,
                        temp,
                    }) catch unreachable,
                    blockIndex,
                );

                self.c.emit(
                    std.fmt.allocPrint(self.alloc, "{s} = bitcast i64 {s} to double", .{
                        result,
                        ext,
                    }) catch unreachable,
                    blockIndex,
                );

                return result;
            }

            if (conv == ConvMode.IMPLICIT) return ConvErr.NoImplicit;

            if (to.isNumeric() or (conv == ConvMode.BITCAST) and (to.isBool())) {
                if (conv != ConvMode.BITCAST) {
                    if (!to.isSigned()) {
                        self.c.emit(
                            std.fmt.allocPrint(self.alloc, "{s} = fptoui {s} {s} to {s}", .{
                                result,
                                codegen.llvmType(from.*),
                                what,
                                codegen.llvmType(to.*),
                            }) catch unreachable,
                            blockIndex,
                        );
                    } else {
                        self.c.emit(
                            std.fmt.allocPrint(self.alloc, "{s} = fptosi {s} {s} to {s}", .{
                                result,
                                codegen.llvmType(from.*),
                                what,
                                codegen.llvmType(to.*),
                            }) catch unreachable,
                            blockIndex,
                        );
                    }

                    return result;
                }

                var t = if (from.isFloat())
                    types.SimpleType.create(self.alloc, types.SimpleType.U32)
                else
                    types.SimpleType.create(self.alloc, types.SimpleType.U64);
                defer t.destroy(self.alloc);

                self.c.emit(
                    std.fmt.allocPrint(self.alloc, "{s} = bitcast {s} {s} to {s}", .{
                        temp,
                        codegen.llvmType(from.*),
                        what,
                        codegen.llvmType(t.*),
                    }) catch unreachable,
                    blockIndex,
                );

                if (to.simple.width() < t.simple.width()) {
                    // `to` has smaller bit width than `t`.
                    self.c.emit(
                        std.fmt.allocPrint(self.alloc, "{s} = trunc {s} {s} to {s}", .{
                            result,
                            codegen.llvmType(t.*),
                            temp,
                            codegen.llvmType(to.*),
                        }) catch unreachable,
                        blockIndex,
                    );
                } else if (to.simple.width() > t.simple.width()) {
                    // `to` has greater bit width than `t`.
                    self.c.emit(
                        std.fmt.allocPrint(self.alloc, "{s} = zext {s} {s} to {s}", .{
                            result,
                            codegen.llvmType(t.*),
                            temp,
                            codegen.llvmType(to.*),
                        }) catch unreachable,
                        blockIndex,
                    );
                } else {
                    // TODO: I think the construct `result = temp` won't work due to memory either leaking
                    // or being destroyed eagerly by the defer at the beginning of this function.
                    // FIXME: Fix me.
                    result = temp;
                }
            } else if (to.isBool()) {
                self.c.emit(
                    std.fmt.allocPrint(self.alloc, "{s} = fcmp une {s} {s}, 0.0", .{
                        result,
                        codegen.llvmType(from.*),
                        what,
                    }) catch unreachable,
                    blockIndex,
                );
            } else if (to.isArray()) {
                var t = if (from.isFloat())
                    types.SimpleType.create(self.alloc, types.SimpleType.U32)
                else
                    types.SimpleType.create(self.alloc, types.SimpleType.U64);
                defer t.destroy(self.alloc);

                self.c.emit(
                    std.fmt.allocPrint(self.alloc, "{s} = bitcast {s} {s} to {s}", .{
                        temp,
                        codegen.llvmType(from.*),
                        what,
                        codegen.llvmType(t.*),
                    }) catch unreachable,
                    blockIndex,
                );

                self.c.emit(
                    std.fmt.allocPrint(self.alloc, "{s} = inttoptr {s} {s} to ptr", .{
                        result,
                        codegen.llvmType(t.*),
                        temp,
                    }) catch unreachable,
                    blockIndex,
                );
            } else unreachable;

            return result;
        }

        unreachable;
    }

    fn emitOpAssign(
        self: *Parser,
        op: tt,
        lhs: Expression,
        rhs: Expression,
        converted: []const u8,
    ) []const u8 {
        var result = self.c.genLLVMNameEmpty();

        var llvmOp = codegen.llvmAssignOp(lhs.lt.?.*, op);
        var llvmT = codegen.llvmType(lhs.lt.?.*);

        self.c.emit(
            std.fmt.allocPrint(self.alloc, "{s} = {s} {s} {s}, {s}", .{
                result,
                llvmOp,
                llvmT,
                rhs.rValue.?,
                converted,
            }) catch unreachable,
            self.c.lastBlockIndex(),
        );

        self.c.emit(
            std.fmt.allocPrint(self.alloc, "store {s} {s}, ptr {s}", .{ llvmT, result, lhs.lValue.? }) catch unreachable,
            self.c.lastBlockIndex(),
        );

        return result;
    }

    fn evalConstant(
        self: *Parser,
        op: token.TokenType,
        lhs: []const u8,
        rhs: []const u8,
    ) []const u8 {
        const lhsInt = std.fmt.parseInt(i128, lhs, 0) catch unreachable;
        const rhsInt = std.fmt.parseInt(i128, rhs, 0) catch unreachable;

        return switch (op) {
            tt.EQL => if (lhsInt == rhsInt) self.createString("1", .{}) else self.createString("0", .{}),
            tt.LT => if (lhsInt < rhsInt) self.createString("1", .{}) else self.createString("0", .{}),
            tt.GT => if (lhsInt > rhsInt) self.createString("1", .{}) else self.createString("0", .{}),
            tt.NEQ => if (lhsInt != rhsInt) self.createString("1", .{}) else self.createString("0", .{}),
            tt.LEQ => if (lhsInt <= rhsInt) self.createString("1", .{}) else self.createString("0", .{}),
            tt.GEQ => if (lhsInt >= rhsInt) self.createString("1", .{}) else self.createString("0", .{}),
            else => unreachable,
        };

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

    }

    fn createString(self: *Parser, comptime fmt: []const u8, args: anytype) []const u8 {
        // TODO: Use this function during emitting to simplify the code.
        return std.fmt.allocPrint(self.alloc, fmt, args) catch unreachable;
    }
};
