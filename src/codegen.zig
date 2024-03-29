const std = @import("std");

const parser = @import("parser.zig");
const tokens = @import("token.zig");
const types = @import("types.zig");
const symbols = @import("symbols.zig");

const tt = tokens.TokenType;

/// CodeGen is responsible for maintaining the LLVM IR.
pub const CodeGen = struct {
    alloc: std.mem.Allocator,

    readySegments: std.ArrayList([]const u8),

    segments: std.ArrayList([]const u8),
    commandNum: u64 = 0,

    waitingBlock: []const u8,
    currentBlock: []const u8,
    waits: bool,

    /// init creates the CodeGen struct. The caller must call `deinit` to destroy
    /// all the related structures after using CodeGen.
    pub fn init(alloc: std.mem.Allocator) CodeGen {
        var c = CodeGen{
            .alloc = alloc,
            .readySegments = std.ArrayList([]const u8).init(alloc),
            .segments = std.ArrayList([]const u8).init(alloc),
            .waitingBlock = std.fmt.allocPrint(alloc, "", .{}) catch unreachable,
            .currentBlock = std.fmt.allocPrint(alloc, "", .{}) catch unreachable,
            .waits = false,
        };

        return c;
    }

    /// deinit destroys all the related structures.
    pub fn deinit(self: *CodeGen) void {
        for (self.segments.items) |s| self.alloc.free(s);
        for (self.readySegments.items) |s| self.alloc.free(s);

        self.segments.deinit();
        self.readySegments.deinit();

        self.alloc.free(self.waitingBlock);
        self.alloc.free(self.currentBlock);
    }

    /// newSegment creates a new segment to generate IR into.
    pub fn newSegment(self: *CodeGen) void {
        var s = std.fmt.allocPrint(self.alloc, "", .{}) catch unreachable;
        self.segments.append(s) catch unreachable;
    }

    /// concatActiveSegments concatenates all the currently active segments into one segment
    /// and moves it to `self.readySegments`. Active segments must be concatenated like
    /// this after a function has been completely parsed and analyzed.
    pub fn concatActiveSegments(self: *CodeGen) void {
        var rs = std.fmt.allocPrint(self.alloc, "", .{}) catch unreachable;

        for (self.segments.items) |s| {
            // Get rid of the segment since we do not need it anymore.
            defer self.alloc.free(s);

            var n = std.fmt.allocPrint(self.alloc, "{s}{s}", .{ rs, s }) catch unreachable;
            self.alloc.free(rs);
            rs = n;
        }

        self.segments.resize(0) catch unreachable;

        self.readySegments.append(rs) catch unreachable;
    }

    /// emitA takes a string and appends it immediately after the contents of the last segment.
    /// This function destroys the allocated string passed in.
    pub fn emitA(self: *CodeGen, string: []const u8) void {
        defer self.alloc.free(string);

        var s = self.segments.items[self.lastBlockIndex()];
        defer self.alloc.free(s);

        self.segments.items[self.lastBlockIndex()] = std.fmt.allocPrint(self.alloc, "{s}{s}", .{ s, string }) catch unreachable;
    }

    /// emitInit takes a string, adds two spaces in front and a newline after and appends it
    /// after the contents of the first segment. This function destroys the allocated string passed in.
    pub fn emitInit(self: *CodeGen, string: []const u8) void {
        defer self.alloc.free(string);

        var s = self.segments.items[0];
        defer self.alloc.free(s);

        self.segments.items[0] = std.fmt.allocPrint(self.alloc, "{s}  {s}\n", .{ s, string }) catch unreachable;
    }

    /// emitBlock emits instructions that open a new block with the provided name.
    /// The name must contain the percent sign.
    pub fn emitBlock(self: *CodeGen, blockName: []const u8) void {
        self.waits = false;

        var s = self.segments.items[self.lastBlockIndex()];
        defer self.alloc.free(s);

        self.segments.items[self.lastBlockIndex()] = std.fmt.allocPrint(self.alloc, "{s}\n{s}:\n", .{ s, blockName[1..] }) catch unreachable;

        self.alloc.free(self.currentBlock);
        self.currentBlock = std.fmt.allocPrint(self.alloc, "{s}", .{blockName}) catch unreachable;
    }

    pub fn emitWaitingBlock(self: *CodeGen, blockName: []const u8) void {
        self.waits = true;

        self.alloc.free(self.waitingBlock);
        self.waitingBlock = std.fmt.allocPrint(self.alloc, "{s}", .{blockName}) catch unreachable;

        self.alloc.free(self.currentBlock);
        self.currentBlock = std.fmt.allocPrint(self.alloc, "{s}", .{blockName}) catch unreachable;
    }

    pub fn emit(self: *CodeGen, string: []const u8, index: usize) void {
        defer self.alloc.free(string);

        if (self.waits) {
            self.waits = false;
            self.emitBlock(self.waitingBlock);
        }
        var s = self.segments.items[index];
        defer self.alloc.free(s);

        var ns = std.fmt.allocPrint(self.alloc, "{s}  {s}\n", .{ s, string }) catch unreachable;
        self.segments.items[index] = ns;
    }

    /// lastBlockIndex returns the index of the last LLVM block.
    pub fn lastBlockIndex(self: *CodeGen) usize {
        return self.segments.items.len - 1;
    }

    /// genLLVMName generates a unique LLVM name based on `name`.
    pub fn genLLVMName(self: *CodeGen, name: []const u8) []const u8 {
        var llvmName = std.fmt.allocPrint(self.alloc, "%{s}.{d}", .{ name, self.commandNum }) catch unreachable;
        self.commandNum += 1;
        return llvmName;
    }

    /// genLLVMNameEmpty generates a unique LLVM name.
    pub fn genLLVMNameEmpty(self: *CodeGen) []const u8 {
        var llvmName = std.fmt.allocPrint(self.alloc, "%x{d}", .{self.commandNum}) catch unreachable;
        self.commandNum += 1;
        return llvmName;
    }

    /// genLLVMGlobalName generates a unique LLVM name for a global variable or function based on `name`.
    pub fn genLLVMGlobalName(self: *CodeGen, name: []const u8) []const u8 {
        if (std.mem.eql(u8, name, "main")) return std.fmt.allocPrint(self.alloc, "@main", .{}) catch unreachable;

        var llvmGlobalName = std.fmt.allocPrint(self.alloc, "@{s}.{d}", .{ name, self.commandNum }) catch unreachable;
        self.commandNum += 1;
        return llvmGlobalName;
    }

    pub fn emitAllocPtr(self: *CodeGen, toType: types.Type) []const u8 {
        const result = self.genLLVMNameEmpty();

        self.emit(
            std.fmt.allocPrint(self.alloc, "{s} = call ptr @malloc(i64 {d})", .{
                result,
                toType.byteWidth(),
            }) catch unreachable,
            self.lastBlockIndex(),
        );

        return result;
    }

    /// emitSignature constructs and emits LLVM IR of the function's signature.
    pub fn emitSignature(self: *CodeGen, func: types.Func, llvmName: []const u8) void {
        var sig = std.fmt.allocPrint(
            self.alloc,
            "define {s} {s}(",
            .{ llvmType(func.retT.*), llvmName },
        ) catch unreachable;

        for (func.args.items, 1..) |arg, i| {
            var n = std.fmt.allocPrint(
                self.alloc,
                "{s}{s} {s}",
                .{ sig, llvmType(arg.t.*), arg.llvmName },
            ) catch unreachable;
            self.alloc.free(sig);

            if (i < func.args.items.len) {
                sig = std.fmt.allocPrint(self.alloc, "{s}, ", .{n}) catch unreachable;
                self.alloc.free(n);
            } else {
                sig = n;
            }
        }

        defer self.alloc.free(sig);

        self.emitA(std.fmt.allocPrint(self.alloc, "{s})", .{sig}) catch unreachable);
    }

    const customDefines =
        \\@__stdinp = external global ptr
        \\@__stdoutp = external global ptr
        \\
        \\define i64 @my_len(ptr %x0) {
        \\  %x1 = getelementptr i64, ptr %x0, i64 -1
        \\  %x2 = load i64, ptr %x1
        \\  ret i64 %x2
        \\}
        \\define void @my_print(ptr %x0) {
        \\  %x1 = load ptr, ptr @__stdoutp
        \\  %x2 = call i32 @"\01_fputs"(ptr %x0, ptr %x1)
        \\  ret void
        \\}
        \\define void @my_stringFree(ptr %x0) {
        \\  %x1 = getelementptr i64, ptr %x0, i64 -1
        \\  call void @free(ptr %x1)
        \\  ret void
        \\}
        \\define ptr @my_readLine() {
        \\  %x1 = alloca ptr
        \\  %x2 = alloca ptr
        \\  %x3 = alloca i64
        \\  %x4 = alloca i64
        \\  %x5 = alloca i8
        \\  %x6 = call ptr @malloc(i64 256)
        \\  store ptr %x6, ptr %x1
        \\  %x7 = getelementptr i8, ptr %x6, i64 8
        \\  store ptr %x7, ptr %x2
        \\  store i64 256, ptr %x3
        \\  store i64 8, ptr %x4
        \\  br label %x8
        \\x8:
        \\  %x9 = load ptr, ptr @__stdinp
        \\  %x10 = call i32 @getc(ptr %x9)
        \\  %x11 = trunc i32 %x10 to i8
        \\  store i8 %x11, ptr %x5
        \\  %x12 = icmp eq i8 %x11, 10
        \\  br i1 %x12, label %x13, label %x22
        \\x13:
        \\  %x14 = load i64, ptr %x4
        \\  %x15 = add i64 %x14, 1
        \\  %x16 = load ptr, ptr %x1
        \\  %x17 = call ptr @realloc(ptr %x16, i64 %x15)
        \\  store ptr %x17, ptr %x1
        \\  %x21 = getelementptr i8, ptr %x17, i64 %x14
        \\  store i8 0, ptr %x21
        \\  br label %x40
        \\x22:
        \\  %x23 = load i64, ptr %x4
        \\  %x24 = load i64, ptr %x3
        \\  %x25 = icmp eq i64 %x23, %x24
        \\  br i1 %x25, label %x26, label %x33
        \\x26:
        \\  %x27 = load i64, ptr %x3
        \\  %x28 = add i64 %x27, 256
        \\  store i64 %x28, ptr %x3
        \\  %x29 = load ptr, ptr %x1
        \\  %x30 = call ptr @realloc(ptr %x29, i64 %x28)
        \\  store ptr %x30, ptr %x1
        \\  %x31 = load i64, ptr %x4
        \\  %x32 = getelementptr i8, ptr %x30, i64 %x31
        \\  store ptr %x32, ptr %x2
        \\  br label %x33
        \\x33:
        \\  %x34 = load i8, ptr %x5
        \\  %x35 = load ptr, ptr %x2
        \\  store i8 %x34, ptr %x35
        \\  %x36 = load ptr, ptr %x2
        \\  %x37 = getelementptr i8, ptr %x36, i32 1
        \\  store ptr %x37, ptr %x2
        \\  %x38 = load i64, ptr %x4
        \\  %x39 = add i64 %x38, 1
        \\  store i64 %x39, ptr %x4
        \\  br label %x8
        \\x40:
        \\  %x41 = load i64, ptr %x4
        \\  %x42 = sub i64 %x41, 8
        \\  %x43 = load ptr, ptr %x1
        \\  store i64 %x42, ptr %x43
        \\  %x44 = getelementptr i8, ptr %x43, i64 8
        \\  ret ptr %x44
        \\}
        \\
    ;

    const customDecls =
        \\declare ptr @realloc(ptr, i64)
        \\declare ptr @malloc(i64)
        \\declare i32 @getc(ptr)
        \\declare i32 @"\01_fputs"(ptr, ptr)
        \\declare void @free(ptr)
        \\declare void @exit(i32)
        \\
    ;

    /// write writes all LLVM IR to the writer.
    pub fn write(self: *CodeGen, writer: std.fs.File.Writer) !void {
        _ = try writer.write(CodeGen.customDefines);
        _ = try writer.write(CodeGen.customDecls);
        _ = try writer.write("\n");

        for (self.readySegments.items) |segment| {
            _ = try writer.write(segment);
            _ = try writer.write("\n");
        }
    }

    test "emitting" {
        var c = CodeGen.init(std.testing.allocator);
        defer c.deinit();

        c.newSegment();
        c.newSegment();

        c.emitA(std.fmt.allocPrint(std.testing.allocator, "mehehe", .{}) catch unreachable);
        c.emitInit(std.fmt.allocPrint(std.testing.allocator, "muhehehe", .{}) catch unreachable);
        c.emitInit(std.fmt.allocPrint(std.testing.allocator, "muhehehe2", .{}) catch unreachable);
        c.emitBlock("%blok1");

        try std.testing.expectEqualStrings("mehehe\nblok1:\n", c.segments.items[1]);
        try std.testing.expectEqualStrings("  muhehehe\n  muhehehe2\n", c.segments.items[0]);
    }
};

/// funcCall constructs a string with the LLVM IR for a function call to the function with the name
/// `llvmFuncName`, passing arguments `llvmParamNames` of types as in `llvmArgTypes` and returning
/// value of type `llvmRetType`.
pub fn funcCall(
    alloc: std.mem.Allocator,
    llvmRetType: []const u8,
    llvmFuncName: []const u8,
    llvmArgTypes: std.ArrayList([]const u8),
    llvmParamNames: std.ArrayList([]const u8),
) []const u8 {
    if (llvmArgTypes.items.len != llvmParamNames.items.len) @panic("arg names and their types must match");

    var tmp = std.fmt.allocPrint(alloc, "call {s} {s}(", .{ llvmRetType, llvmFuncName }) catch unreachable;
    defer alloc.free(tmp);

    if (llvmParamNames.items.len > 0) {
        var tmp1 = std.fmt.allocPrint(alloc, "{s}{s} {s}", .{
            tmp,
            llvmArgTypes.items[0],
            llvmParamNames.items[0],
        }) catch unreachable;
        alloc.free(tmp);
        tmp = tmp1;

        for (1..llvmParamNames.items.len) |i| {
            var tmp2 = std.fmt.allocPrint(alloc, "{s}, {s} {s}", .{
                tmp,
                llvmArgTypes.items[i],
                llvmParamNames.items[i],
            }) catch unreachable;
            alloc.free(tmp);
            tmp = tmp2;
        }
    }

    return std.fmt.allocPrint(alloc, "{s})", .{tmp}) catch unreachable;
}

/// llvmType returns the name of the appropriate type in the LLVM IR.
pub fn llvmType(t: types.Type) []const u8 {
    return switch (t) {
        types.TypeTag.simple => |st| switch (st) {
            types.SimpleType.UNIT => "void",
            types.SimpleType.BOOL => "i1",
            types.SimpleType.U8, types.SimpleType.I8 => "i8",
            types.SimpleType.U16, types.SimpleType.I16 => "i16",
            types.SimpleType.U32, types.SimpleType.I32 => "i32",
            types.SimpleType.U64, types.SimpleType.I64 => "i64",
            types.SimpleType.FLOAT => "float",
            types.SimpleType.DOUBLE => "double",
        },
        types.TypeTag.array => "ptr",
        types.TypeTag.constant => "TODO!",
        else => unreachable,
    };
}

/// llvmAssignOp returns the name of the appropriate assignment operation in the LLVM IR.
pub fn llvmAssignOp(t: types.Type, op: tokens.TokenType) []const u8 {
    switch (op) {
        tt.LSH_ASSIGN => return "shl",
        tt.RSH_ASSIGN => return if (t.isSigned()) "ashr" else "lshr",
        tt.AND_ASSIGN => return "and",
        tt.OR_ASSIGN => return "or",
        tt.XOR_ASSIGN => return "xor",
        else => {},
    }

    if (t.isDouble() or t.isFloat()) {
        return switch (op) {
            tt.ADD_ASSIGN => "fadd",
            tt.SUB_ASSIGN => "fsub",
            tt.MUL_ASSIGN => "fmul",
            tt.QUO_ASSIGN => "fdiv",
            tt.REM_ASSIGN => "frem",
            else => unreachable,
        };
    }

    return switch (op) {
        tt.ADD_ASSIGN => "add",
        tt.SUB_ASSIGN => "sub",
        tt.MUL_ASSIGN => "mul",
        tt.QUO_ASSIGN => if (t.isSigned()) "sdiv" else "udiv",
        tt.REM_ASSIGN => if (t.isSigned()) "srem" else "urem",
        else => unreachable,
    };
}

/// llvmFloatOpreturns the name of the appropriate floating-point operation in the LLVM IR.
pub fn llvmFloatOp(op: tokens.TokenType) []const u8 {
    return switch (op) {
        tt.EQL => "oeq",
        tt.NEQ => "one",
        tt.GT => "ogt",
        tt.LT => "olt",
        tt.GEQ => "oge",
        tt.LEQ => "ole",

        tt.ADD => "fadd",
        tt.SUB => "fsub",
        tt.MUL => "fmul",
        tt.QUO => "fdiv",
        tt.REM => "frem",
        else => unreachable,
    };
}

/// llvmFloatOpreturns the name of the appropriate integer operation in the LLVM IR.
pub fn llvmIntOp(op: tokens.TokenType) []const u8 {
    return switch (op) {
        tt.EQL => "eq",
        tt.NEQ => "ne",
        tt.GT => "gt",
        tt.LT => "lt",
        tt.GEQ => "ge",
        tt.LEQ => "le",

        tt.ADD => "add",
        tt.SUB => "sub",
        tt.B_RSH => "shr",
        tt.B_LSH => "shl",
        tt.B_AND => "and",
        tt.B_XOR => "xor",
        tt.B_OR => "or",
        tt.MUL => "mul",
        tt.QUO => "div",
        tt.REM => "rem",
        else => unreachable,
    };
}

/// evalConstant evaluates the provided constants depending on the operation.
pub fn evalConstant(
    op: tokens.TokenType,
    lhs: []const u8,
    rhs: []const u8,
) !i128 {
    const lhsInt = std.fmt.parseInt(i128, lhs, 0) catch unreachable;
    const rhsInt = std.fmt.parseInt(i128, rhs, 0) catch unreachable;

    return switch (op) {
        tt.EQL => @intFromBool(lhsInt == rhsInt),
        tt.LT => @intFromBool(lhsInt < rhsInt),
        tt.GT => @intFromBool(lhsInt > rhsInt),
        tt.NEQ => @intFromBool(lhsInt != rhsInt),
        tt.LEQ => @intFromBool(lhsInt <= rhsInt),
        tt.GEQ => @intFromBool(lhsInt >= rhsInt),

        tt.ADD => lhsInt + rhsInt,
        tt.SUB => lhsInt - rhsInt,
        tt.MUL => lhsInt * rhsInt,
        tt.QUO => if (rhsInt != 0) @divFloor(lhsInt, rhsInt) else error.DivisionByZero,
        tt.REM => if (rhsInt != 0) @rem(lhsInt, rhsInt) else error.RemainderAfterZero,

        tt.B_AND => lhsInt & rhsInt,
        tt.B_OR => lhsInt | rhsInt,
        tt.B_XOR => lhsInt ^ rhsInt,
        tt.B_RSH => if (lhsInt >= 0)
            (if (rhsInt > 127 or rhsInt < -127)
                0
            else
                (if (rhsInt >= 0)
                    @bitCast(@as(u128, @bitCast(lhsInt)) >> @as(u7, @truncate(@as(u128, @bitCast(rhsInt)))))
                else
                    @bitCast(@as(u128, @bitCast(lhsInt)) << @as(u7, @truncate(@as(u128, @bitCast(-rhsInt)))))))
        else
            (if (rhsInt > 127)
                (-1)
            else
                (if (rhsInt < -127)
                    0
                else
                    @bitCast(if (rhsInt >= 0)
                        (~(@as(u128, @bitCast(-1 - lhsInt)) >> @as(u7, @truncate(@as(u128, @bitCast(rhsInt))))))
                    else
                        (@as(u128, @bitCast(lhsInt)) << @as(u7, @truncate(@as(u128, @bitCast(-rhsInt)))))))),
        tt.B_LSH => if (lhsInt >= 0)
            (if (rhsInt > 127 or rhsInt < -127)
                0
            else
                (if (rhsInt >= 0)
                    @bitCast(@as(u128, @bitCast(lhsInt)) << @as(u7, @truncate(@as(u128, @bitCast(rhsInt)))))
                else
                    @bitCast(@as(u128, @bitCast(lhsInt)) >> @as(u7, @truncate(@as(u128, @bitCast(-rhsInt)))))))
        else
            (if (rhsInt > 127)
                0
            else
                (if (rhsInt < -127)
                    (-1)
                else
                    @bitCast(if (rhsInt < 0)
                        (~(@as(u128, @bitCast(-1 - lhsInt)) >> @as(u7, @truncate(@as(u128, @bitCast(-rhsInt))))))
                    else
                        (@as(u128, @bitCast(lhsInt)) << @as(u7, @truncate(@as(u128, @bitCast(rhsInt)))))))),

        else => unreachable,
    };
}

test "signature" {
    {
        var name = try std.fmt.allocPrint(std.testing.allocator, "@main", .{});
        defer std.testing.allocator.free(name);

        var retT = types.SimpleType.create(std.testing.allocator, types.SimpleType.I32);
        var func = types.Func.init(std.testing.allocator, retT);
        func.args = std.ArrayList(symbols.Symbol).init(std.testing.allocator);

        defer func.destroy(std.testing.allocator);

        var c = CodeGen.init(std.testing.allocator);
        defer c.deinit();
        c.newSegment();

        c.emitSignature(func, name);

        try std.testing.expectEqualStrings("define i32 @main()", c.segments.items[0]);
    }

    {
        var args = std.ArrayList(symbols.Symbol).init(std.testing.allocator);
        args.append(symbols.Symbol{
            .name = "arg1",
            .llvmName = std.fmt.allocPrint(std.testing.allocator, "%arg1.1", .{}) catch unreachable,
            .t = types.SimpleType.create(std.testing.allocator, types.SimpleType.I32),
            .defined = true,
        }) catch unreachable;
        args.append(symbols.Symbol{
            .name = "arg2",
            .llvmName = std.fmt.allocPrint(std.testing.allocator, "%arg2.2", .{}) catch unreachable,
            .t = types.SimpleType.create(std.testing.allocator, types.SimpleType.BOOL),
            .defined = true,
        }) catch unreachable;

        var name = try std.fmt.allocPrint(std.testing.allocator, "@func123", .{});
        defer std.testing.allocator.free(name);

        var retT = types.SimpleType.create(std.testing.allocator, types.SimpleType.I32);
        var func = types.Func.init(std.testing.allocator, retT);
        func.args = args;
        defer func.destroy(std.testing.allocator);

        var c = CodeGen.init(std.testing.allocator);
        defer c.deinit();
        c.newSegment();

        c.emitSignature(func, name);

        try std.testing.expectEqualStrings("define i32 @func123(i32 %arg1.1, i1 %arg2.2)", c.segments.items[0]);
    }
}
