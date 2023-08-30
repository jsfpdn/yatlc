const std = @import("std");

const types = @import("types.zig");
const symbols = @import("symbols.zig");

pub const CodeGen = struct {
    alloc: std.mem.Allocator,

    triple: []const u8,
    segments: std.ArrayList([]const u8),
    commandNum: u64 = 0,

    waitingBlock: []const u8,
    currentBlock: []const u8,
    waits: bool,

    pub fn init(alloc: std.mem.Allocator) CodeGen {
        return CodeGen{
            .alloc = alloc,
            .triple = "TODO",
            .segments = std.ArrayList([]const u8).initCapacity(alloc, 3) catch unreachable,
            .waitingBlock = std.fmt.allocPrint(alloc, "", .{}) catch unreachable,
            .currentBlock = std.fmt.allocPrint(alloc, "", .{}) catch unreachable,
            .waits = false,
        };
    }

    pub fn deinit(self: *CodeGen) void {
        for (self.segments.items) |b| self.alloc.free(b);
        self.segments.deinit();
        self.alloc.free(self.waitingBlock);
        self.alloc.free(self.currentBlock);
    }

    pub fn newSegment(self: *CodeGen) void {
        self.segments.append(std.fmt.allocPrint(self.alloc, "", .{}) catch unreachable) catch unreachable;
    }

    /// emitA takes a string and appends it immediately after the contents of the last segment.
    pub fn emitA(self: *CodeGen, string: []const u8) void {
        defer self.alloc.free(string);

        var s = self.segments.pop();
        defer self.alloc.free(s);

        var ns = std.fmt.allocPrint(self.alloc, "{s}{s}", .{ s, string }) catch unreachable;

        self.segments.append(ns) catch unreachable;
    }

    /// emitInit takes a string, adds two spaces in front and a newline after and appends it
    /// after the contents of the first segment.
    pub fn emitInit(self: *CodeGen, string: []const u8) void {
        defer self.alloc.free(string);

        var s = self.segments.items[0];
        defer self.alloc.free(s);

        var ns = std.fmt.allocPrint(self.alloc, "{s}  {s}\n", .{ s, string }) catch unreachable;
        self.segments.items[0] = ns;
    }

    /// emitBlock emits instructions that open a new block with the provided name.
    /// The name must contain the percent sign.
    pub fn emitBlock(self: *CodeGen, blockName: []const u8) void {
        self.waits = false;

        var s = self.segments.items[self.segments.items.len - 1];
        defer self.alloc.free(s);

        var ns = std.fmt.allocPrint(self.alloc, "{s}\n{s}:", .{ s, blockName[1..] }) catch unreachable;
        self.segments.items[self.segments.items.len - 1] = ns;

        self.alloc.free(self.currentBlock);
        self.currentBlock = std.fmt.allocPrint(self.alloc, "{s}", .{blockName}) catch unreachable;
    }

    pub fn emitWaitingBlock(self: *CodeGen, blockName: []const u8) void {
        self.waits = true;

        self.alloc.free(self.waitingBlock);
        self.waitingBlock = std.fmt.allocPrint(self.alloc, "{s}", .{blockName}) catch unreachable;

        self.alloc.free(self.currentBlock);
        self.waitingBlock = std.fmt.allocPrint(self.alloc, "{s}", .{blockName}) catch unreachable;
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

    pub fn lastBlockIndex(self: *CodeGen) usize {
        return self.segments.items.len - 1;
    }

    pub fn genLLVMName(self: *CodeGen, name: []const u8) []const u8 {
        var llvmName = std.fmt.allocPrint(self.alloc, "%{s}.{d}", .{ name, self.commandNum }) catch unreachable;
        self.commandNum += 1;
        return llvmName;
    }

    pub fn write(self: *CodeGen, writer: std.fs.File.Writer) !void {
        for (self.segments.items) |segment|
            _ = writer.write(segment) catch unreachable;
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

        try std.testing.expectEqualStrings("mehehe\nblok1:", c.segments.items[1]);
        try std.testing.expectEqualStrings("  muhehehe\n  muhehehe2\n", c.segments.items[0]);
    }
};

pub fn signature(alloc: std.mem.Allocator, name: []const u8, args: std.ArrayList(symbols.Symbol), retT: types.Type) []const u8 {
    var sig = std.fmt.allocPrint(alloc, "define {s} {s}(", .{ llvmType(retT), name }) catch unreachable;

    for (args.items, 1..) |arg, i| {
        var n = std.fmt.allocPrint(alloc, "{s}{s} {s}", .{ sig, llvmType(arg.t.*), arg.llvmName }) catch unreachable;
        alloc.free(sig);

        if (i < args.items.len) {
            sig = std.fmt.allocPrint(alloc, "{s}, ", .{n}) catch unreachable;
            alloc.free(n);
        } else {
            sig = n;
        }
    }
    defer alloc.free(sig);
    return std.fmt.allocPrint(alloc, "{s})", .{sig}) catch unreachable;
}

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

test "signature" {
    {
        var args = std.ArrayList(symbols.Symbol).init(std.testing.allocator);
        defer args.deinit();

        var name = try std.fmt.allocPrint(std.testing.allocator, "@main", .{});
        defer std.testing.allocator.free(name);

        var retT = types.SimpleType.create(std.testing.allocator, types.SimpleType.I32);
        defer retT.destroy(std.testing.allocator);

        var sig = signature(std.testing.allocator, name, args, retT.*);
        defer std.testing.allocator.free(sig);

        try std.testing.expectEqualStrings("define i32 @main()", sig);
    }

    {
        var args = std.ArrayList(symbols.Symbol).init(std.testing.allocator);
        defer args.deinit();

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
        defer for (args.items) |arg|
            arg.destroy(std.testing.allocator);

        var name = try std.fmt.allocPrint(std.testing.allocator, "@func123", .{});
        defer std.testing.allocator.free(name);

        var retT = types.SimpleType.create(std.testing.allocator, types.SimpleType.I32);
        defer retT.destroy(std.testing.allocator);

        var sig = signature(std.testing.allocator, name, args, retT.*);
        defer std.testing.allocator.free(sig);

        try std.testing.expectEqualStrings("define i32 @func123(i32 %arg1.1, i1 %arg2.2)", sig);
    }
}
