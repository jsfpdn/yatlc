const std = @import("std");

const symbols = @import("symbols.zig");
const tokens = @import("token.zig");

pub fn startsType(symbol: []const u8) bool {
    if (SimpleType.getType(symbol)) |found| {
        _ = found;
        return true;
    }

    return std.mem.eql(u8, symbol, "[");
}

pub const SimpleType = enum(u8) {
    I64,
    I32,
    I16,
    I8,
    U64,
    U32,
    U16,
    U8,
    FLOAT,
    DOUBLE,
    BOOL,
    UNIT,

    const SimpleTypeTable = [@typeInfo(SimpleType).Enum.fields.len][:0]const u8{
        "i64",
        "i32",
        "i16",
        "i8",
        "u64",
        "u32",
        "u16",
        "u8",
        "float",
        "double",
        "bool",
        "unit",
    };

    const widths = [@typeInfo(SimpleType).Enum.fields.len]u8{ 64, 32, 16, 8, 64, 32, 16, 8, 32, 64, 1, 0 };

    pub fn str(self: SimpleType) []const u8 {
        return SimpleTypeTable[@intFromEnum(self)];
    }

    pub fn create(alloc: std.mem.Allocator, st: SimpleType) *Type {
        var t = alloc.create(Type) catch unreachable;
        t.* = Type{ .simple = st };
        return t;
    }

    pub fn getType(name: []const u8) ?SimpleType {
        for (SimpleTypeTable, 0..) |t, i| {
            if (std.mem.eql(u8, name, t)) {
                return @as(SimpleType, @enumFromInt(i));
            }
        }

        return null;
    }

    pub fn width(self: SimpleType) u8 {
        return SimpleType.widths[@intFromEnum(self)];
    }

    pub fn isType(name: []const u8) bool {
        if (SimpleType.getType(name)) |_| {
            return true;
        }

        return false;
    }

    pub fn isIntegral(st: SimpleType) bool {
        return switch (st) {
            SimpleType.UNIT, SimpleType.BOOL => false,
            else => true,
        };
    }

    pub fn isNumeric(st: SimpleType) bool {
        return switch (st) {
            SimpleType.UNIT, SimpleType.BOOL, SimpleType.DOUBLE, SimpleType.FLOAT => false,
            else => true,
        };
    }

    pub fn isSigned(st: SimpleType) bool {
        return @intFromEnum(st) >= @intFromEnum(SimpleType.I64) and @intFromEnum(st) <= @intFromEnum(SimpleType.I8);
    }
};

pub const Array = struct {
    alloc: std.mem.Allocator,

    dimensions: usize = 0,
    ofType: *Type = undefined,

    pub fn create(alloc: std.mem.Allocator, dimensions: usize, ofType: *Type) *Type {
        var t = alloc.create(Type) catch unreachable;
        t.* = Type{ .array = Array{ .alloc = alloc, .dimensions = dimensions, .ofType = ofType } };
        return t;
    }

    fn destroy(self: *Array) void {
        self.*.ofType.destroy(self.alloc);
    }

    fn str(self: Array) []const u8 {
        // TODO: Format the string representation of array as `[-,-,-]<type>`.
        // This means that self.str() creates dynamically allocated string
        // therefore every call to self.str() must deallocate the string.
        _ = self;
        return "array";
    }
};

pub const Func = struct {
    retT: *Type,
    args: std.ArrayList(symbols.Symbol),
    namedParams: bool = false,

    pub fn init(alloc: std.mem.Allocator, retT: *Type) Func {
        var func = Func{
            .args = std.ArrayList(symbols.Symbol).init(alloc),
            .retT = retT,
        };

        return func;
    }

    pub fn destroy(self: *Func, alloc: std.mem.Allocator) void {
        for (self.args.items) |arg| arg.destroy(alloc);
        self.args.deinit();
        self.retT.destroy(alloc);
    }

    pub const DefinitionErrors = error{ArgTypeMismatch};

    /// defines checks whether the receiver struct `self` is a valid definition of an already
    /// declared function `func`.
    pub fn defines(self: *Func, funcDecl: Func) DefinitionErrors!void {
        // Self cannot define `func` if `func` is already defined.
        if (self.args.items.len != funcDecl.args.items.len) return DefinitionErrors.ArgTypeMismatch;

        for (0..self.args.items.len) |i| {
            if (!self.args.items[i].t.equals(funcDecl.args.items[i].t.*)) {
                // TODO: Ideally inform what the mismatch is and where it happened.
                return DefinitionErrors.ArgTypeMismatch;
            }
        }
    }
};

pub const Constant = struct {
    value: i128 = 0,

    pub fn create(alloc: std.mem.Allocator, value: i128) *Type {
        var t = alloc.create(Type) catch unreachable;
        t.* = Type{ .constant = Constant{ .value = value } };
        return t;
    }
};

pub const TypeTag = enum(u8) {
    simple,
    array,
    func,
    constant,
};

pub const Type = union(TypeTag) {
    simple: SimpleType,
    constant: Constant,
    array: Array,
    func: Func,

    pub fn destroy(self: *Type, alloc: std.mem.Allocator) void {
        switch (self.*) {
            TypeTag.array => |*array| array.destroy(),
            TypeTag.func => |*func| func.destroy(alloc),
            else => {},
        }
        alloc.destroy(self);
    }

    pub fn clone(self: *Type, alloc: std.mem.Allocator) *Type {
        var t = alloc.create(Type) catch unreachable;
        t.* = switch (self.*) {
            TypeTag.simple => |simple| Type{ .simple = simple },
            TypeTag.constant => |constant| Type{ .constant = Constant{ .value = constant.value } },
            TypeTag.array => |array| Type{ .array = Array{
                .alloc = array.alloc,
                .dimensions = array.dimensions,
                .ofType = array.ofType.clone(alloc),
            } },
            TypeTag.func => |func| blk: {
                var newFunc = Func{
                    .retT = func.retT.clone(alloc),
                    .namedParams = func.namedParams,
                    .args = std.ArrayList(symbols.Symbol).initCapacity(alloc, func.args.items.len) catch unreachable,
                };
                for (func.args.items) |arg| newFunc.args.append(arg.clone(alloc)) catch unreachable;

                break :blk Type{ .func = newFunc };
            },
        };
        return t;
    }

    pub fn equals(self: Type, other: Type) bool {
        if (@intFromEnum(self) != @intFromEnum(other)) return false;

        return switch (self) {
            TypeTag.simple => |simple| simple == other.simple,
            TypeTag.constant => true,
            TypeTag.array => |array| array.dimensions == other.array.dimensions and array.ofType.equals(other.array.ofType.*),
            TypeTag.func => |func| blk: {
                if (func.args.items.len != other.func.args.items.len or func.namedParams != other.func.namedParams or !func.retT.equals(other.func.retT.*))
                    break :blk false;
                for (0..func.args.items.len) |i| {
                    if (!func.args.items[i].t.equals(other.func.args.items[i].t.*))
                        break :blk false;
                }
                break :blk true;
            },
        };
    }

    pub fn isArray(self: Type) bool {
        return switch (self) {
            TypeTag.array => |array| array.dimensions >= 1,
            else => false,
        };
    }

    pub fn isPointer(self: Type) bool {
        return switch (self) {
            TypeTag.array => |array| array.dimensions == 0,
            else => false,
        };
    }

    pub fn isConstant(self: Type) bool {
        return switch (self) {
            TypeTag.constant => true,
            else => false,
        };
    }

    pub fn isFunction(self: Type) bool {
        return switch (self) {
            TypeTag.func => true,
            else => false,
        };
    }

    pub fn isUnit(self: Type) bool {
        return switch (self) {
            TypeTag.simple => |simple| simple == SimpleType.UNIT,
            else => false,
        };
    }

    pub fn isBool(self: Type) bool {
        return switch (self) {
            TypeTag.simple => |st| st == SimpleType.BOOL,
            else => false,
        };
    }

    pub fn isDouble(self: Type) bool {
        return switch (self) {
            TypeTag.simple => |st| st == SimpleType.DOUBLE,
            else => false,
        };
    }

    pub fn isFloat(self: Type) bool {
        return switch (self) {
            TypeTag.simple => |st| st == SimpleType.FLOAT,
            else => false,
        };
    }

    pub fn isIntegral(self: Type) bool {
        return switch (self) {
            TypeTag.simple => |st| SimpleType.isIntegral(st),
            TypeTag.constant => true,
            else => false,
        };
    }

    pub fn isNumeric(self: Type) bool {
        return switch (self) {
            TypeTag.simple => |st| SimpleType.isNumeric(st),
            TypeTag.constant => true,
            else => false,
        };
    }

    pub fn isSigned(self: Type) bool {
        return switch (self) {
            TypeTag.simple => |st| SimpleType.isSigned(st),
            else => false,
        };
    }

    pub fn str(self: Type) []const u8 {
        return switch (self) {
            TypeTag.simple => |st| st.str(),
            TypeTag.array => |a| a.str(),
            TypeTag.func => "function",
            TypeTag.constant => "constant",
        };
    }
};

pub fn leastSupertype(alloc: std.mem.Allocator, fstParam: *Type, sndParam: *Type) ?*Type {
    var fst = fstParam;
    var snd = sndParam;

    if (fst.equals(snd.*)) return fst.clone(alloc);
    if (fst.isUnit() or snd.isUnit()) return null;
    if (fst.isBool() or snd.isBool()) return null;
    if (snd.isPointer()) {
        if (fst.isArray()) return fst.clone(alloc);
        return null;
    }

    // TODO: What if fst.isPointer and snd.isArray?
    if (fst.isArray() or snd.isArray()) return null;
    if (snd.isConstant()) {
        var t = fst;
        fst = snd;
        snd = t;
    }

    if (fst.isConstant()) {
        if (snd.isIntegral()) return snd.clone(alloc);
        return null;
    }

    if (fst.isDouble() or snd.isDouble()) return SimpleType.create(alloc, SimpleType.DOUBLE);
    if (fst.isFloat() or snd.isFloat()) return SimpleType.create(alloc, SimpleType.FLOAT);

    if (fst.simple.width() < snd.simple.width() or (fst.simple.width() == snd.simple.width() and fst.simple.isSigned())) {
        var t = fst;
        fst = snd;
        snd = t;
    }

    if (fst.simple.isSigned() or !snd.simple.isSigned()) return fst.clone(alloc);

    return switch (fst.simple) {
        SimpleType.U64 => null,
        SimpleType.U32 => SimpleType.create(alloc, SimpleType.I64),
        SimpleType.U16 => SimpleType.create(alloc, SimpleType.I64),
        SimpleType.U8 => SimpleType.create(alloc, SimpleType.I64),
        else => unreachable,
    };
}

pub fn checkBounds(t: SimpleType, n: i128) bool {
    return switch (t) {
        SimpleType.I8 => -128 <= n and n <= 127,
        SimpleType.I16 => -32768 <= n and n <= 32767,
        SimpleType.I32 => -2147483648 <= n and n <= 2147483647,
        SimpleType.I64 => -9223372036854775808 <= n and n <= 9223372036854775807,
        SimpleType.U8 => 0 <= n and n <= 255,
        SimpleType.U16 => 0 <= n and n <= 65535,
        SimpleType.U32 => 0 <= n and n <= 4294967295,
        SimpleType.U64 => 0 <= n and n <= 18446744073709551615,
        else => unreachable,
    };
}

pub fn minimumSignedType(alloc: std.mem.Allocator, n: i128) *Type {
    if (-128 <= n and n <= 127)
        return SimpleType.create(alloc, SimpleType.I8);

    if (-32768 <= n and n <= 32767)
        return SimpleType.create(alloc, SimpleType.I16);

    if (-2147483648 <= n and n <= 2147483647)
        return SimpleType.create(alloc, SimpleType.I32);

    if (-9223372036854775808 <= n and n <= 9223372036854775807)
        return SimpleType.create(alloc, SimpleType.I64);

    unreachable;
}
