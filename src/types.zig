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
        _ = self;
        // TODO: Format the string representation of array as `[-,-,-]<type>`.
        // This means that self.str() creates dynamically allocated string
        // therefore every call to self.str() must deallocate the string.
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
    pointer,
};

pub const Type = union(TypeTag) {
    simple: SimpleType,
    constant: Constant,
    array: Array,
    func: Func,
    pointer: bool,

    pub fn createPointer(alloc: std.mem.Allocator) *Type {
        var t = alloc.create(Type) catch unreachable;
        t.* = Type{ .pointer = true };
        return t;
    }

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
            TypeTag.pointer => Type{ .pointer = true },
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
            TypeTag.constant, TypeTag.pointer => true,
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
            TypeTag.array => true,
            else => false,
        };
    }

    pub fn isPointer(self: Type) bool {
        return switch (self) {
            TypeTag.pointer => true,
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
            TypeTag.pointer => "pointer",
        };
    }

    pub fn bitWidth(self: Type) u8 {
        return switch (self) {
            TypeTag.simple => |st| switch (st) {
                SimpleType.BOOL => 1,
                SimpleType.I8, SimpleType.U8 => 8,
                SimpleType.I16, SimpleType.U16 => 16,
                SimpleType.I32, SimpleType.U32 => 32,
                SimpleType.I64, SimpleType.U64 => 64,
                else => unreachable,
            },
            else => unreachable,
        };
    }

    pub fn byteWidth(self: Type) u8 {
        return switch (self) {
            TypeTag.simple => |st| switch (st) {
                SimpleType.UNIT => 0,
                SimpleType.FLOAT => 4,
                SimpleType.DOUBLE => 8,
                else => @divExact(self.bitWidth(), 8),
            },
            TypeTag.array => 8,
            else => unreachable,
        };
    }
};

/// Computes the absolute value of an integer. Wraps `std.math.asbInt` for convenience.
fn abs(x: anytype) @TypeOf(x) {
    return std.math.absInt(x) catch unreachable;
}

pub fn leastSupertype(alloc: std.mem.Allocator, fstParam: *Type, sndParam: *Type) ?*Type {
    var fst = fstParam;
    var snd = sndParam;

    if (fst.equals(snd.*)) {
        if (fst.isConstant() and
            (abs(fst.constant.value) < abs(snd.constant.value) or
            (abs(fst.constant.value) == abs(snd.constant.value) and snd.constant.value >= 0)))
        {
            return snd.clone(alloc);
        }

        return fst.clone(alloc);
    }
    if (fst.isUnit() or snd.isUnit()) return null;
    if (fst.isBool() or snd.isBool()) return null;
    if (fst.isPointer()) {
        if (snd.isArray()) return snd.clone(alloc);
        return null;
    }

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

    if (fst.bitWidth() < snd.bitWidth() or (fst.bitWidth() == snd.bitWidth() and fst.simple.isSigned())) {
        var t = fst;
        fst = snd;
        snd = t;
    }

    if (fst.simple.isSigned() or !snd.simple.isSigned()) return fst.clone(alloc);

    return switch (fst.simple) {
        SimpleType.U64 => null,
        SimpleType.U32 => SimpleType.create(alloc, SimpleType.I64),
        SimpleType.U16 => SimpleType.create(alloc, SimpleType.I32),
        SimpleType.U8 => SimpleType.create(alloc, SimpleType.I16),
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

pub fn minimumSignedType(alloc: std.mem.Allocator, n: i128) !*Type {
    if (-128 <= n and n <= 127)
        return SimpleType.create(alloc, SimpleType.I8);

    if (-32768 <= n and n <= 32767)
        return SimpleType.create(alloc, SimpleType.I16);

    if (-2147483648 <= n and n <= 2147483647)
        return SimpleType.create(alloc, SimpleType.I32);

    if (-9223372036854775808 <= n and n <= 9223372036854775807)
        return SimpleType.create(alloc, SimpleType.I64);

    return error.Overflow;
}
