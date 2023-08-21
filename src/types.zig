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
};

pub const Array = struct {
    dimensions: usize = 0,
    ofType: *Type = undefined,

    fn destroy(self: *Array, alloc: std.mem.Allocator) void {
        self.*.ofType.destroy(alloc);
    }
};

pub const Func = struct {
    retT: *Type,
    args: std.ArrayList(symbols.Symbol),

    namedParams: bool = false,
    defined: bool = false,

    pub fn init(alloc: std.mem.Allocator, retT: *Type) Func {
        var func = Func{
            .args = std.ArrayList(symbols.Symbol).init(alloc),
            .retT = retT,
        };

        return func;
    }

    pub fn destroy(self: *Func, alloc: std.mem.Allocator) void {
        for (self.args.items) |arg| arg.t.destroy(alloc);
        self.args.deinit();
        self.retT.destroy(alloc);
    }

    pub const DefinitionErrors = error{ AlreadyDefined, ArgTypeMismatch };

    /// defines checks whether the receiver struct `self` is a valid definition of an already
    /// declared function `func`.
    pub fn defines(self: *Func, funcDecl: Func) DefinitionErrors!void {
        // Self cannot define `func` if `func` is already defined.
        if (funcDecl.defined) return DefinitionErrors.AlreadyDefined;
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
    int: i128 = 0,
    float: f64 = 0.0,
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
            TypeTag.array => |*array| array.destroy(alloc),
            TypeTag.func => |*func| func.destroy(alloc),
            else => {},
        }
        alloc.destroy(self);
    }

    pub fn clone(self: *Type, alloc: std.mem.Allocator) *Type {
        var t = alloc.create(Type) catch unreachable;
        t.* = switch (self.*) {
            TypeTag.simple => |simple| Type{ .simple = simple },
            TypeTag.constant => |constant| Type{ .constant = Constant{ .int = constant.int, .float = constant.float } },
            TypeTag.array => |array| Type{ .array = Array{ .dimensions = array.dimensions, .ofType = array.ofType.clone(alloc) } },
            TypeTag.func => |func| blk: {
                var newFunc = Func{
                    .retT = func.retT.clone(alloc),
                    .namedParams = func.namedParams,
                    .defined = func.defined,
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
            TypeTag.constant => |constant| constant.int == other.constant.int,
            TypeTag.array => |array| array.dimensions == other.array.dimensions and array.ofType.equals(other.array.ofType.*),
            TypeTag.func => |func| blk: {
                if (func.args.items.len != other.func.args.items.len or func.defined != other.func.defined or func.namedParams != other.func.namedParams or !func.retT.equals(other.func.retT.*))
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

    pub fn str(self: Type) []const u8 {
        return switch (self) {
            TypeTag.simple => |st| st.str(),
            TypeTag.array => "array",
            TypeTag.func => "function",
            TypeTag.constant => "constant",
        };
    }
};

test "array type does not leak memory" {
    var innerT = try std.testing.allocator.create(Type);
    innerT.* = Type{ .simple = SimpleType.I8 };

    var t = try std.testing.allocator.create(Type);
    t.* = Type{ .array = Array{
        .ofType = innerT,
    } };

    defer t.destroy(std.testing.allocator);
}
