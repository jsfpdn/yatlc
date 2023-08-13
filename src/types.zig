const std = @import("std");

const symbols = @import("symbols.zig");

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
    STRING,
    CHAR,
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
        "string",
        "char",
        "unit",
    };

    pub fn getType(str: []const u8) ?SimpleType {
        for (SimpleTypeTable, 0..) |t, i| {
            if (std.mem.eql(u8, str, t)) {
                return @as(SimpleType, @enumFromInt(i));
            }
        }

        return null;
    }

    pub fn isType(str: []const u8) bool {
        if (SimpleType.getType(str)) |_| {
            return true;
        }

        return false;
    }
};

pub const Array = struct {
    dimensions: usize = 0,
    ofType: *Type = undefined,

    fn destroy(self: *Array, alloc: std.mem.Allocator) void {
        self.*.ofType.destroy(alloc);
    }
};

pub const Pointer = struct {
    toType: *Type = undefined,

    fn destroy(self: *Pointer, alloc: std.mem.Allocator) void {
        self.*.toType.destroy(alloc);
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

        if (func.args.items.len == 0) {
            func.namedParams = true;
        } else {
            for (func.args.items) |arg| {
                if (std.mem.eql(u8, arg.name, "")) {
                    func.namedParams = false;
                }
            }
        }

        return func;
    }

    pub fn destroy(self: *Func, alloc: std.mem.Allocator) void {
        for (self.args.items) |arg| arg.t.destroy(alloc);
        self.args.deinit();
        self.retT.destroy(alloc);
    }

    pub const DefinitionErrors = error{ AlreadyDefined, ArgsNotNamed, ArgTypeMismatch };

    /// defines checks whether the receiver struct `self` is a valid definition of an already
    /// declared function `func`.
    pub fn defines(self: *Func, funcDecl: Func) DefinitionErrors!void {
        // Self cannot define `func` if `func` is already defined.
        if (funcDecl.defined) return DefinitionErrors.AlreadyDefined;
        // Self cannot define anything if parameters are not named.
        if (!self.namedParams) return DefinitionErrors.ArgsNotNamed;

        for (0..self.args.items.len) |i| {
            if (!self.args.items[i].t.equals(funcDecl.args.items[i].t)) {
                // TODO: Ideally inform what the mismatch is and where it happened.
                return DefinitionErrors.ArgTypeMismatch;
            }
        }
    }
};

pub const TypeTag = enum(u8) {
    sType,
    cType,
    pointer,
    func,
};

pub const Type = union(TypeTag) {
    sType: SimpleType,
    cType: Array,
    pointer: Pointer,
    func: Func,

    pub fn destroy(self: *Type, alloc: std.mem.Allocator) void {
        switch (self.*) {
            TypeTag.cType => |*array| array.destroy(alloc),
            TypeTag.pointer => |*pointer| pointer.destroy(alloc),
            TypeTag.func => |*func| func.destroy(alloc),
            TypeTag.sType => {},
        }
        alloc.destroy(self);
    }

    pub fn clone(self: *Type, alloc: std.mem.Allocator) *Type {
        var t = alloc.create(Type) catch unreachable;
        t.* = switch (self.*) {
            TypeTag.sType => |simple| Type{ .sType = simple },
            TypeTag.cType => |array| Type{ .cType = Array{ .dimensions = array.dimensions, .ofType = array.ofType.clone(alloc) } },
            TypeTag.pointer => |pointer| Type{ .pointer = Pointer{ .toType = pointer.toType.clone(alloc) } },
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

    pub fn equals(self: *Type, other: *Type) bool {
        if (@intFromEnum(self.*) != @intFromEnum(other.*)) return false;

        return switch (self.*) {
            TypeTag.sType => |selfSimple| selfSimple == other.sType,
            TypeTag.pointer => |selfPointer| selfPointer.toType.equals(other.pointer.toType),
            TypeTag.func => |selfFunc| blk: {
                if (selfFunc.args.items.len != other.func.args.items.len or selfFunc.defined != other.func.defined or selfFunc.namedParams != other.func.namedParams or !selfFunc.retT.equals(other.func.retT))
                    break :blk false;
                for (0..selfFunc.args.items.len) |i| {
                    if (!selfFunc.args.items[i].t.equals(other.func.args.items[i].t))
                        break :blk false;
                }
                break :blk true;
            },
            TypeTag.cType => |selfArray| selfArray.dimensions == other.cType.dimensions and selfArray.ofType.equals(other.cType.ofType),
        };
    }
};

pub fn IsIntegral(t: Type) bool {
    switch (t) {
        Type.cType => return false,
        Type.sType => {
            switch (t.sType) {
                SimpleType.I64, SimpleType.I32, SimpleType.I16, SimpleType.U64, SimpleType.U32, SimpleType.U16 => return true,
                else => return false,
            }
        },
        else => return false,
    }
}

pub fn IsNum(t: Type) bool {
    if (IsIntegral(t)) {
        return true;
    }

    return switch (t) {
        Type.sType => {
            return switch (t.sType) {
                SimpleType.FLOAT => true,
                else => false,
            };
        },
        else => false,
    };
}

test "array type does not leak memory" {
    var innerT = try std.testing.allocator.create(Type);
    innerT.* = Type{ .sType = SimpleType.I8 };

    var t = try std.testing.allocator.create(Type);
    t.* = Type{ .cType = Array{
        .ofType = innerT,
    } };

    defer t.destroy(std.testing.allocator);
}

test "pointer type does not leak memory" {
    var innerT = try std.testing.allocator.create(Type);
    innerT.* = Type{ .sType = SimpleType.I8 };

    var ptr = try std.testing.allocator.create(Type);
    ptr.* = Type{ .pointer = Pointer{
        .toType = innerT,
    } };

    defer ptr.destroy(std.testing.allocator);
}
