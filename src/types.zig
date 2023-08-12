const std = @import("std");

const symbols = @import("symbols.zig");

pub const SimpleType = enum(u8) {
    I32,
    I16,
    I8,
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
        "i32",
        "i16",
        "i8",
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
        return Func{
            .args = std.ArrayList(symbols.Symbol).init(alloc),
            .retT = retT,
        };
    }

    pub fn destroy(self: *Func, alloc: std.mem.Allocator) void {
        for (self.args.items) |arg| arg.t.destroy(alloc);
        self.args.deinit();
        self.retT.destroy(alloc);
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
        switch (self.*) {
            TypeTag.sType => |simple| {
                t.* = Type{ .sType = simple };
                return t;
            },
            TypeTag.cType => |array| {
                var newArray = Array{ .dimensions = array.dimensions, .ofType = array.ofType.clone(alloc) };
                t.* = Type{ .cType = newArray };
                return t;
            },
            TypeTag.pointer => |pointer| {
                var newPointer = Pointer{ .toType = pointer.toType.clone(alloc) };
                t.* = Type{ .pointer = newPointer };
                return t;
            },
            TypeTag.func => |func| {
                var newFunc = Func{
                    .retT = func.retT.clone(alloc),
                    .namedParams = func.namedParams,
                    .defined = func.defined,
                    .args = std.ArrayList(symbols.Symbol).initCapacity(alloc, func.args.items.len) catch unreachable,
                };
                for (func.args.items) |arg| newFunc.args.append(arg.clone(alloc)) catch unreachable;

                t.* = Type{ .func = newFunc };
                return t;
            },
        }
    }
};

pub fn IsIntegral(t: Type) bool {
    switch (t) {
        Type.cType => return false,
        Type.sType => {
            switch (t.sType) {
                SimpleType.I32, SimpleType.I16, SimpleType.U32, SimpleType.U16 => return true,
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
