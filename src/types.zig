const std = @import("std");

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
};

pub const Array = struct {
    dimensions: usize = 0,
    ofType: *Type = undefined,

    fn destroy(self: *Array, alloc: std.mem.Allocator) void {
        self.*.ofType.destroy(alloc);
    }
};

pub const TypeTag = enum {
    sType,
    cType,
    constant,
};

pub const Type = union(TypeTag) {
    sType: SimpleType,
    cType: Array,
    constant: i128,

    pub fn destroy(self: *Type, alloc: std.mem.Allocator) void {
        switch (self.*) {
            TypeTag.cType => |*array| array.destroy(alloc),
            else => {},
        }
        alloc.destroy(self);
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
    }
    unreachable;
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
