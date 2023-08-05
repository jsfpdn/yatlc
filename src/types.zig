const std = @import("std");

pub const SimpleType = enum(u8) {
    I32,
    I16,
    I8,
    U32,
    U16,
    U8,
    F32,
    F16,
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
        "f32",
        "f16",
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
    dimensions: []const u8,
    ofType: *Type,
};

pub const Type = union(enum) {
    sType: SimpleType,
    cType: Array,
    constant: i128,
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
