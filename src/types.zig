const std = @import("std");

pub const SimpleType = enum(u8) {
    I32,
    I16,
    U32,
    U16,
    FLOAT,
    BOOL,
    STRING,
    CHAR,
    VOID,

    const SimpleTypeTable = [@typeInfo(SimpleType).Enum.fields.len][:0]const u8{
        "i32",
        "i16",
        "u32",
        "u16",
        "float",
        "bool",
        "string",
        "char",
        "void",
    };

    pub fn getType(str: []const u8) ?SimpleType {
        var i: i8 = 0;

        for (SimpleTypeTable) |t| {
            if (std.mem.eql(u8, str, t)) {
                return @intToEnum(SimpleType, i);
            }
            i += 1;
        }

        return null;
    }
};

pub const Array = struct {
    dimensions: []const u8,
    ofType: SimpleType,
};

pub const Type = union(enum) {
    sType: SimpleType,
    cType: Array,
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
