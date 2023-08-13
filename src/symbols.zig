// Symbol table implementation.

const std = @import("std");
const tokens = @import("token.zig");
const types = @import("types.zig");

pub const SymbolError = error{ SymbolAlreadyExists, OutOfMemory };

pub const Symbol = struct {
    name: []const u8 = "",
    llvmName: []const u8 = "",
    location: tokens.Token = undefined,
    t: *types.Type,

    pub fn destroy(self: Symbol, alloc: std.mem.Allocator) void {
        self.t.destroy(alloc);
    }

    pub fn clone(self: Symbol, alloc: std.mem.Allocator) Symbol {
        return Symbol{
            .name = self.name,
            .llvmName = self.llvmName,
            .location = self.location,
            .t = self.t.clone(alloc),
        };
    }
};

pub const SymbolTable = struct {
    scopeStack: std.ArrayList(std.StringHashMap(Symbol)),
    alloc: std.mem.Allocator,

    pub fn init(alloc: std.mem.Allocator) SymbolTable {
        return SymbolTable{
            .scopeStack = std.ArrayList(std.StringHashMap(Symbol)).init(alloc),
            .alloc = alloc,
        };
    }

    /// deinit all the open scopes and all the supporting data structures
    pub fn deinit(self: *SymbolTable) void {
        for (self.scopeStack.items) |*scope| {
            var valIt = scope.valueIterator();
            while (valIt.next()) |symbol| {
                symbol.destroy(self.alloc);
            }

            scope.deinit();
        }
        self.scopeStack.deinit();
    }

    /// open a new scope
    pub fn open(self: *SymbolTable) !void {
        var scope: std.StringHashMap(Symbol) = std.StringHashMap(Symbol).init(self.alloc);
        try self.scopeStack.append(scope);
    }

    /// close deinits the last open scope and removes it from the stack of open scopes
    pub fn close(self: *SymbolTable) void {
        if (self.scopeStack.items.len == 0) {
            @panic("ICE: tried to close scope when none is open");
        }

        var scope = self.scopeStack.pop();
        var valIt = scope.valueIterator();
        while (valIt.next()) |symbol| {
            symbol.destroy(self.alloc);
        }

        scope.deinit();
    }

    /// insert symbol to the active scope
    pub fn insert(self: *SymbolTable, symbol: Symbol) SymbolError!void {
        if (self.scopeStack.items.len == 0) {
            @panic("ICE: tried to insert symbol when no scope is open");
        }

        var activeScope = &self.scopeStack.items[self.scopeStack.items.len - 1];

        if (activeScope.contains(symbol.name)) {
            return SymbolError.SymbolAlreadyExists;
        }

        activeScope.put(symbol.name, symbol) catch unreachable;
    }

    pub fn upsert(self: *SymbolTable, symbol: Symbol) void {
        if (self.scopeStack.items.len == 0) {
            @panic("ICE: tried to insert symbol when no scope is open");
        }

        var activeScope = &self.scopeStack.items[self.scopeStack.items.len - 1];
        if (activeScope.fetchRemove(symbol.name)) |kv| kv.value.destroy(self.alloc);
        activeScope.put(symbol.name, symbol) catch unreachable;
    }

    /// get symbol. Search for the symbol starts at the innermost scope towards the outermost scope.
    pub fn get(self: *SymbolTable, name: []const u8) ?Symbol {
        var i = self.scopeStack.items.len;

        while (i > 0) {
            i -= 1;
            if (self.scopeStack.items[i].get(name)) |s| return s;
        }

        return null;
    }
};

test "get on empty SymbolTable does not fail" {
    var st = SymbolTable.init(std.testing.allocator);
    defer st.deinit();

    try std.testing.expectEqual(st.get("var1"), null);
}

test "basic SymbolTable usage" {
    const pt = @import("parser_test.zig");

    var st = SymbolTable.init(std.testing.allocator);
    defer st.deinit();

    try st.open();

    // Insert symbol and look it up.
    try st.insert(Symbol{ .name = "var1", .t = pt.createSimpleType(types.SimpleType.U8, std.testing.allocator) });
    try std.testing.expectEqual(st.get("var1").?.name, "var1");

    // Creating new scope does not hide symbol in already existing scopes.
    try st.open();
    try std.testing.expectEqual(st.get("var1").?.name, "var1");

    // Add symbol to the innermost scope.
    try st.insert(Symbol{ .name = "var2", .t = pt.createSimpleType(types.SimpleType.U8, std.testing.allocator) });
    try std.testing.expectEqual(st.get("var2").?.name, "var2");

    // Closing the scope destroys the symbol.
    st.close();
    try std.testing.expectEqual(st.get("var2"), null);

    st.close();
}