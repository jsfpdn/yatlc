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
    defined: bool,

    pub fn destroy(self: Symbol, alloc: std.mem.Allocator) void {
        self.t.destroy(alloc);
    }

    pub fn clone(self: Symbol, alloc: std.mem.Allocator) Symbol {
        return Symbol{
            .name = self.name,
            .llvmName = self.llvmName,
            .location = self.location,
            .t = self.t.clone(alloc),
            .defined = self.defined,
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

    pub fn functionDefined(self: *SymbolTable, name: []const u8) bool {
        var it = self.globalIterator();
        while (it.next()) |s| {
            if (std.mem.eql(u8, s.name, name) and s.t.isFunction()) {
                return true;
            }
        }
        return false;
    }

    // Returns whether a symbol with the name is already declared in the currently open scope.
    pub fn declared(self: *SymbolTable, name: []const u8) bool {
        if (self.scopeStack.items.len == 0) {
            @panic("ICE: cannot check currently open scope since there are no scopes");
        }

        return self.scopeStack.items[self.scopeStack.items.len - 1].contains(name);
    }

    pub fn globalIterator(self: *SymbolTable) std.StringHashMap(Symbol).ValueIterator {
        if (self.scopeStack.items.len == 0) {
            @panic("ICE: cannot access global scope since it is not open");
        }

        return self.scopeStack.items[0].valueIterator();
    }
};
