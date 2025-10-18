const std = @import("std");
const Allocator = std.mem.Allocator;

/// Manages scopes and holds associations between variables and SSA temporaries
pub const SymbolTable = struct {
    scopes: std.ArrayList(Scope),
    next_var_id: u32,
    alloc: Allocator,

    // Scope sub struct
    const Scope = std.StringHashMap([]const u8);
    const Sigil = enum { function, global };

    /// Return a new empty symbol table
    pub fn init(alloc: Allocator) SymbolTable {
        const symtab: SymbolTable = .{
            .alloc = alloc,
            .scopes = std.ArrayList(Scope).empty,
            .next_var_id = 0,
        };
        symtab.pushScope(); // Init table with 1 scope (main/global)
        return symtab;
    }

    /// Pushes a new empty scope map into the stack
    pub fn pushScope(self: *SymbolTable) !void {
        const new_scope = Scope.init(self.alloc);
        try self.scopes.append(self.alloc, new_scope);
    }

    /// Pops the latest scope off the stack
    pub fn popScope(self: *SymbolTable) !void {
        const last_scope = self.scopes.pop();
        if (last_scope) |s| {
            // Free all items
            const iter = s.iterator();
            while (iter.next()) |entry| {
                self.alloc.free(entry.key_ptr);
                self.alloc.free(entry.value_ptr);
            }
            // Free the map itself
            s.deinit();
        }
    }

    /// Define a new variable in the latest scope
    pub fn define(self: *SymbolTable, name: []const u8, sigil: Sigil) ![]const u8 {
        const last_scope = self.scopes.getLast();
        const name_copy = self.alloc.dupe(u8, name); // Make owned copy
        const ssa_name = switch (sigil) {
            .global => try std.fmt.allocPrint(self.alloc, "$var{d}", .{self.next_var_id}),
            .function => try std.fmt.allocPrint(self.alloc, "%var{d}", .{self.next_var_id}),
        };

        // Increment variable number
        self.next_var_id = self.next_var_id + 1;
        try last_scope.put(name_copy, ssa_name);

        // Return the name in case it needs to be used
        return ssa_name;
    }

    /// Lookup a name to retrieve its ssa_name in the latest scope
    pub fn lookup(self: *SymbolTable, name: []const u8) !?[]const u8 {
        const i = self.scopes.items.len - 1;
        // Look through all scopes in reverse order
        while (i >= 0) : (i -= 1) {
            const scope = self.scopes.items[i];
            if (scope.get(name)) |found| {
                return found;
            }
        }
        // Searched all scopes and didn't find the item
        return null;
    }
};
