const std = @import("std");
const Allocator = std.mem.Allocator;

pub const Sigil = enum { function, global };

/// Manages scopes and holds associations between variables and SSA temporaries
pub const SymbolTable = struct {
    scopes: std.ArrayList(*Scope),
    next_var_id: u32,
    alloc: Allocator,

    // Scope sub struct
    const Scope = std.StringHashMap(*Temporary);

    /// Return a new empty symbol table
    pub fn init(alloc: Allocator) !SymbolTable {
        var symtab: SymbolTable = .{
            .alloc = alloc,
            .scopes = std.ArrayList(*Scope).empty,
            .next_var_id = 0,
        };
        try symtab.pushScope(); // Init table with 1 scope (main/global)
        return symtab;
    }

    pub fn deinit(self: *SymbolTable) void {
        for (0..(self.scopes.items.len + 1)) |_| {
            self.popScope();
        }
        self.scopes.deinit(self.alloc);
    }

    /// Pushes a new empty scope map into the stack
    pub fn pushScope(self: *SymbolTable) !void {
        const new_scope = try self.alloc.create(Scope);
        new_scope.* = Scope.init(self.alloc);
        try self.scopes.append(self.alloc, new_scope);
    }

    /// Pops the latest scope off the stack
    pub fn popScope(self: *SymbolTable) void {
        if (self.scopes.pop()) |last_scope| {
            // Free all items
            var iter = last_scope.iterator();
            while (iter.next()) |entry| {
                const var_name = entry.key_ptr.*;
                const temp = entry.value_ptr.*;

                self.alloc.free(var_name);
                self.alloc.free(temp.name);
                self.alloc.destroy(temp);
            }
            // Free the map itself
            last_scope.deinit();
            // Free the scope pointer
            self.alloc.destroy(last_scope);
        }
    }

    /// Define a new variable in the latest scope
    pub fn define(self: *SymbolTable, name: []const u8, sigil: Sigil) !*Temporary {
        const last_scope = self.scopes.getLast();

        // Copy name
        const name_copy = try self.alloc.dupe(u8, name); // Make owned copy

        // Create temp and add to map
        const ssa_temporary = try self.createTemporary(sigil);
        try last_scope.put(name_copy, ssa_temporary);

        // Return the name in case it needs to be used
        return ssa_temporary;
    }

    /// Lookup a name to retrieve its ssa_name in the latest scope
    pub fn lookup(self: *SymbolTable, name: []const u8) ?*Temporary {
        // Look through all scopes in reverse order
        var i: usize = self.scopes.items.len;
        while (i > 0) {
            i -= 1;
            const scope = self.scopes.items[i];
            if (scope.*.get(name)) |found| {
                return found;
            }
        }
        // Searched all scopes and didn't find the item
        return null;
    }

    /// Create a new temporary and increment internal counter for temp names
    /// Allocates using stored allocator
    pub fn createTemporary(self: *SymbolTable, sigil: Sigil) !*Temporary {
        const temp = try self.alloc.create(Temporary);
        temp.* = .{ .name = try std.fmt.allocPrint(self.alloc, "var{d}", .{self.next_var_id}), .sigil = sigil };

        // Increment variable number
        self.next_var_id = self.next_var_id + 1;
        return temp;
    }
};

pub const Temporary = struct {
    sigil: Sigil,
    name: []const u8,

    /// Print with allocator
    pub fn print(self: *Temporary, alloc: Allocator) ![]const u8 {
        return std.fmt.allocPrint(alloc, "{s}{s}", .{ switch (self.sigil) {
            .function => "%",
            .global => "$",
        }, self.name });
    }

    /// Write with writer
    pub fn write(self: *Temporary, writer: std.Io.Writer) !void {
        try writer.print("{s}{s}", .{ switch (self.sigil) {
            .function => "%",
            .global => "$",
        }, self.name });
    }
};
