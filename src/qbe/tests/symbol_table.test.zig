const std = @import("std");
const SymbolTable = @import("../symbol_table.zig").SymbolTable;
const Sigil = @import("../symbol_table.zig").SymbolTable.Sigil;

test "SymbolTable: init creates table with one scope" {
    var symtab = try SymbolTable.init(std.testing.allocator);
    defer symtab.deinit();

    try std.testing.expectEqual(@as(usize, 1), symtab.scopes.items.len);
    try std.testing.expectEqual(@as(u32, 0), symtab.next_var_id);
}

test "SymbolTable: define with function sigil creates correct SSA name" {
    var symtab = try SymbolTable.init(std.testing.allocator);
    defer symtab.deinit();

    const ssa_name = try symtab.define("foo", .function);

    try std.testing.expectEqualStrings("%var0", ssa_name);
    try std.testing.expectEqual(@as(u32, 1), symtab.next_var_id);
}

test "SymbolTable: define with global sigil creates correct SSA name" {
    var symtab = try SymbolTable.init(std.testing.allocator);
    defer symtab.deinit();

    const ssa_name = try symtab.define("bar", .global);

    try std.testing.expectEqualStrings("$var0", ssa_name);
    try std.testing.expectEqual(@as(u32, 1), symtab.next_var_id);
}

test "SymbolTable: define increments var_id sequentially" {
    var symtab = try SymbolTable.init(std.testing.allocator);
    defer symtab.deinit();

    const ssa1 = try symtab.define("x", .function);
    const ssa2 = try symtab.define("y", .function);
    const ssa3 = try symtab.define("z", .global);

    try std.testing.expectEqualStrings("%var0", ssa1);
    try std.testing.expectEqualStrings("%var1", ssa2);
    try std.testing.expectEqualStrings("$var2", ssa3);
    try std.testing.expectEqual(@as(u32, 3), symtab.next_var_id);
}

test "SymbolTable: lookup finds variable in current scope" {
    var symtab = try SymbolTable.init(std.testing.allocator);
    defer symtab.deinit();

    _ = try symtab.define("foo", .function);

    const result = symtab.lookup("foo");
    try std.testing.expect(result != null);
    try std.testing.expectEqualStrings("%var0", result.?);
}

test "SymbolTable: lookup returns null for undefined variable" {
    var symtab = try SymbolTable.init(std.testing.allocator);
    defer symtab.deinit();

    const result = symtab.lookup("undefined");
    try std.testing.expect(result == null);
}

test "SymbolTable: pushScope adds new scope" {
    var symtab = try SymbolTable.init(std.testing.allocator);
    defer symtab.deinit();

    try symtab.pushScope();
    try std.testing.expectEqual(@as(usize, 2), symtab.scopes.items.len);

    try symtab.pushScope();
    try std.testing.expectEqual(@as(usize, 3), symtab.scopes.items.len);
}

test "SymbolTable: popScope removes latest scope" {
    var symtab = try SymbolTable.init(std.testing.allocator);
    defer symtab.deinit();

    try symtab.pushScope();
    try symtab.pushScope();
    try std.testing.expectEqual(@as(usize, 3), symtab.scopes.items.len);

    symtab.popScope();
    try std.testing.expectEqual(@as(usize, 2), symtab.scopes.items.len);

    symtab.popScope();
    try std.testing.expectEqual(@as(usize, 1), symtab.scopes.items.len);
}

test "SymbolTable: lookup searches through nested scopes" {
    var symtab = try SymbolTable.init(std.testing.allocator);
    defer symtab.deinit();

    // Define in outer scope
    _ = try symtab.define("outer", .function);

    // Push new scope
    try symtab.pushScope();

    // Define in inner scope
    _ = try symtab.define("inner", .function);

    // Should find both variables
    const outer_result = symtab.lookup("outer");
    try std.testing.expect(outer_result != null);
    try std.testing.expectEqualStrings("%var0", outer_result.?);

    const inner_result = symtab.lookup("inner");
    try std.testing.expect(inner_result != null);
    try std.testing.expectEqualStrings("%var1", inner_result.?);
}

test "SymbolTable: inner scope shadows outer scope" {
    var symtab = try SymbolTable.init(std.testing.allocator);
    defer symtab.deinit();

    // Define "x" in outer scope
    _ = try symtab.define("x", .function);

    // Push new scope
    try symtab.pushScope();

    // Define "x" again in inner scope
    _ = try symtab.define("x", .function);

    // Lookup should find the inner definition
    const result = symtab.lookup("x");
    try std.testing.expect(result != null);
    try std.testing.expectEqualStrings("%var1", result.?);

    // Pop inner scope
    symtab.popScope();

    // Now should find outer definition
    const outer_result = symtab.lookup("x");
    try std.testing.expect(outer_result != null);
    try std.testing.expectEqualStrings("%var0", outer_result.?);
}

test "SymbolTable: variables not accessible after scope pop" {
    var symtab = try SymbolTable.init(std.testing.allocator);
    defer symtab.deinit();

    // Push new scope
    try symtab.pushScope();

    // Define variable in inner scope
    _ = try symtab.define("temp", .function);

    // Should be found
    var result = symtab.lookup("temp");
    try std.testing.expect(result != null);

    // Pop scope
    symtab.popScope();

    // Should not be found anymore
    result = symtab.lookup("temp");
    try std.testing.expect(result == null);
}

test "SymbolTable: mixed function and global variables" {
    var symtab = try SymbolTable.init(std.testing.allocator);
    defer symtab.deinit();

    const global1 = try symtab.define("g1", .global);
    const func1 = try symtab.define("f1", .function);
    const global2 = try symtab.define("g2", .global);
    const func2 = try symtab.define("f2", .function);

    try std.testing.expectEqualStrings("$var0", global1);
    try std.testing.expectEqualStrings("%var1", func1);
    try std.testing.expectEqualStrings("$var2", global2);
    try std.testing.expectEqualStrings("%var3", func2);
}

test "SymbolTable: deeply nested scopes" {
    var symtab = try SymbolTable.init(std.testing.allocator);
    defer symtab.deinit();

    // Create 5 levels of nesting
    var i: usize = 0;
    while (i < 5) : (i += 1) {
        try symtab.pushScope();
        const var_name = try std.fmt.allocPrint(std.testing.allocator, "var{d}", .{i});
        defer std.testing.allocator.free(var_name);
        _ = try symtab.define(var_name, .function);
    }

    try std.testing.expectEqual(@as(usize, 6), symtab.scopes.items.len);

    // Should find all variables
    var j: usize = 0;
    while (j < 5) : (j += 1) {
        const var_name = try std.fmt.allocPrint(std.testing.allocator, "var{d}", .{j});
        defer std.testing.allocator.free(var_name);
        const result = symtab.lookup(var_name);
        try std.testing.expect(result != null);
    }
}
