const std = @import("std");
const SymbolTable = @import("../symbol_table.zig").SymbolTable;
const Sigil = @import("../symbol_table.zig").Sigil;
const Type = @import("../function.zig").Type;

test "SymbolTable: init creates table with one scope" {
    var symtab = try SymbolTable.init(std.testing.allocator);
    defer symtab.deinit();

    try std.testing.expectEqual(@as(usize, 1), symtab.scopes.items.len);
    try std.testing.expectEqual(@as(u32, 0), symtab.next_var_id);
}

test "SymbolTable: define with function sigil creates correct SSA name" {
    var symtab = try SymbolTable.init(std.testing.allocator);
    defer symtab.deinit();

    const temp = try symtab.define("foo", .function, .w);

    try std.testing.expectEqualStrings("var0", temp.name);
    try std.testing.expectEqual(Sigil.function, temp.sigil);

    const ssa_name = try temp.print(std.testing.allocator);
    defer std.testing.allocator.free(ssa_name);
    try std.testing.expectEqualStrings("%var0", ssa_name);

    try std.testing.expectEqual(@as(u32, 1), symtab.next_var_id);
}

test "SymbolTable: define with global sigil creates correct SSA name" {
    var symtab = try SymbolTable.init(std.testing.allocator);
    defer symtab.deinit();

    const temp = try symtab.define("bar", .global, .w);

    try std.testing.expectEqualStrings("var0", temp.name);
    try std.testing.expectEqual(Sigil.global, temp.sigil);

    const ssa_name = try temp.print(std.testing.allocator);
    defer std.testing.allocator.free(ssa_name);
    try std.testing.expectEqualStrings("$var0", ssa_name);

    try std.testing.expectEqual(@as(u32, 1), symtab.next_var_id);
}

test "SymbolTable: define increments var_id sequentially" {
    var symtab = try SymbolTable.init(std.testing.allocator);
    defer symtab.deinit();

    const temp1 = try symtab.define("x", .function, .w);
    const temp2 = try symtab.define("y", .function, .w);
    const temp3 = try symtab.define("z", .global, .w);

    const ssa1 = try temp1.print(std.testing.allocator);
    defer std.testing.allocator.free(ssa1);
    const ssa2 = try temp2.print(std.testing.allocator);
    defer std.testing.allocator.free(ssa2);
    const ssa3 = try temp3.print(std.testing.allocator);
    defer std.testing.allocator.free(ssa3);

    try std.testing.expectEqualStrings("%var0", ssa1);
    try std.testing.expectEqualStrings("%var1", ssa2);
    try std.testing.expectEqualStrings("$var2", ssa3);
    try std.testing.expectEqual(@as(u32, 3), symtab.next_var_id);
}

test "SymbolTable: lookup finds variable in current scope" {
    var symtab = try SymbolTable.init(std.testing.allocator);
    defer symtab.deinit();

    _ = try symtab.define("foo", .function, .w);

    const result = symtab.lookup("foo");
    try std.testing.expect(result != null);

    const ssa_name = try result.?.print(std.testing.allocator);
    defer std.testing.allocator.free(ssa_name);
    try std.testing.expectEqualStrings("%var0", ssa_name);
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
    _ = try symtab.define("outer", .function, .w);

    // Push new scope
    try symtab.pushScope();

    // Define in inner scope
    _ = try symtab.define("inner", .function, .w);

    // Should find both variables
    const outer_result = symtab.lookup("outer");
    try std.testing.expect(outer_result != null);
    const outer_ssa = try outer_result.?.print(std.testing.allocator);
    defer std.testing.allocator.free(outer_ssa);
    try std.testing.expectEqualStrings("%var0", outer_ssa);

    const inner_result = symtab.lookup("inner");
    try std.testing.expect(inner_result != null);
    const inner_ssa = try inner_result.?.print(std.testing.allocator);
    defer std.testing.allocator.free(inner_ssa);
    try std.testing.expectEqualStrings("%var1", inner_ssa);
}

test "SymbolTable: inner scope shadows outer scope" {
    var symtab = try SymbolTable.init(std.testing.allocator);
    defer symtab.deinit();

    // Define "x" in outer scope
    _ = try symtab.define("x", .function, .w);

    // Push new scope
    try symtab.pushScope();

    // Define "x" again in inner scope
    _ = try symtab.define("x", .function, .w);

    // Lookup should find the inner definition
    const result = symtab.lookup("x");
    try std.testing.expect(result != null);
    const inner_ssa = try result.?.print(std.testing.allocator);
    defer std.testing.allocator.free(inner_ssa);
    try std.testing.expectEqualStrings("%var1", inner_ssa);

    // Pop inner scope
    symtab.popScope();

    // Now should find outer definition
    const outer_result = symtab.lookup("x");
    try std.testing.expect(outer_result != null);
    const outer_ssa = try outer_result.?.print(std.testing.allocator);
    defer std.testing.allocator.free(outer_ssa);
    try std.testing.expectEqualStrings("%var0", outer_ssa);
}

test "SymbolTable: variables not accessible after scope pop" {
    var symtab = try SymbolTable.init(std.testing.allocator);
    defer symtab.deinit();

    // Push new scope
    try symtab.pushScope();

    // Define variable in inner scope
    _ = try symtab.define("temp", .function, .w);

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

    const temp1 = try symtab.define("g1", .global, .w);
    const temp2 = try symtab.define("f1", .function, .w);
    const temp3 = try symtab.define("g2", .global, .w);
    const temp4 = try symtab.define("f2", .function, .w);

    const ssa1 = try temp1.print(std.testing.allocator);
    defer std.testing.allocator.free(ssa1);
    const ssa2 = try temp2.print(std.testing.allocator);
    defer std.testing.allocator.free(ssa2);
    const ssa3 = try temp3.print(std.testing.allocator);
    defer std.testing.allocator.free(ssa3);
    const ssa4 = try temp4.print(std.testing.allocator);
    defer std.testing.allocator.free(ssa4);

    try std.testing.expectEqualStrings("$var0", ssa1);
    try std.testing.expectEqual(Sigil.global, temp1.sigil);
    try std.testing.expectEqualStrings("%var1", ssa2);
    try std.testing.expectEqual(Sigil.function, temp2.sigil);
    try std.testing.expectEqualStrings("$var2", ssa3);
    try std.testing.expectEqual(Sigil.global, temp3.sigil);
    try std.testing.expectEqualStrings("%var3", ssa4);
    try std.testing.expectEqual(Sigil.function, temp4.sigil);
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
        _ = try symtab.define(var_name, .function, .w);
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
