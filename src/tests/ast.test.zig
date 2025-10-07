const std = @import("std");
const testing = std.testing;
const token = @import("../token.zig");
const ast = @import("../ast.zig");

const Identifier = ast.Identifier;
const NameList = ast.NameList;

test "NameList Writer" {
    const alloc = testing.allocator;

    // Make 3 names
    const name1 = try testing.allocator.create(Identifier);
    defer testing.allocator.destroy(name1);
    name1.* = .{ .token = token.Token{ .type = .ident, .literal = "hello", .start_pos = 0, .end_pos = 5 }, .value = "hello" };

    const name2 = try testing.allocator.create(Identifier);
    defer testing.allocator.destroy(name2);
    name2.* = .{ .token = token.Token{ .type = .ident, .literal = "world", .start_pos = 0, .end_pos = 5 }, .value = "world" };

    const name3 = try testing.allocator.create(Identifier);
    defer testing.allocator.destroy(name3);
    name3.* = .{ .token = token.Token{ .type = .ident, .literal = "again", .start_pos = 0, .end_pos = 5 }, .value = "again" };

    // Initialize writer
    var writer = std.Io.Writer.Allocating.init(alloc);
    defer writer.deinit();

    // Test single name
    var nl = try NameList.init(alloc, name1);
    defer nl.deinit();

    try nl.write(&writer.writer);
    try testing.expectEqualStrings("hello", writer.written());
    writer.clearRetainingCapacity();

    // Test 3 names
    try nl.add(name2);
    try nl.add(name3);

    try nl.write(&writer.writer);
    try testing.expectEqualStrings("hello, world, again", writer.written());
}

test "ForNumericStatement Writer" {
    const alloc = testing.allocator;

    // Create loop variable
    const loop_var = try alloc.create(ast.Identifier);
    defer alloc.destroy(loop_var);
    loop_var.* = .{
        .token = token.Token{ .type = .ident, .literal = "i", .start_pos = 0, .end_pos = 1 },
        .value = "i",
    };

    // Create start expression (1)
    const start_expr = try alloc.create(ast.Expression);
    defer alloc.destroy(start_expr);
    const start_num = try alloc.create(ast.NumberLiteral);
    defer alloc.destroy(start_num);
    start_num.* = .{
        .token = token.Token{ .type = .number, .literal = "1", .start_pos = 0, .end_pos = 1 },
        .value = .{ .Integer = 1 },
    };
    start_expr.* = .{ .Number = start_num };

    // Create end expression (10)
    const end_expr = try alloc.create(ast.Expression);
    defer alloc.destroy(end_expr);
    const end_num = try alloc.create(ast.NumberLiteral);
    defer alloc.destroy(end_num);
    end_num.* = .{
        .token = token.Token{ .type = .number, .literal = "10", .start_pos = 0, .end_pos = 2 },
        .value = .{ .Integer = 10 },
    };
    end_expr.* = .{ .Number = end_num };

    // Create step expression (2)
    const step_expr = try alloc.create(ast.Expression);
    defer alloc.destroy(step_expr);
    const step_num = try alloc.create(ast.NumberLiteral);
    defer alloc.destroy(step_num);
    step_num.* = .{
        .token = token.Token{ .type = .number, .literal = "2", .start_pos = 0, .end_pos = 1 },
        .value = .{ .Integer = 2 },
    };
    step_expr.* = .{ .Number = step_num };

    // Create empty block
    const block = try alloc.create(ast.Block);
    defer alloc.destroy(block);
    block.* = .{ .statements = &[_]*ast.Statement{} };

    // Initialize writer
    var writer = std.Io.Writer.Allocating.init(alloc);
    defer writer.deinit();

    // Test without step
    const for_stmt_no_step = ast.ForNumericStatement{
        .token = token.Token{ .type = .t_for, .literal = "for", .start_pos = 0, .end_pos = 3 },
        .var_name = loop_var,
        .start = start_expr,
        .end = end_expr,
        .step = null,
        .block = block,
    };

    try for_stmt_no_step.write(&writer.writer);
    try testing.expectEqualStrings("for i = 1, 10 do  end", writer.written());
    writer.clearRetainingCapacity();

    // Test with step
    const for_stmt_with_step = ast.ForNumericStatement{
        .token = token.Token{ .type = .t_for, .literal = "for", .start_pos = 0, .end_pos = 3 },
        .var_name = loop_var,
        .start = start_expr,
        .end = end_expr,
        .step = step_expr,
        .block = block,
    };

    try for_stmt_with_step.write(&writer.writer);
    try testing.expectEqualStrings("for i = 1, 10, 2 do  end", writer.written());
}

test "ForGenericStatement Writer" {
    const alloc = testing.allocator;

    // Create loop variables (k, v)
    const var_k = try alloc.create(ast.Identifier);
    defer alloc.destroy(var_k);
    var_k.* = .{
        .token = token.Token{ .type = .ident, .literal = "k", .start_pos = 0, .end_pos = 1 },
        .value = "k",
    };

    const var_v = try alloc.create(ast.Identifier);
    defer alloc.destroy(var_v);
    var_v.* = .{
        .token = token.Token{ .type = .ident, .literal = "v", .start_pos = 0, .end_pos = 1 },
        .value = "v",
    };

    // Create namelist
    var names = try ast.NameList.init(alloc, var_k);
    defer names.deinit();
    try names.add(var_v);

    const names_ptr = try alloc.create(ast.NameList);
    defer alloc.destroy(names_ptr);
    names_ptr.* = names;

    // Create iterator expression (pairs)
    const pairs_ident = try alloc.create(ast.Identifier);
    defer alloc.destroy(pairs_ident);
    pairs_ident.* = .{
        .token = token.Token{ .type = .ident, .literal = "pairs", .start_pos = 0, .end_pos = 5 },
        .value = "pairs",
    };

    const pairs_expr = try alloc.create(ast.Expression);
    defer alloc.destroy(pairs_expr);
    pairs_expr.* = .{ .Identifier = pairs_ident };

    // Create second iterator expression (t)
    const t_ident = try alloc.create(ast.Identifier);
    defer alloc.destroy(t_ident);
    t_ident.* = .{
        .token = token.Token{ .type = .ident, .literal = "t", .start_pos = 0, .end_pos = 1 },
        .value = "t",
    };

    const t_expr = try alloc.create(ast.Expression);
    defer alloc.destroy(t_expr);
    t_expr.* = .{ .Identifier = t_ident };

    // Create expressions list
    var expressions = try std.ArrayList(*ast.Expression).initCapacity(alloc, 2);
    defer expressions.deinit(alloc);
    try expressions.append(alloc, pairs_expr);

    // Create empty block
    const block = try alloc.create(ast.Block);
    defer alloc.destroy(block);
    block.* = .{ .statements = &[_]*ast.Statement{} };

    // Initialize writer
    var writer = std.Io.Writer.Allocating.init(alloc);
    defer writer.deinit();

    // Test with single expression
    var for_stmt_single = ast.ForGenericStatement{
        .token = token.Token{ .type = .t_for, .literal = "for", .start_pos = 0, .end_pos = 3 },
        .names = names_ptr,
        .expressions = expressions,
        .block = block,
        .allocator = alloc,
    };

    try for_stmt_single.write(&writer.writer);
    try testing.expectEqualStrings("for k, v in pairs do  end", writer.written());
    writer.clearRetainingCapacity();

    // Test with multiple expressions
    try for_stmt_single.expressions.append(alloc, t_expr);
    try for_stmt_single.write(&writer.writer);
    try testing.expectEqualStrings("for k, v in pairs, t do  end", writer.written());
}
