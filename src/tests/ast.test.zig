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

test "TableConstructor Writer - Empty Table" {
    const alloc = testing.allocator;

    // Create empty table
    var table = ast.TableConstructor{
        .token = token.Token{ .type = .lbrace, .literal = "{", .start_pos = 0, .end_pos = 1 },
        .fields = try std.ArrayList(*ast.Field).initCapacity(alloc, 0),
        .allocator = alloc,
    };
    defer table.deinit();

    // Initialize writer
    var writer = std.Io.Writer.Allocating.init(alloc);
    defer writer.deinit();

    try table.write(&writer.writer);
    try testing.expectEqualStrings("{}", writer.written());
}

test "TableConstructor Writer - Array Style Fields" {
    const alloc = testing.allocator;

    // Create number expressions
    const num1_lit = try alloc.create(ast.NumberLiteral);
    defer alloc.destroy(num1_lit);
    num1_lit.* = .{
        .token = token.Token{ .type = .int, .literal = "1", .start_pos = 0, .end_pos = 1 },
        .value = .{ .Integer = 1 },
    };
    const num1_expr = try alloc.create(ast.Expression);
    defer alloc.destroy(num1_expr);
    num1_expr.* = .{ .Number = num1_lit };

    const num2_lit = try alloc.create(ast.NumberLiteral);
    defer alloc.destroy(num2_lit);
    num2_lit.* = .{
        .token = token.Token{ .type = .int, .literal = "2", .start_pos = 0, .end_pos = 1 },
        .value = .{ .Integer = 2 },
    };
    const num2_expr = try alloc.create(ast.Expression);
    defer alloc.destroy(num2_expr);
    num2_expr.* = .{ .Number = num2_lit };

    const num3_lit = try alloc.create(ast.NumberLiteral);
    defer alloc.destroy(num3_lit);
    num3_lit.* = .{
        .token = token.Token{ .type = .int, .literal = "3", .start_pos = 0, .end_pos = 1 },
        .value = .{ .Integer = 3 },
    };
    const num3_expr = try alloc.create(ast.Expression);
    defer alloc.destroy(num3_expr);
    num3_expr.* = .{ .Number = num3_lit };

    // Create array-style fields
    const field1 = try alloc.create(ast.Field);
    defer alloc.destroy(field1);
    field1.* = .{ .ArrayStyle = num1_expr };

    const field2 = try alloc.create(ast.Field);
    defer alloc.destroy(field2);
    field2.* = .{ .ArrayStyle = num2_expr };

    const field3 = try alloc.create(ast.Field);
    defer alloc.destroy(field3);
    field3.* = .{ .ArrayStyle = num3_expr };

    // Create table with fields
    var fields = try std.ArrayList(*ast.Field).initCapacity(alloc, 3);
    defer fields.deinit(alloc);
    try fields.append(alloc, field1);
    try fields.append(alloc, field2);
    try fields.append(alloc, field3);

    var table = ast.TableConstructor{
        .token = token.Token{ .type = .lbrace, .literal = "{", .start_pos = 0, .end_pos = 1 },
        .fields = fields,
        .allocator = alloc,
    };

    // Initialize writer
    var writer = std.Io.Writer.Allocating.init(alloc);
    defer writer.deinit();

    try table.write(&writer.writer);
    try testing.expectEqualStrings("{1, 2, 3}", writer.written());
}

test "TableConstructor Writer - Record Style Fields" {
    const alloc = testing.allocator;

    // Create identifier for x
    const x_ident = try alloc.create(ast.Identifier);
    defer alloc.destroy(x_ident);
    x_ident.* = .{
        .token = token.Token{ .type = .ident, .literal = "x", .start_pos = 0, .end_pos = 1 },
        .value = "x",
    };

    // Create value expression for x (10)
    const x_val_lit = try alloc.create(ast.NumberLiteral);
    defer alloc.destroy(x_val_lit);
    x_val_lit.* = .{
        .token = token.Token{ .type = .int, .literal = "10", .start_pos = 0, .end_pos = 2 },
        .value = .{ .Integer = 10 },
    };
    const x_val_expr = try alloc.create(ast.Expression);
    defer alloc.destroy(x_val_expr);
    x_val_expr.* = .{ .Number = x_val_lit };

    // Create identifier for y
    const y_ident = try alloc.create(ast.Identifier);
    defer alloc.destroy(y_ident);
    y_ident.* = .{
        .token = token.Token{ .type = .ident, .literal = "y", .start_pos = 0, .end_pos = 1 },
        .value = "y",
    };

    // Create value expression for y (20)
    const y_val_lit = try alloc.create(ast.NumberLiteral);
    defer alloc.destroy(y_val_lit);
    y_val_lit.* = .{
        .token = token.Token{ .type = .int, .literal = "20", .start_pos = 0, .end_pos = 2 },
        .value = .{ .Integer = 20 },
    };
    const y_val_expr = try alloc.create(ast.Expression);
    defer alloc.destroy(y_val_expr);
    y_val_expr.* = .{ .Number = y_val_lit };

    // Create record-style fields
    const field1 = try alloc.create(ast.Field);
    defer alloc.destroy(field1);
    field1.* = .{ .RecordStyle = .{ .name = x_ident, .value = x_val_expr } };

    const field2 = try alloc.create(ast.Field);
    defer alloc.destroy(field2);
    field2.* = .{ .RecordStyle = .{ .name = y_ident, .value = y_val_expr } };

    // Create table with fields
    var fields = try std.ArrayList(*ast.Field).initCapacity(alloc, 2);
    defer fields.deinit(alloc);
    try fields.append(alloc, field1);
    try fields.append(alloc, field2);

    var table = ast.TableConstructor{
        .token = token.Token{ .type = .lbrace, .literal = "{", .start_pos = 0, .end_pos = 1 },
        .fields = fields,
        .allocator = alloc,
    };

    // Initialize writer
    var writer = std.Io.Writer.Allocating.init(alloc);
    defer writer.deinit();

    try table.write(&writer.writer);
    try testing.expectEqualStrings("{x = 10, y = 20}", writer.written());
}

test "TableConstructor Writer - Computed Key Fields" {
    const alloc = testing.allocator;

    // Create key expression (identifier "key")
    const key_ident = try alloc.create(ast.Identifier);
    defer alloc.destroy(key_ident);
    key_ident.* = .{
        .token = token.Token{ .type = .ident, .literal = "key", .start_pos = 0, .end_pos = 3 },
        .value = "key",
    };
    const key_expr = try alloc.create(ast.Expression);
    defer alloc.destroy(key_expr);
    key_expr.* = .{ .Identifier = key_ident };

    // Create value expression (string "value")
    const val_lit = try alloc.create(ast.StringLiteral);
    defer alloc.destroy(val_lit);
    val_lit.* = .{
        .token = token.Token{ .type = .string, .literal = "\"value\"", .start_pos = 0, .end_pos = 7 },
        .value = "value",
    };
    const val_expr = try alloc.create(ast.Expression);
    defer alloc.destroy(val_expr);
    val_expr.* = .{ .String = val_lit };

    // Create computed-key field
    const field = try alloc.create(ast.Field);
    defer alloc.destroy(field);
    field.* = .{ .ComputedKey = .{ .key = key_expr, .value = val_expr } };

    // Create table with field
    var fields = try std.ArrayList(*ast.Field).initCapacity(alloc, 1);
    defer fields.deinit(alloc);
    try fields.append(alloc, field);

    var table = ast.TableConstructor{
        .token = token.Token{ .type = .lbrace, .literal = "{", .start_pos = 0, .end_pos = 1 },
        .fields = fields,
        .allocator = alloc,
    };

    // Initialize writer
    var writer = std.Io.Writer.Allocating.init(alloc);
    defer writer.deinit();

    try table.write(&writer.writer);
    try testing.expectEqualStrings("{[key] = \"value\"}", writer.written());
}

test "TableConstructor Writer - Mixed Field Types" {
    const alloc = testing.allocator;

    // Create array-style field (42)
    const num_lit = try alloc.create(ast.NumberLiteral);
    defer alloc.destroy(num_lit);
    num_lit.* = .{
        .token = token.Token{ .type = .int, .literal = "42", .start_pos = 0, .end_pos = 2 },
        .value = .{ .Integer = 42 },
    };
    const num_expr = try alloc.create(ast.Expression);
    defer alloc.destroy(num_expr);
    num_expr.* = .{ .Number = num_lit };

    const field1 = try alloc.create(ast.Field);
    defer alloc.destroy(field1);
    field1.* = .{ .ArrayStyle = num_expr };

    // Create record-style field (name = "test")
    const name_ident = try alloc.create(ast.Identifier);
    defer alloc.destroy(name_ident);
    name_ident.* = .{
        .token = token.Token{ .type = .ident, .literal = "name", .start_pos = 0, .end_pos = 4 },
        .value = "name",
    };

    const name_val_lit = try alloc.create(ast.StringLiteral);
    defer alloc.destroy(name_val_lit);
    name_val_lit.* = .{
        .token = token.Token{ .type = .string, .literal = "\"test\"", .start_pos = 0, .end_pos = 6 },
        .value = "test",
    };
    const name_val_expr = try alloc.create(ast.Expression);
    defer alloc.destroy(name_val_expr);
    name_val_expr.* = .{ .String = name_val_lit };

    const field2 = try alloc.create(ast.Field);
    defer alloc.destroy(field2);
    field2.* = .{ .RecordStyle = .{ .name = name_ident, .value = name_val_expr } };

    // Create computed-key field ([1] = true)
    const key_num_lit = try alloc.create(ast.NumberLiteral);
    defer alloc.destroy(key_num_lit);
    key_num_lit.* = .{
        .token = token.Token{ .type = .int, .literal = "1", .start_pos = 0, .end_pos = 1 },
        .value = .{ .Integer = 1 },
    };
    const key_num_expr = try alloc.create(ast.Expression);
    defer alloc.destroy(key_num_expr);
    key_num_expr.* = .{ .Number = key_num_lit };

    const bool_lit = try alloc.create(ast.BooleanLiteral);
    defer alloc.destroy(bool_lit);
    bool_lit.* = .{
        .token = token.Token{ .type = .true, .literal = "true", .start_pos = 0, .end_pos = 4 },
        .value = true,
    };
    const bool_expr = try alloc.create(ast.Expression);
    defer alloc.destroy(bool_expr);
    bool_expr.* = .{ .Boolean = bool_lit };

    const field3 = try alloc.create(ast.Field);
    defer alloc.destroy(field3);
    field3.* = .{ .ComputedKey = .{ .key = key_num_expr, .value = bool_expr } };

    // Create table with all three field types
    var fields = try std.ArrayList(*ast.Field).initCapacity(alloc, 3);
    defer fields.deinit(alloc);
    try fields.append(alloc, field1);
    try fields.append(alloc, field2);
    try fields.append(alloc, field3);

    var table = ast.TableConstructor{
        .token = token.Token{ .type = .lbrace, .literal = "{", .start_pos = 0, .end_pos = 1 },
        .fields = fields,
        .allocator = alloc,
    };

    // Initialize writer
    var writer = std.Io.Writer.Allocating.init(alloc);
    defer writer.deinit();

    try table.write(&writer.writer);
    try testing.expectEqualStrings("{42, name = \"test\", [1] = true}", writer.written());
}
