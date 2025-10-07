const std = @import("std");
const lex = @import("../lexer.zig");
const Parser = @import("../parser.zig").Parser;
const ast = @import("../ast.zig");

test "create parser over string" {
    const input = "local x = 3";
    const allocator = std.testing.allocator;
    var lexer = try lex.Lexer.init(allocator, input);
    defer lexer.deinit();

    var parser = try Parser.init(allocator, &lexer);
    defer parser.deinit();
    try std.testing.expectEqualStrings(parser.current_token.literal, "local");
    try std.testing.expectEqualStrings(parser.peek_token.literal, "x");
}

test "local statements" {
    const input =
        \\local x = 3;
        \\local foo = 400;
    ;

    const allocator = std.testing.allocator;
    var lexer = try lex.Lexer.init(allocator, input);
    var parser = try Parser.init(allocator, &lexer);
    defer lexer.deinit();
    defer parser.deinit();
    const program = try parser.parseProgram();
    try assertNoErrors(&parser);
    try std.testing.expectEqual(2, program.statements.len);
    try testAssignmentStatement(program.statements[0], "x", true);
    try testAssignmentStatement(program.statements[1], "foo", true);
}

test "return statements" {
    const input =
        \\return x;
        \\return 3;
        \\return;
    ;

    const allocator = std.testing.allocator;
    var lexer = try lex.Lexer.init(allocator, input);
    var parser = try Parser.init(allocator, &lexer);
    defer lexer.deinit();
    defer parser.deinit();
    const program = try parser.parseProgram();
    try assertNoErrors(&parser);
    try std.testing.expectEqual(@as(usize, 3), program.statements.len);
    try testReturnStatement(program.statements[0], "");
    try testReturnStatement(program.statements[1], "");
    try testReturnStatement(program.statements[2], "");
}

test "identifier expressions" {
    const input =
        \\foobar;
    ;

    const allocator = std.testing.allocator;
    var lexer = try lex.Lexer.init(allocator, input);
    var parser = try Parser.init(allocator, &lexer);
    defer lexer.deinit();
    defer parser.deinit();
    const program = try parser.parseProgram();
    try assertNoErrors(&parser);
    try std.testing.expectEqual(@as(usize, 1), program.statements.len);
    switch (program.statements[0].*) {
        .Expression => |e| {
            switch (e.expr.*) {
                .Identifier => |ident| {
                    try std.testing.expectEqualStrings("foobar", ident.value);
                },
                else => {
                    unreachable;
                },
            }
        },
        else => unreachable,
    }
}

test "integer expressions" {
    const input =
        \\42;
        \\3.14
    ;

    const allocator = std.testing.allocator;
    var lexer = try lex.Lexer.init(allocator, input);
    var parser = try Parser.init(allocator, &lexer);
    defer lexer.deinit();
    defer parser.deinit();
    const program = try parser.parseProgram();
    try assertNoErrors(&parser);
    try std.testing.expectEqual(@as(usize, 2), program.statements.len);
    switch (program.statements[0].*) {
        .Expression => |e| {
            switch (e.expr.*) {
                .Number => |n| {
                    try std.testing.expectEqual(42, n.value.Integer);
                },
                else => {
                    unreachable;
                },
            }
        },
        else => unreachable,
    }

    switch (program.statements[1].*) {
        .Expression => |e| {
            switch (e.expr.*) {
                .Number => |n| {
                    try std.testing.expectEqual(3.14, n.value.Float);
                },
                else => {
                    unreachable;
                },
            }
        },
        else => unreachable,
    }
}

test "boolean expressions" {
    const input =
        \\true;
        \\false;
    ;

    const allocator = std.testing.allocator;
    var lexer = try lex.Lexer.init(allocator, input);
    var parser = try Parser.init(allocator, &lexer);
    defer lexer.deinit();
    defer parser.deinit();
    const program = try parser.parseProgram();
    try assertNoErrors(&parser);
    try std.testing.expectEqual(@as(usize, 2), program.statements.len);
    switch (program.statements[0].*) {
        .Expression => |e| {
            switch (e.expr.*) {
                .Boolean => |b| {
                    try std.testing.expectEqual(true, b.value);
                    try std.testing.expectEqualStrings("true", e.expr.tokenLiteral());
                },
                else => {
                    unreachable;
                },
            }
        },
        else => unreachable,
    }

    switch (program.statements[1].*) {
        .Expression => |e| {
            switch (e.expr.*) {
                .Boolean => |b| {
                    try std.testing.expectEqual(false, b.value);
                    try std.testing.expectEqualStrings("false", e.expr.tokenLiteral());
                },
                else => {
                    unreachable;
                },
            }
        },
        else => unreachable,
    }
}

test "nil expression" {
    const input = "nil";

    const allocator = std.testing.allocator;
    var lexer = try lex.Lexer.init(allocator, input);
    var parser = try Parser.init(allocator, &lexer);
    defer lexer.deinit();
    defer parser.deinit();
    const program = try parser.parseProgram();
    try assertNoErrors(&parser);
    try std.testing.expectEqual(@as(usize, 1), program.statements.len);
    switch (program.statements[0].*) {
        .Expression => |e| {
            switch (e.expr.*) {
                .Nil => {
                    try std.testing.expectEqualStrings("nil", e.expr.tokenLiteral());
                },
                else => {
                    unreachable;
                },
            }
        },
        else => unreachable,
    }
}

test "infix expressions" {
    const test_cases = .{
        .{
            .input = "5 + 3;",
            .left_value = 5,
            .operator = "+",
            .right_value = 3,
        },
        .{
            .input = "10 - 7;",
            .left_value = 10,
            .operator = "-",
            .right_value = 7,
        },
        .{
            .input = "8 * 2;",
            .left_value = 8,
            .operator = "*",
            .right_value = 2,
        },
        .{
            .input = "15 / 3;",
            .left_value = 15,
            .operator = "/",
            .right_value = 3,
        },
    };

    const allocator = std.testing.allocator;
    inline for (test_cases) |tc| {
        var lexer = try lex.Lexer.init(allocator, tc.input);
        var parser = try Parser.init(allocator, &lexer);
        defer lexer.deinit();
        defer parser.deinit();
        const program = try parser.parseProgram();
        try assertNoErrors(&parser);
        try std.testing.expectEqual(@as(usize, 1), program.statements.len);

        switch (program.statements[0].*) {
            .Expression => |e| {
                switch (e.expr.*) {
                    .Infix => |inf| {
                        try std.testing.expectEqualStrings(tc.operator, inf.operator);
                        switch (inf.left.*) {
                            .Number => |left_int| {
                                try std.testing.expectEqual(tc.left_value, left_int.value.Integer);
                            },
                            else => unreachable,
                        }
                        switch (inf.right.*) {
                            .Number => |right_int| {
                                try std.testing.expectEqual(tc.right_value, right_int.value.Integer);
                            },
                            else => unreachable,
                        }
                    },
                    else => unreachable,
                }
            },
            else => unreachable,
        }
    }
}

test "infix expressions: write" {
    const test_cases = .{
        .{
            .input = "5 + 3;",
            .expected_string = "(5 + 3);",
        },
        .{
            .input = "5 - 3;",
            .expected_string = "(5 - 3);",
        },
        .{
            .input = "5 * 3;",
            .expected_string = "(5 * 3);",
        },
        .{
            .input = "5 / 3;",
            .expected_string = "(5 / 3);",
        },
        .{
            .input = "5 > 3;",
            .expected_string = "(5 > 3);",
        },
        .{
            .input = "5 < 3;",
            .expected_string = "(5 < 3);",
        },
        .{
            .input = "5 == 3;",
            .expected_string = "(5 == 3);",
        },
        .{
            .input = "5 ~= 3;",
            .expected_string = "(5 ~= 3);",
        },
        // Test operator precedence
        .{
            .input = "5 + 3 * 2;",
            .expected_string = "(5 + (3 * 2));",
        },
        .{
            .input = "5 * 3 + 2;",
            .expected_string = "((5 * 3) + 2);",
        },
        .{
            .input = "5 + 3 + 2;",
            .expected_string = "((5 + 3) + 2);",
        },
        // Test with identifiers
        .{
            .input = "x + y;",
            .expected_string = "(x + y);",
        },
    };

    const allocator = std.testing.allocator;
    inline for (test_cases) |tc| {
        var lexer = try lex.Lexer.init(allocator, tc.input);
        var parser = try Parser.init(allocator, &lexer);
        defer lexer.deinit();
        defer parser.deinit();
        const program = try parser.parseProgram();
        try assertNoErrors(&parser);

        var writer = std.Io.Writer.Allocating.init(allocator);
        defer writer.deinit();
        try program.write(&writer.writer);

        try std.testing.expectEqualStrings(tc.expected_string, writer.written());
    }
}

test "prefix expressions" {
    const test_cases = .{
        .{
            .input = "-15;",
            .operator = "-",
            .value = 15,
        },
        .{
            .input = "not 5;",
            .operator = "not",
            .value = 5,
        },
    };

    const allocator = std.testing.allocator;
    inline for (test_cases) |tc| {
        var lexer = try lex.Lexer.init(allocator, tc.input);
        var parser = try Parser.init(allocator, &lexer);
        defer lexer.deinit();
        defer parser.deinit();
        const program = try parser.parseProgram();
        try assertNoErrors(&parser);
        try std.testing.expectEqual(@as(usize, 1), program.statements.len);

        switch (program.statements[0].*) {
            .Expression => |e| {
                switch (e.expr.*) {
                    .Prefix => |p| {
                        try std.testing.expectEqualStrings(tc.operator, p.operator);
                        switch (p.expression.*) {
                            .Number => |int| {
                                try std.testing.expectEqual(tc.value, int.value.Integer);
                            },
                            else => unreachable,
                        }
                    },
                    else => unreachable,
                }
            },
            else => unreachable,
        }
    }
}

test "if expressions" {
    const input = "if (x < y) then x end";

    const allocator = std.testing.allocator;
    var lexer = try lex.Lexer.init(allocator, input);
    var parser = try Parser.init(allocator, &lexer);
    defer lexer.deinit();
    defer parser.deinit();
    const program = parser.parseProgram() catch |err| switch (err) {
        Parser.ParserError.fail => {
            try assertNoErrors(&parser);
            return;
        },
        else => unreachable,
    };
    try std.testing.expectEqual(@as(usize, 1), program.statements.len);

    // Test that we got an expression statement
    switch (program.statements[0].*) {
        .Expression => |e| {
            switch (e.expr.*) {
                .Conditional => |cond| {
                    // Test the condition is (x < y)
                    switch (cond.condition.*) {
                        .Infix => |inf| {
                            try std.testing.expectEqualStrings("<", inf.operator);
                            switch (inf.left.*) {
                                .Identifier => |left_id| {
                                    try std.testing.expectEqualStrings("x", left_id.value);
                                },
                                else => unreachable,
                            }
                            switch (inf.right.*) {
                                .Identifier => |right_id| {
                                    try std.testing.expectEqualStrings("y", right_id.value);
                                },
                                else => unreachable,
                            }
                        },
                        else => unreachable,
                    }

                    // Test the consequence block contains x
                    try std.testing.expectEqual(@as(usize, 1), cond.then_block.statements.len);
                    switch (cond.then_block.statements[0].*) {
                        .Expression => |cons_expr| {
                            switch (cons_expr.expr.*) {
                                .Identifier => |id| {
                                    try std.testing.expectEqualStrings("x", id.value);
                                },
                                else => unreachable,
                            }
                        },
                        else => unreachable,
                    }

                    // Test there is no alternative
                    try std.testing.expect(cond.else_block == null);
                },
                else => unreachable,
            }
        },
        else => unreachable,
    }
}

test "if else expressions" {
    const input = "if (x < y) then x else y end ";

    const allocator = std.testing.allocator;
    var lexer = try lex.Lexer.init(allocator, input);
    var parser = try Parser.init(allocator, &lexer);
    defer lexer.deinit();
    defer parser.deinit();
    const program = try parser.parseProgram();
    try assertNoErrors(&parser);
    try std.testing.expectEqual(@as(usize, 1), program.statements.len);

    switch (program.statements[0].*) {
        .Expression => |e| {
            switch (e.expr.*) {
                .Conditional => |cond| {
                    // Test condition (x < y)
                    switch (cond.condition.*) {
                        .Infix => |inf| {
                            try std.testing.expectEqualStrings("<", inf.operator);
                            switch (inf.left.*) {
                                .Identifier => |left_id| {
                                    try std.testing.expectEqualStrings("x", left_id.value);
                                },
                                else => unreachable,
                            }
                            switch (inf.right.*) {
                                .Identifier => |right_id| {
                                    try std.testing.expectEqualStrings("y", right_id.value);
                                },
                                else => unreachable,
                            }
                        },
                        else => unreachable,
                    }

                    // Test consequence
                    try std.testing.expectEqual(@as(usize, 1), cond.then_block.statements.len);
                    switch (cond.then_block.statements[0].*) {
                        .Expression => |cons_expr| {
                            switch (cons_expr.expr.*) {
                                .Identifier => |id| {
                                    try std.testing.expectEqualStrings("x", id.value);
                                },
                                else => unreachable,
                            }
                        },
                        else => unreachable,
                    }

                    // Test alternative
                    try std.testing.expect(cond.else_block != null);
                    try std.testing.expectEqual(@as(usize, 1), cond.else_block.?.statements.len);
                    switch (cond.else_block.?.statements[0].*) {
                        .Expression => |alt_expr| {
                            switch (alt_expr.expr.*) {
                                .Identifier => |id| {
                                    try std.testing.expectEqualStrings("y", id.value);
                                },
                                else => unreachable,
                            }
                        },
                        else => unreachable,
                    }
                },
                else => unreachable,
            }
        },
        else => unreachable,
    }
}

test "if expression string representation" {
    const test_cases = .{
        .{
            .input = "if (x < y) then x end",
            .expected_string = "if (x < y) then x; end;",
        },
        .{
            .input = "if (x < y) then x else y end",
            .expected_string = "if (x < y) then x; else y; end;",
        },
    };

    const allocator = std.testing.allocator;
    inline for (test_cases) |tc| {
        var lexer = try lex.Lexer.init(allocator, tc.input);
        var parser = try Parser.init(allocator, &lexer);
        defer lexer.deinit();
        defer parser.deinit();
        const program = try parser.parseProgram();
        try assertNoErrors(&parser);

        var list = std.Io.Writer.Allocating.init(allocator);
        defer list.deinit();
        try program.write(&list.writer);
        try std.testing.expectEqualStrings(tc.expected_string, list.written());
    }
}

test "string writer" {
    const test_cases = .{ .{
        .input = "local x=10;",
        .expected_string = "local x = 10;",
    }, .{
        .input = "myVar;",
        .expected_string = "myVar;",
    }, .{
        .input = "42;",
        .expected_string = "42;",
    }, .{
        .input = "return 3;",
        .expected_string = "return 3;",
    }, .{ .input = "(3 + (3 + 3))", .expected_string = "(3 + (3 + 3));" } };

    const allocator = std.testing.allocator;
    inline for (test_cases) |tc| {
        var lexer = try lex.Lexer.init(allocator, tc.input);
        var parser = try Parser.init(allocator, &lexer);
        try assertNoErrors(&parser);
        defer lexer.deinit();
        defer parser.deinit();
        const program = try parser.parseProgram();
        var list = std.Io.Writer.Allocating.init(allocator);
        defer list.deinit();
        try program.write(&list.writer);
        try std.testing.expectEqualStrings(tc.expected_string, list.written());
    }
}

fn testAssignmentStatement(s: *ast.Statement, name: []const u8, is_local: bool) !void {
    if (is_local) {
        try std.testing.expectEqualStrings("local", s.tokenLiteral());
    }

    switch (s.*) {
        .Assignment => |as| {
            try std.testing.expectEqual(is_local, as.is_local);
            try std.testing.expectEqual(@as(usize, 1), as.names.names.items.len);
            try std.testing.expectEqualStrings(name, as.names.names.items[0].value);
            try std.testing.expectEqualStrings(name, as.names.names.items[0].tokenLiteral());
        },
        else => unreachable,
    }
}

fn testReturnStatement(s: *ast.Statement, _: []const u8) !void {
    try std.testing.expectEqualStrings("return", s.tokenLiteral());

    switch (s.*) {
        .Return => {
            // TODO: test the expression
            try std.testing.expect(true);
        },
        else => unreachable,
    }
}

fn assertNoErrors(p: *Parser) !void {
    if (p.parser_error != null) {
        // var buf: [1024]u8 = undefined;
        std.debug.print("ERROR: {s} with input: \"{s}\" at {s} \n", .{ p.parser_error.?.*, p.lexer.input, p.current_token.literal });
    }
    try std.testing.expectEqual(p.parser_error, null);
}

test "bitwise operators" {
    const test_cases = .{
        .{ .input = "5 & 3;", .expected = "(5 & 3);" },
        .{ .input = "5 | 3;", .expected = "(5 | 3);" },
        .{ .input = "5 ~ 3;", .expected = "(5 ~ 3);" },
        .{ .input = "5 << 2;", .expected = "(5 << 2);" },
        .{ .input = "16 >> 2;", .expected = "(16 >> 2);" },
        // Precedence: & > ~ > |
        .{ .input = "1 | 2 ~ 3 & 4;", .expected = "(1 | (2 ~ (3 & 4)));" },
    };

    const allocator = std.testing.allocator;
    inline for (test_cases) |tc| {
        var lexer = try lex.Lexer.init(allocator, tc.input);
        var parser = try Parser.init(allocator, &lexer);
        defer lexer.deinit();
        defer parser.deinit();
        const program = try parser.parseProgram();
        try assertNoErrors(&parser);

        var writer = std.Io.Writer.Allocating.init(allocator);
        defer writer.deinit();
        try program.write(&writer.writer);
        try std.testing.expectEqualStrings(tc.expected, writer.written());
    }
}

test "new arithmetic operators" {
    const test_cases = .{
        .{ .input = "10 % 3;", .expected = "(10 % 3);" },
        .{ .input = "10 // 3;", .expected = "(10 // 3);" },
        .{ .input = "2 ^ 3;", .expected = "(2 ^ 3);" },
        // Exponentiation is right-associative and highest precedence
        .{ .input = "2 ^ 3 ^ 2;", .expected = "(2 ^ (3 ^ 2));" },
        .{ .input = "2 + 3 ^ 2;", .expected = "(2 + (3 ^ 2));" },
        .{ .input = "2 * 3 % 4;", .expected = "((2 * 3) % 4);" },
    };

    const allocator = std.testing.allocator;
    inline for (test_cases) |tc| {
        var lexer = try lex.Lexer.init(allocator, tc.input);
        var parser = try Parser.init(allocator, &lexer);
        defer lexer.deinit();
        defer parser.deinit();
        const program = try parser.parseProgram();
        try assertNoErrors(&parser);

        var writer = std.Io.Writer.Allocating.init(allocator);
        defer writer.deinit();
        try program.write(&writer.writer);
        try std.testing.expectEqualStrings(tc.expected, writer.written());
    }
}

test "string concatenation (using identifiers)" {
    const test_cases = .{
        .{ .input = "a .. b;", .expected = "(a .. b);" },
        // Concatenation is right-associative
        .{ .input = "a .. b .. c;", .expected = "(a .. (b .. c));" },
    };

    const allocator = std.testing.allocator;
    inline for (test_cases) |tc| {
        var lexer = try lex.Lexer.init(allocator, tc.input);
        var parser = try Parser.init(allocator, &lexer);
        defer lexer.deinit();
        defer parser.deinit();
        const program = try parser.parseProgram();
        try assertNoErrors(&parser);

        var writer = std.Io.Writer.Allocating.init(allocator);
        defer writer.deinit();
        try program.write(&writer.writer);
        try std.testing.expectEqualStrings(tc.expected, writer.written());
    }
}

test "logical operators" {
    const test_cases = .{
        .{ .input = "true and false;", .expected = "(true and false);" },
        .{ .input = "true or false;", .expected = "(true or false);" },
        // or has lower precedence than and
        .{ .input = "true or false and false;", .expected = "(true or (false and false));" },
        .{ .input = "1 < 2 and 3 > 2;", .expected = "((1 < 2) and (3 > 2));" },
    };

    const allocator = std.testing.allocator;
    inline for (test_cases) |tc| {
        var lexer = try lex.Lexer.init(allocator, tc.input);
        var parser = try Parser.init(allocator, &lexer);
        defer lexer.deinit();
        defer parser.deinit();
        const program = try parser.parseProgram();
        try assertNoErrors(&parser);

        var writer = std.Io.Writer.Allocating.init(allocator);
        defer writer.deinit();
        try program.write(&writer.writer);
        try std.testing.expectEqualStrings(tc.expected, writer.written());
    }
}

test "prefix operators" {
    const test_cases = .{
        .{ .input = "#mytable;", .expected = "(#mytable);" },
        .{ .input = "~5;", .expected = "(~5);" },
        .{ .input = "#(x + y);", .expected = "(#(x + y));" },
    };

    const allocator = std.testing.allocator;
    inline for (test_cases) |tc| {
        var lexer = try lex.Lexer.init(allocator, tc.input);
        var parser = try Parser.init(allocator, &lexer);
        defer lexer.deinit();
        defer parser.deinit();
        const program = try parser.parseProgram();
        try assertNoErrors(&parser);

        var writer = std.Io.Writer.Allocating.init(allocator);
        defer writer.deinit();
        try program.write(&writer.writer);
        try std.testing.expectEqualStrings(tc.expected, writer.written());
    }
}

test "comparison operators" {
    const test_cases = .{
        .{ .input = "5 <= 10;", .expected = "(5 <= 10);" },
        .{ .input = "10 >= 5;", .expected = "(10 >= 5);" },
    };

    const allocator = std.testing.allocator;
    inline for (test_cases) |tc| {
        var lexer = try lex.Lexer.init(allocator, tc.input);
        var parser = try Parser.init(allocator, &lexer);
        defer lexer.deinit();
        defer parser.deinit();
        const program = try parser.parseProgram();
        try assertNoErrors(&parser);

        var writer = std.Io.Writer.Allocating.init(allocator);
        defer writer.deinit();
        try program.write(&writer.writer);
        try std.testing.expectEqualStrings(tc.expected, writer.written());
    }
}

test "string literal parsing" {
    const test_cases = .{
        .{ .input = "\"hello world\"", .expected = "\"hello world\";" },
    };

    const allocator = std.testing.allocator;
    inline for (test_cases) |tc| {
        var lexer = try lex.Lexer.init(allocator, tc.input);
        var parser = try Parser.init(allocator, &lexer);
        defer lexer.deinit();
        defer parser.deinit();
        const program = try parser.parseProgram();
        try assertNoErrors(&parser);

        var writer = std.Io.Writer.Allocating.init(allocator);
        defer writer.deinit();
        try program.write(&writer.writer);
        try std.testing.expectEqualStrings(tc.expected, writer.written());
    }
}

test "complex operator precedence" {
    const test_cases = .{
        // Exponent > unary > product > sum > shift > bitwise_and > bitwise_xor > bitwise_or > comparison > and > or
        .{ .input = "1 + 2 * 3 ^ 4;", .expected = "(1 + (2 * (3 ^ 4)));" },
        .{ .input = "a or b and c < d;", .expected = "(a or (b and (c < d)));" },
        .{ .input = "x << 2 + 3;", .expected = "(x << (2 + 3));" },
        .{ .input = "-2 ^ 2;", .expected = "(-(2 ^ 2));" },
    };

    const allocator = std.testing.allocator;
    inline for (test_cases) |tc| {
        var lexer = try lex.Lexer.init(allocator, tc.input);
        var parser = try Parser.init(allocator, &lexer);
        defer lexer.deinit();
        defer parser.deinit();
        const program = try parser.parseProgram();
        try assertNoErrors(&parser);

        var writer = std.Io.Writer.Allocating.init(allocator);
        defer writer.deinit();
        try program.write(&writer.writer);
        try std.testing.expectEqualStrings(tc.expected, writer.written());
    }
}

test "varargs expression" {
    const input = "...";

    const allocator = std.testing.allocator;
    var lexer = try lex.Lexer.init(allocator, input);
    var parser = try Parser.init(allocator, &lexer);
    defer lexer.deinit();
    defer parser.deinit();
    const program = try parser.parseProgram();
    try assertNoErrors(&parser);
    try std.testing.expectEqual(@as(usize, 1), program.statements.len);
    switch (program.statements[0].*) {
        .Expression => |e| {
            switch (e.expr.*) {
                .Varargs => {
                    try std.testing.expectEqualStrings("...", e.expr.tokenLiteral());
                },
                else => {
                    unreachable;
                },
            }
        },
        else => unreachable,
    }
}

test "varargs in return statement" {
    const input = "return ...;";

    const allocator = std.testing.allocator;
    var lexer = try lex.Lexer.init(allocator, input);
    var parser = try Parser.init(allocator, &lexer);
    defer lexer.deinit();
    defer parser.deinit();
    const program = try parser.parseProgram();
    try assertNoErrors(&parser);
    try std.testing.expectEqual(@as(usize, 1), program.statements.len);

    switch (program.statements[0].*) {
        .Return => |ret| {
            try std.testing.expect(ret.expr != null);
            switch (ret.expr.?.*) {
                .Varargs => {
                    try std.testing.expectEqualStrings("...", ret.expr.?.tokenLiteral());
                },
                else => unreachable,
            }
        },
        else => unreachable,
    }
}

test "varargs in assignment" {
    const input = "local x = ...;";

    const allocator = std.testing.allocator;
    var lexer = try lex.Lexer.init(allocator, input);
    var parser = try Parser.init(allocator, &lexer);
    defer lexer.deinit();
    defer parser.deinit();
    const program = try parser.parseProgram();
    try assertNoErrors(&parser);
    try std.testing.expectEqual(@as(usize, 1), program.statements.len);

    switch (program.statements[0].*) {
        .Assignment => |assign| {
            try std.testing.expectEqual(@as(usize, 1), assign.names.names.items.len);
            try std.testing.expectEqualStrings("x", assign.names.names.items[0].value);
            switch (assign.expr.*) {
                .Varargs => {
                    try std.testing.expectEqualStrings("...", assign.expr.tokenLiteral());
                },
                else => unreachable,
            }
        },
        else => unreachable,
    }
}

test "varargs string representation" {
    const test_cases = .{
        .{ .input = "...", .expected = "...;" },
        .{ .input = "return ...", .expected = "return ...;" },
        .{ .input = "local x = ...", .expected = "local x = ...;" },
    };

    const allocator = std.testing.allocator;
    inline for (test_cases) |tc| {
        var lexer = try lex.Lexer.init(allocator, tc.input);
        var parser = try Parser.init(allocator, &lexer);
        defer lexer.deinit();
        defer parser.deinit();
        const program = try parser.parseProgram();
        try assertNoErrors(&parser);

        var writer = std.Io.Writer.Allocating.init(allocator);
        defer writer.deinit();
        try program.write(&writer.writer);
        try std.testing.expectEqualStrings(tc.expected, writer.written());
    }
}

test "varargs vs concatenation disambiguation" {
    // Ensure ... is parsed as varargs, not as .. followed by something else
    const test_cases = .{
        .{ .input = "a .. b", .expected = "(a .. b);" },
        .{ .input = "...", .expected = "...;" },
    };

    const allocator = std.testing.allocator;
    inline for (test_cases) |tc| {
        var lexer = try lex.Lexer.init(allocator, tc.input);
        var parser = try Parser.init(allocator, &lexer);
        defer lexer.deinit();
        defer parser.deinit();
        const program = try parser.parseProgram();
        try assertNoErrors(&parser);

        var writer = std.Io.Writer.Allocating.init(allocator);
        defer writer.deinit();
        try program.write(&writer.writer);
        try std.testing.expectEqualStrings(tc.expected, writer.written());
    }
}

test "non-local assignment" {
    const input = "x = 5;";

    const allocator = std.testing.allocator;
    var lexer = try lex.Lexer.init(allocator, input);
    var parser = try Parser.init(allocator, &lexer);
    defer lexer.deinit();
    defer parser.deinit();
    const program = try parser.parseProgram();
    try assertNoErrors(&parser);
    try std.testing.expectEqual(@as(usize, 1), program.statements.len);

    switch (program.statements[0].*) {
        .Assignment => |assign| {
            try std.testing.expectEqual(false, assign.is_local);
            try std.testing.expectEqual(@as(usize, 1), assign.names.names.items.len);
            try std.testing.expectEqualStrings("x", assign.names.names.items[0].value);
        },
        else => unreachable,
    }
}

test "multiple name assignment - local" {
    const input = "local x, y, z = 1;";

    const allocator = std.testing.allocator;
    var lexer = try lex.Lexer.init(allocator, input);
    var parser = try Parser.init(allocator, &lexer);
    defer lexer.deinit();
    defer parser.deinit();
    const program = try parser.parseProgram();
    try assertNoErrors(&parser);
    try std.testing.expectEqual(@as(usize, 1), program.statements.len);

    switch (program.statements[0].*) {
        .Assignment => |assign| {
            try std.testing.expectEqual(true, assign.is_local);
            try std.testing.expectEqual(@as(usize, 3), assign.names.names.items.len);
            try std.testing.expectEqualStrings("x", assign.names.names.items[0].value);
            try std.testing.expectEqualStrings("y", assign.names.names.items[1].value);
            try std.testing.expectEqualStrings("z", assign.names.names.items[2].value);
        },
        else => unreachable,
    }
}

test "multiple name assignment - non-local" {
    const input = "x, y, z = 1;";

    const allocator = std.testing.allocator;
    var lexer = try lex.Lexer.init(allocator, input);
    var parser = try Parser.init(allocator, &lexer);
    defer lexer.deinit();
    defer parser.deinit();
    const program = try parser.parseProgram();
    try assertNoErrors(&parser);
    try std.testing.expectEqual(@as(usize, 1), program.statements.len);

    switch (program.statements[0].*) {
        .Assignment => |assign| {
            try std.testing.expectEqual(false, assign.is_local);
            try std.testing.expectEqual(@as(usize, 3), assign.names.names.items.len);
            try std.testing.expectEqualStrings("x", assign.names.names.items[0].value);
            try std.testing.expectEqualStrings("y", assign.names.names.items[1].value);
            try std.testing.expectEqualStrings("z", assign.names.names.items[2].value);
        },
        else => unreachable,
    }
}

test "assignment string representation" {
    const test_cases = .{
        .{ .input = "local x = 5", .expected = "local x = 5;" },
        .{ .input = "x = 5", .expected = "x = 5;" },
        .{ .input = "local x, y = 10", .expected = "local x, y = 10;" },
        .{ .input = "a, b, c = 42", .expected = "a, b, c = 42;" },
    };

    const allocator = std.testing.allocator;
    inline for (test_cases) |tc| {
        var lexer = try lex.Lexer.init(allocator, tc.input);
        var parser = try Parser.init(allocator, &lexer);
        defer lexer.deinit();
        defer parser.deinit();
        const program = try parser.parseProgram();
        try assertNoErrors(&parser);

        var writer = std.Io.Writer.Allocating.init(allocator);
        defer writer.deinit();
        try program.write(&writer.writer);
        try std.testing.expectEqualStrings(tc.expected, writer.written());
    }
}

test "function definition - no parameters" {
    const input = "function() end";

    const allocator = std.testing.allocator;
    var lexer = try lex.Lexer.init(allocator, input);
    var parser = try Parser.init(allocator, &lexer);
    defer lexer.deinit();
    defer parser.deinit();
    const program = try parser.parseProgram();
    try assertNoErrors(&parser);
    try std.testing.expectEqual(@as(usize, 1), program.statements.len);

    switch (program.statements[0].*) {
        .Expression => |e| {
            switch (e.expr.*) {
                .FunctionDef => |func| {
                    try std.testing.expectEqualStrings("function", func.tokenLiteral());
                    try std.testing.expect(func.body.params == null);
                    try std.testing.expectEqual(@as(usize, 0), func.body.block.statements.len);
                },
                else => unreachable,
            }
        },
        else => unreachable,
    }
}

test "function definition - single parameter" {
    const input = "function(x) end";

    const allocator = std.testing.allocator;
    var lexer = try lex.Lexer.init(allocator, input);
    var parser = try Parser.init(allocator, &lexer);
    defer lexer.deinit();
    defer parser.deinit();
    const program = try parser.parseProgram();
    try assertNoErrors(&parser);
    try std.testing.expectEqual(@as(usize, 1), program.statements.len);

    switch (program.statements[0].*) {
        .Expression => |e| {
            switch (e.expr.*) {
                .FunctionDef => |func| {
                    try std.testing.expect(func.body.params != null);
                    try std.testing.expectEqual(@as(usize, 1), func.body.params.?.names.items.len);
                    try std.testing.expectEqualStrings("x", func.body.params.?.names.items[0].value);
                    try std.testing.expectEqual(false, func.body.params.?.has_varargs);
                },
                else => unreachable,
            }
        },
        else => unreachable,
    }
}

test "function definition - multiple parameters" {
    const input = "function(x, y, z) end";

    const allocator = std.testing.allocator;
    var lexer = try lex.Lexer.init(allocator, input);
    var parser = try Parser.init(allocator, &lexer);
    defer lexer.deinit();
    defer parser.deinit();
    const program = try parser.parseProgram();
    try assertNoErrors(&parser);
    try std.testing.expectEqual(@as(usize, 1), program.statements.len);

    switch (program.statements[0].*) {
        .Expression => |e| {
            switch (e.expr.*) {
                .FunctionDef => |func| {
                    try std.testing.expect(func.body.params != null);
                    try std.testing.expectEqual(@as(usize, 3), func.body.params.?.names.items.len);
                    try std.testing.expectEqualStrings("x", func.body.params.?.names.items[0].value);
                    try std.testing.expectEqualStrings("y", func.body.params.?.names.items[1].value);
                    try std.testing.expectEqualStrings("z", func.body.params.?.names.items[2].value);
                    try std.testing.expectEqual(false, func.body.params.?.has_varargs);
                },
                else => unreachable,
            }
        },
        else => unreachable,
    }
}

test "function definition - only varargs" {
    const input = "function(...) end";

    const allocator = std.testing.allocator;
    var lexer = try lex.Lexer.init(allocator, input);
    var parser = try Parser.init(allocator, &lexer);
    defer lexer.deinit();
    defer parser.deinit();
    const program = try parser.parseProgram();
    try assertNoErrors(&parser);
    try std.testing.expectEqual(@as(usize, 1), program.statements.len);

    switch (program.statements[0].*) {
        .Expression => |e| {
            switch (e.expr.*) {
                .FunctionDef => |func| {
                    try std.testing.expect(func.body.params != null);
                    try std.testing.expectEqual(@as(usize, 0), func.body.params.?.names.items.len);
                    try std.testing.expectEqual(true, func.body.params.?.has_varargs);
                },
                else => unreachable,
            }
        },
        else => unreachable,
    }
}

test "function definition - parameters with varargs" {
    const input = "function(a, b, ...) end";

    const allocator = std.testing.allocator;
    var lexer = try lex.Lexer.init(allocator, input);
    var parser = try Parser.init(allocator, &lexer);
    defer lexer.deinit();
    defer parser.deinit();
    const program = try parser.parseProgram();
    try assertNoErrors(&parser);
    try std.testing.expectEqual(@as(usize, 1), program.statements.len);

    switch (program.statements[0].*) {
        .Expression => |e| {
            switch (e.expr.*) {
                .FunctionDef => |func| {
                    try std.testing.expect(func.body.params != null);
                    try std.testing.expectEqual(@as(usize, 2), func.body.params.?.names.items.len);
                    try std.testing.expectEqualStrings("a", func.body.params.?.names.items[0].value);
                    try std.testing.expectEqualStrings("b", func.body.params.?.names.items[1].value);
                    try std.testing.expectEqual(true, func.body.params.?.has_varargs);
                },
                else => unreachable,
            }
        },
        else => unreachable,
    }
}

test "function definition - with body statements" {
    const input = "function(x) return x + 1 end";

    const allocator = std.testing.allocator;
    var lexer = try lex.Lexer.init(allocator, input);
    var parser = try Parser.init(allocator, &lexer);
    defer lexer.deinit();
    defer parser.deinit();
    const program = try parser.parseProgram();
    try assertNoErrors(&parser);
    try std.testing.expectEqual(@as(usize, 1), program.statements.len);

    switch (program.statements[0].*) {
        .Expression => |e| {
            switch (e.expr.*) {
                .FunctionDef => |func| {
                    try std.testing.expectEqual(@as(usize, 1), func.body.block.statements.len);
                    switch (func.body.block.statements[0].*) {
                        .Return => |ret| {
                            try std.testing.expect(ret.expr != null);
                        },
                        else => unreachable,
                    }
                },
                else => unreachable,
            }
        },
        else => unreachable,
    }
}

test "function definition - in assignment" {
    const input = "local f = function(x) return x end";

    const allocator = std.testing.allocator;
    var lexer = try lex.Lexer.init(allocator, input);
    var parser = try Parser.init(allocator, &lexer);
    defer lexer.deinit();
    defer parser.deinit();
    const program = try parser.parseProgram();
    try assertNoErrors(&parser);
    try std.testing.expectEqual(@as(usize, 1), program.statements.len);

    switch (program.statements[0].*) {
        .Assignment => |assign| {
            try std.testing.expectEqual(true, assign.is_local);
            try std.testing.expectEqualStrings("f", assign.names.names.items[0].value);
            switch (assign.expr.*) {
                .FunctionDef => |func| {
                    try std.testing.expectEqualStrings("function", func.tokenLiteral());
                },
                else => unreachable,
            }
        },
        else => unreachable,
    }
}

test "function definition - nested functions" {
    const input = "function() return function() end end";

    const allocator = std.testing.allocator;
    var lexer = try lex.Lexer.init(allocator, input);
    var parser = try Parser.init(allocator, &lexer);
    defer lexer.deinit();
    defer parser.deinit();
    const program = try parser.parseProgram();
    try assertNoErrors(&parser);
    try std.testing.expectEqual(@as(usize, 1), program.statements.len);

    switch (program.statements[0].*) {
        .Expression => |e| {
            switch (e.expr.*) {
                .FunctionDef => |outer_func| {
                    try std.testing.expectEqual(@as(usize, 1), outer_func.body.block.statements.len);
                    switch (outer_func.body.block.statements[0].*) {
                        .Return => |ret| {
                            switch (ret.expr.?.*) {
                                .FunctionDef => |inner_func| {
                                    try std.testing.expectEqualStrings("function", inner_func.tokenLiteral());
                                },
                                else => unreachable,
                            }
                        },
                        else => unreachable,
                    }
                },
                else => unreachable,
            }
        },
        else => unreachable,
    }
}

test "function definition - string representation" {
    const test_cases = .{
        .{ .input = "function() end", .expected = "function() end;" },
        .{ .input = "function(x) end", .expected = "function(x) end;" },
        .{ .input = "function(x, y, z) end", .expected = "function(x, y, z) end;" },
        .{ .input = "function(...) end", .expected = "function(...) end;" },
        .{ .input = "function(a, b, ...) end", .expected = "function(a, b, ...) end;" },
        .{ .input = "function(x) return x end", .expected = "function(x) return x; end;" },
        .{ .input = "local f = function() end", .expected = "local f = function() end;" },
    };

    const allocator = std.testing.allocator;
    inline for (test_cases) |tc| {
        var lexer = try lex.Lexer.init(allocator, tc.input);
        var parser = try Parser.init(allocator, &lexer);
        defer lexer.deinit();
        defer parser.deinit();
        const program = try parser.parseProgram();
        try assertNoErrors(&parser);

        var writer = std.Io.Writer.Allocating.init(allocator);
        defer writer.deinit();
        try program.write(&writer.writer);
        try std.testing.expectEqualStrings(tc.expected, writer.written());
    }
}

test "function declaration - local" {
    const input = "local function add(x, y) return x + y end";

    const allocator = std.testing.allocator;
    var lexer = try lex.Lexer.init(allocator, input);
    var parser = try Parser.init(allocator, &lexer);
    defer lexer.deinit();
    defer parser.deinit();
    const program = try parser.parseProgram();
    try assertNoErrors(&parser);
    try std.testing.expectEqual(@as(usize, 1), program.statements.len);

    switch (program.statements[0].*) {
        .FunctionDeclaration => |func| {
            try std.testing.expectEqual(true, func.is_local);
            try std.testing.expectEqualStrings("add", func.name.value);
            try std.testing.expectEqualStrings("function", func.tokenLiteral());

            // Check parameters
            try std.testing.expect(func.body.params != null);
            try std.testing.expectEqual(@as(usize, 2), func.body.params.?.names.items.len);
            try std.testing.expectEqualStrings("x", func.body.params.?.names.items[0].value);
            try std.testing.expectEqualStrings("y", func.body.params.?.names.items[1].value);

            // Check body has a return statement
            try std.testing.expectEqual(@as(usize, 1), func.body.block.statements.len);
            switch (func.body.block.statements[0].*) {
                .Return => {},
                else => unreachable,
            }
        },
        else => unreachable,
    }
}

test "function declaration - non-local" {
    const input = "function multiply(a, b, c) return a * b * c end";

    const allocator = std.testing.allocator;
    var lexer = try lex.Lexer.init(allocator, input);
    var parser = try Parser.init(allocator, &lexer);
    defer lexer.deinit();
    defer parser.deinit();
    const program = try parser.parseProgram();
    try assertNoErrors(&parser);
    try std.testing.expectEqual(@as(usize, 1), program.statements.len);

    switch (program.statements[0].*) {
        .FunctionDeclaration => |func| {
            try std.testing.expectEqual(false, func.is_local);
            try std.testing.expectEqualStrings("multiply", func.name.value);
            try std.testing.expectEqualStrings("function", func.tokenLiteral());

            // Check parameters
            try std.testing.expect(func.body.params != null);
            try std.testing.expectEqual(@as(usize, 3), func.body.params.?.names.items.len);
            try std.testing.expectEqualStrings("a", func.body.params.?.names.items[0].value);
            try std.testing.expectEqualStrings("b", func.body.params.?.names.items[1].value);
            try std.testing.expectEqualStrings("c", func.body.params.?.names.items[2].value);

            // Check body has a return statement
            try std.testing.expectEqual(@as(usize, 1), func.body.block.statements.len);
            switch (func.body.block.statements[0].*) {
                .Return => {},
                else => unreachable,
            }
        },
        else => unreachable,
    }
}

test "function declaration - string representation" {
    const test_cases = .{
        .{ .input = "local function add(x, y) return x + y end", .expected = "local function add(x, y) return (x + y); end;" },
        .{ .input = "function multiply(a, b, c) return a * b * c end", .expected = "function multiply(a, b, c) return ((a * b) * c); end;" },
        .{ .input = "local function noParams() end", .expected = "local function noParams() end;" },
        .{ .input = "function withVarargs(...) end", .expected = "function withVarargs(...) end;" },
        .{ .input = "local function mixed(x, ...) end", .expected = "local function mixed(x, ...) end;" },
    };

    const allocator = std.testing.allocator;
    inline for (test_cases) |tc| {
        var lexer = try lex.Lexer.init(allocator, tc.input);
        var parser = try Parser.init(allocator, &lexer);
        defer lexer.deinit();
        defer parser.deinit();
        const program = try parser.parseProgram();
        try assertNoErrors(&parser);

        var writer = std.Io.Writer.Allocating.init(allocator);
        defer writer.deinit();
        try program.write(&writer.writer);
        try std.testing.expectEqualStrings(tc.expected, writer.written());
    }
}
