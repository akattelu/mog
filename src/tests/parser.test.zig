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

test "do statement - empty block" {
    const input = "do end";

    const allocator = std.testing.allocator;
    var lexer = try lex.Lexer.init(allocator, input);
    var parser = try Parser.init(allocator, &lexer);
    defer lexer.deinit();
    defer parser.deinit();
    const program = try parser.parseProgram();
    try assertNoErrors(&parser);
    try std.testing.expectEqual(@as(usize, 1), program.statements.len);

    switch (program.statements[0].*) {
        .Do => |do_stmt| {
            try std.testing.expectEqualStrings("do", do_stmt.tokenLiteral());
            try std.testing.expectEqual(@as(usize, 0), do_stmt.block.statements.len);
        },
        else => unreachable,
    }
}

test "do statement - with statements" {
    const input = "do local x = 5 x = x + 1 end";

    const allocator = std.testing.allocator;
    var lexer = try lex.Lexer.init(allocator, input);
    var parser = try Parser.init(allocator, &lexer);
    defer lexer.deinit();
    defer parser.deinit();
    const program = try parser.parseProgram();
    try assertNoErrors(&parser);
    try std.testing.expectEqual(@as(usize, 1), program.statements.len);

    switch (program.statements[0].*) {
        .Do => |do_stmt| {
            try std.testing.expectEqualStrings("do", do_stmt.tokenLiteral());
            try std.testing.expectEqual(@as(usize, 2), do_stmt.block.statements.len);

            // First statement should be local assignment
            switch (do_stmt.block.statements[0].*) {
                .Assignment => |assign| {
                    try std.testing.expectEqual(true, assign.is_local);
                    try std.testing.expectEqualStrings("x", assign.names.names.items[0].value);
                },
                else => unreachable,
            }

            // Second statement should be non-local assignment
            switch (do_stmt.block.statements[1].*) {
                .Assignment => |assign| {
                    try std.testing.expectEqual(false, assign.is_local);
                    try std.testing.expectEqualStrings("x", assign.names.names.items[0].value);
                },
                else => unreachable,
            }
        },
        else => unreachable,
    }
}

test "do statement - string representation" {
    const test_cases = .{
        .{ .input = "do end", .expected = "do  end;" },
        .{ .input = "do local x = 5 end", .expected = "do local x = 5; end;" },
        .{ .input = "do x = 1 y = 2 end", .expected = "do x = 1;\ny = 2;\n end;" },
        .{ .input = "do return 42 end", .expected = "do return 42; end;" },
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

test "while statement - basic structure" {
    const input = "while x < 10 do end";

    const allocator = std.testing.allocator;
    var lexer = try lex.Lexer.init(allocator, input);
    var parser = try Parser.init(allocator, &lexer);
    defer lexer.deinit();
    defer parser.deinit();
    const program = try parser.parseProgram();
    try assertNoErrors(&parser);
    try std.testing.expectEqual(@as(usize, 1), program.statements.len);

    switch (program.statements[0].*) {
        .While => |while_stmt| {
            try std.testing.expectEqualStrings("while", while_stmt.tokenLiteral());
            try std.testing.expectEqual(@as(usize, 0), while_stmt.block.statements.len);

            // Verify condition is an infix expression
            switch (while_stmt.condition.*) {
                .Infix => |infix| {
                    try std.testing.expectEqualStrings("<", infix.operator);
                },
                else => unreachable,
            }
        },
        else => unreachable,
    }
}

test "while statement - with body" {
    const input = "while true do x = x + 1 end";

    const allocator = std.testing.allocator;
    var lexer = try lex.Lexer.init(allocator, input);
    var parser = try Parser.init(allocator, &lexer);
    defer lexer.deinit();
    defer parser.deinit();
    const program = try parser.parseProgram();
    try assertNoErrors(&parser);
    try std.testing.expectEqual(@as(usize, 1), program.statements.len);

    switch (program.statements[0].*) {
        .While => |while_stmt| {
            try std.testing.expectEqualStrings("while", while_stmt.tokenLiteral());
            try std.testing.expectEqual(@as(usize, 1), while_stmt.block.statements.len);

            // Verify condition is a boolean
            switch (while_stmt.condition.*) {
                .Boolean => |bool_expr| {
                    try std.testing.expectEqual(true, bool_expr.value);
                },
                else => unreachable,
            }

            // Verify body has assignment
            switch (while_stmt.block.statements[0].*) {
                .Assignment => |assign| {
                    try std.testing.expectEqual(false, assign.is_local);
                },
                else => unreachable,
            }
        },
        else => unreachable,
    }
}

test "while statement - string representation" {
    const test_cases = .{
        .{ .input = "while true do end", .expected = "while true do  end;" },
        .{ .input = "while x < 10 do x = x + 1 end", .expected = "while (x < 10) do x = (x + 1); end;" },
        .{ .input = "while i > 0 do i = i - 1 return i end", .expected = "while (i > 0) do i = (i - 1);\nreturn i;\n end;" },
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

test "repeat statement - basic structure" {
    const input = "repeat until x > 10";

    const allocator = std.testing.allocator;
    var lexer = try lex.Lexer.init(allocator, input);
    var parser = try Parser.init(allocator, &lexer);
    defer lexer.deinit();
    defer parser.deinit();
    const program = try parser.parseProgram();
    try assertNoErrors(&parser);
    try std.testing.expectEqual(@as(usize, 1), program.statements.len);

    switch (program.statements[0].*) {
        .Repeat => |repeat_stmt| {
            try std.testing.expectEqualStrings("repeat", repeat_stmt.tokenLiteral());
            try std.testing.expectEqual(@as(usize, 0), repeat_stmt.block.statements.len);

            // Verify condition is an infix expression
            switch (repeat_stmt.condition.*) {
                .Infix => |infix| {
                    try std.testing.expectEqualStrings(">", infix.operator);
                },
                else => unreachable,
            }
        },
        else => unreachable,
    }
}

test "repeat statement - with body" {
    const input = "repeat x = x + 1 until x >= 10";

    const allocator = std.testing.allocator;
    var lexer = try lex.Lexer.init(allocator, input);
    var parser = try Parser.init(allocator, &lexer);
    defer lexer.deinit();
    defer parser.deinit();
    const program = try parser.parseProgram();
    try assertNoErrors(&parser);
    try std.testing.expectEqual(@as(usize, 1), program.statements.len);

    switch (program.statements[0].*) {
        .Repeat => |repeat_stmt| {
            try std.testing.expectEqualStrings("repeat", repeat_stmt.tokenLiteral());
            try std.testing.expectEqual(@as(usize, 1), repeat_stmt.block.statements.len);

            // Verify body has assignment
            switch (repeat_stmt.block.statements[0].*) {
                .Assignment => |assign| {
                    try std.testing.expectEqual(false, assign.is_local);
                    try std.testing.expectEqualStrings("x", assign.names.names.items[0].value);
                },
                else => unreachable,
            }

            // Verify condition
            switch (repeat_stmt.condition.*) {
                .Infix => |infix| {
                    try std.testing.expectEqualStrings(">=", infix.operator);
                },
                else => unreachable,
            }
        },
        else => unreachable,
    }
}

test "repeat statement - string representation" {
    const test_cases = .{
        .{ .input = "repeat until false", .expected = "repeat  until false;" },
        .{ .input = "repeat x = x + 1 until x >= 10", .expected = "repeat x = (x + 1); until (x >= 10);" },
        .{ .input = "repeat i = i - 1 local y = i until i == 0", .expected = "repeat i = (i - 1);\nlocal y = i;\n until (i == 0);" },
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

test "for numeric statement - basic structure (start, end)" {
    const input = "for i = 1, 10 do end";

    const allocator = std.testing.allocator;
    var lexer = try lex.Lexer.init(allocator, input);
    var parser = try Parser.init(allocator, &lexer);
    defer lexer.deinit();
    defer parser.deinit();
    const program = try parser.parseProgram();
    try assertNoErrors(&parser);
    try std.testing.expectEqual(@as(usize, 1), program.statements.len);

    switch (program.statements[0].*) {
        .ForNumeric => |for_stmt| {
            try std.testing.expectEqualStrings("for", for_stmt.tokenLiteral());
            try std.testing.expectEqualStrings("i", for_stmt.var_name.value);
            try std.testing.expectEqual(@as(usize, 0), for_stmt.block.statements.len);

            // Verify start expression is integer 1
            switch (for_stmt.start.*) {
                .Number => |num| {
                    switch (num.value) {
                        .Integer => |int| {
                            try std.testing.expectEqual(@as(i32, 1), int);
                        },
                        else => unreachable,
                    }
                },
                else => unreachable,
            }

            // Verify end expression is integer 10
            switch (for_stmt.end.*) {
                .Number => |num| {
                    switch (num.value) {
                        .Integer => |int| {
                            try std.testing.expectEqual(@as(i32, 10), int);
                        },
                        else => unreachable,
                    }
                },
                else => unreachable,
            }

            // Verify no step expression
            try std.testing.expectEqual(@as(?*ast.Expression, null), for_stmt.step);
        },
        else => unreachable,
    }
}

test "for numeric statement - with step" {
    const input = "for i = 0, 100, 2 do end";

    const allocator = std.testing.allocator;
    var lexer = try lex.Lexer.init(allocator, input);
    var parser = try Parser.init(allocator, &lexer);
    defer lexer.deinit();
    defer parser.deinit();
    const program = try parser.parseProgram();
    try assertNoErrors(&parser);
    try std.testing.expectEqual(@as(usize, 1), program.statements.len);

    switch (program.statements[0].*) {
        .ForNumeric => |for_stmt| {
            try std.testing.expectEqualStrings("for", for_stmt.tokenLiteral());
            try std.testing.expectEqualStrings("i", for_stmt.var_name.value);

            // Verify start, end, and step
            switch (for_stmt.start.*) {
                .Number => |num| {
                    switch (num.value) {
                        .Integer => |int| {
                            try std.testing.expectEqual(@as(i32, 0), int);
                        },
                        else => unreachable,
                    }
                },
                else => unreachable,
            }

            switch (for_stmt.end.*) {
                .Number => |num| {
                    switch (num.value) {
                        .Integer => |int| {
                            try std.testing.expectEqual(@as(i32, 100), int);
                        },
                        else => unreachable,
                    }
                },
                else => unreachable,
            }

            // Verify step exists and is 2
            try std.testing.expect(for_stmt.step != null);
            switch (for_stmt.step.?.*) {
                .Number => |num| {
                    switch (num.value) {
                        .Integer => |int| {
                            try std.testing.expectEqual(@as(i32, 2), int);
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

test "for numeric statement - with body" {
    const input = "for i = 1, 5 do x = x + i end";

    const allocator = std.testing.allocator;
    var lexer = try lex.Lexer.init(allocator, input);
    var parser = try Parser.init(allocator, &lexer);
    defer lexer.deinit();
    defer parser.deinit();
    const program = try parser.parseProgram();
    try assertNoErrors(&parser);
    try std.testing.expectEqual(@as(usize, 1), program.statements.len);

    switch (program.statements[0].*) {
        .ForNumeric => |for_stmt| {
            try std.testing.expectEqualStrings("i", for_stmt.var_name.value);
            try std.testing.expectEqual(@as(usize, 1), for_stmt.block.statements.len);

            // Verify the body contains an assignment
            switch (for_stmt.block.statements[0].*) {
                .Assignment => |assign| {
                    try std.testing.expectEqual(false, assign.is_local);
                    try std.testing.expectEqualStrings("x", assign.names.names.items[0].value);
                },
                else => unreachable,
            }
        },
        else => unreachable,
    }
}

test "for numeric statement - string representation" {
    const test_cases = .{
        .{ .input = "for i = 1, 10 do end", .expected = "for i = 1, 10 do  end;" },
        .{ .input = "for i = 0, 100, 2 do end", .expected = "for i = 0, 100, 2 do  end;" },
        .{ .input = "for i = 1, 5 do x = x + i end", .expected = "for i = 1, 5 do x = (x + i); end;" },
        .{ .input = "for n = 10, 1, -1 do return n end", .expected = "for n = 10, 1, (-1) do return n; end;" },
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

test "for generic statement - single variable" {
    const input = "for x in iter do end";

    const allocator = std.testing.allocator;
    var lexer = try lex.Lexer.init(allocator, input);
    var parser = try Parser.init(allocator, &lexer);
    defer lexer.deinit();
    defer parser.deinit();
    const program = try parser.parseProgram();
    try assertNoErrors(&parser);
    try std.testing.expectEqual(@as(usize, 1), program.statements.len);

    switch (program.statements[0].*) {
        .ForGeneric => |for_stmt| {
            try std.testing.expectEqualStrings("for", for_stmt.tokenLiteral());
            try std.testing.expectEqual(@as(usize, 0), for_stmt.block.statements.len);

            // Verify single loop variable
            try std.testing.expectEqual(@as(usize, 1), for_stmt.names.names.items.len);
            try std.testing.expectEqualStrings("x", for_stmt.names.names.items[0].value);

            // Verify single iterator expression
            try std.testing.expectEqual(@as(usize, 1), for_stmt.expressions.items.len);
            switch (for_stmt.expressions.items[0].*) {
                .Identifier => |ident| {
                    try std.testing.expectEqualStrings("iter", ident.value);
                },
                else => unreachable,
            }
        },
        else => unreachable,
    }
}

test "for generic statement - multiple variables" {
    const input = "for k, v in pairs do end";

    const allocator = std.testing.allocator;
    var lexer = try lex.Lexer.init(allocator, input);
    var parser = try Parser.init(allocator, &lexer);
    defer lexer.deinit();
    defer parser.deinit();
    const program = try parser.parseProgram();
    try assertNoErrors(&parser);
    try std.testing.expectEqual(@as(usize, 1), program.statements.len);

    switch (program.statements[0].*) {
        .ForGeneric => |for_stmt| {
            try std.testing.expectEqualStrings("for", for_stmt.tokenLiteral());

            // Verify two loop variables
            try std.testing.expectEqual(@as(usize, 2), for_stmt.names.names.items.len);
            try std.testing.expectEqualStrings("k", for_stmt.names.names.items[0].value);
            try std.testing.expectEqualStrings("v", for_stmt.names.names.items[1].value);

            // Verify single iterator expression
            try std.testing.expectEqual(@as(usize, 1), for_stmt.expressions.items.len);
            switch (for_stmt.expressions.items[0].*) {
                .Identifier => |ident| {
                    try std.testing.expectEqualStrings("pairs", ident.value);
                },
                else => unreachable,
            }
        },
        else => unreachable,
    }
}

test "for generic statement - multiple iterator expressions" {
    const input = "for x in a, b, c do end";

    const allocator = std.testing.allocator;
    var lexer = try lex.Lexer.init(allocator, input);
    var parser = try Parser.init(allocator, &lexer);
    defer lexer.deinit();
    defer parser.deinit();
    const program = try parser.parseProgram();
    try assertNoErrors(&parser);
    try std.testing.expectEqual(@as(usize, 1), program.statements.len);

    switch (program.statements[0].*) {
        .ForGeneric => |for_stmt| {
            try std.testing.expectEqualStrings("for", for_stmt.tokenLiteral());

            // Verify single loop variable
            try std.testing.expectEqual(@as(usize, 1), for_stmt.names.names.items.len);
            try std.testing.expectEqualStrings("x", for_stmt.names.names.items[0].value);

            // Verify three iterator expressions
            try std.testing.expectEqual(@as(usize, 3), for_stmt.expressions.items.len);
            switch (for_stmt.expressions.items[0].*) {
                .Identifier => |ident| {
                    try std.testing.expectEqualStrings("a", ident.value);
                },
                else => unreachable,
            }
            switch (for_stmt.expressions.items[1].*) {
                .Identifier => |ident| {
                    try std.testing.expectEqualStrings("b", ident.value);
                },
                else => unreachable,
            }
            switch (for_stmt.expressions.items[2].*) {
                .Identifier => |ident| {
                    try std.testing.expectEqualStrings("c", ident.value);
                },
                else => unreachable,
            }
        },
        else => unreachable,
    }
}

test "for generic statement - with body" {
    const input = "for k, v in pairs do x = v end";

    const allocator = std.testing.allocator;
    var lexer = try lex.Lexer.init(allocator, input);
    var parser = try Parser.init(allocator, &lexer);
    defer lexer.deinit();
    defer parser.deinit();
    const program = try parser.parseProgram();
    try assertNoErrors(&parser);
    try std.testing.expectEqual(@as(usize, 1), program.statements.len);

    switch (program.statements[0].*) {
        .ForGeneric => |for_stmt| {
            try std.testing.expectEqual(@as(usize, 2), for_stmt.names.names.items.len);
            try std.testing.expectEqual(@as(usize, 1), for_stmt.block.statements.len);

            // Verify the body contains an assignment
            switch (for_stmt.block.statements[0].*) {
                .Assignment => |assign| {
                    try std.testing.expectEqual(false, assign.is_local);
                    try std.testing.expectEqualStrings("x", assign.names.names.items[0].value);
                },
                else => unreachable,
            }
        },
        else => unreachable,
    }
}

test "for generic statement - string representation" {
    const test_cases = .{
        .{ .input = "for x in iter do end", .expected = "for x in iter do  end;" },
        .{ .input = "for k, v in pairs do end", .expected = "for k, v in pairs do  end;" },
        .{ .input = "for x in a, b, c do end", .expected = "for x in a, b, c do  end;" },
        .{ .input = "for i, val in items do x = val end", .expected = "for i, val in items do x = val; end;" },
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

test "table constructor - empty table" {
    const input = "{}";

    const allocator = std.testing.allocator;
    var lexer = try lex.Lexer.init(allocator, input);
    var parser = try Parser.init(allocator, &lexer);
    defer lexer.deinit();
    defer parser.deinit();
    const program = try parser.parseProgram();
    try assertNoErrors(&parser);
    try std.testing.expectEqual(@as(usize, 1), program.statements.len);

    switch (program.statements[0].*) {
        .Expression => |expr_stmt| {
            switch (expr_stmt.expr.*) {
                .TableConstructor => |table| {
                    try std.testing.expectEqual(@as(usize, 0), table.fields.items.len);
                },
                else => unreachable,
            }
        },
        else => unreachable,
    }
}

test "table constructor - array style fields" {
    const input = "{1, 2, 3}";

    const allocator = std.testing.allocator;
    var lexer = try lex.Lexer.init(allocator, input);
    var parser = try Parser.init(allocator, &lexer);
    defer lexer.deinit();
    defer parser.deinit();
    const program = try parser.parseProgram();
    try assertNoErrors(&parser);
    try std.testing.expectEqual(@as(usize, 1), program.statements.len);

    switch (program.statements[0].*) {
        .Expression => |expr_stmt| {
            switch (expr_stmt.expr.*) {
                .TableConstructor => |table| {
                    try std.testing.expectEqual(@as(usize, 3), table.fields.items.len);

                    // Check all fields are array style
                    for (table.fields.items) |field| {
                        switch (field.*) {
                            .ArrayStyle => {},
                            else => unreachable,
                        }
                    }
                },
                else => unreachable,
            }
        },
        else => unreachable,
    }
}

test "table constructor - record style fields" {
    const input = "{x = 10, y = 20}";

    const allocator = std.testing.allocator;
    var lexer = try lex.Lexer.init(allocator, input);
    var parser = try Parser.init(allocator, &lexer);
    defer lexer.deinit();
    defer parser.deinit();
    const program = try parser.parseProgram();
    try assertNoErrors(&parser);
    try std.testing.expectEqual(@as(usize, 1), program.statements.len);

    switch (program.statements[0].*) {
        .Expression => |expr_stmt| {
            switch (expr_stmt.expr.*) {
                .TableConstructor => |table| {
                    try std.testing.expectEqual(@as(usize, 2), table.fields.items.len);

                    // Check first field
                    switch (table.fields.items[0].*) {
                        .RecordStyle => |record| {
                            try std.testing.expectEqualStrings("x", record.name.value);
                        },
                        else => unreachable,
                    }

                    // Check second field
                    switch (table.fields.items[1].*) {
                        .RecordStyle => |record| {
                            try std.testing.expectEqualStrings("y", record.name.value);
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

test "table constructor - computed key fields" {
    const input = "{[key] = value}";

    const allocator = std.testing.allocator;
    var lexer = try lex.Lexer.init(allocator, input);
    var parser = try Parser.init(allocator, &lexer);
    defer lexer.deinit();
    defer parser.deinit();
    const program = try parser.parseProgram();
    try assertNoErrors(&parser);
    try std.testing.expectEqual(@as(usize, 1), program.statements.len);

    switch (program.statements[0].*) {
        .Expression => |expr_stmt| {
            switch (expr_stmt.expr.*) {
                .TableConstructor => |table| {
                    try std.testing.expectEqual(@as(usize, 1), table.fields.items.len);

                    // Check field is computed key
                    switch (table.fields.items[0].*) {
                        .ComputedKey => |computed| {
                            // Verify key is identifier
                            switch (computed.key.*) {
                                .Identifier => |ident| {
                                    try std.testing.expectEqualStrings("key", ident.value);
                                },
                                else => unreachable,
                            }
                            // Verify value is identifier
                            switch (computed.value.*) {
                                .Identifier => |ident| {
                                    try std.testing.expectEqualStrings("value", ident.value);
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

test "table constructor - mixed field types" {
    const input = "{1, x = 2, [y] = 3}";

    const allocator = std.testing.allocator;
    var lexer = try lex.Lexer.init(allocator, input);
    var parser = try Parser.init(allocator, &lexer);
    defer lexer.deinit();
    defer parser.deinit();
    const program = try parser.parseProgram();
    try assertNoErrors(&parser);
    try std.testing.expectEqual(@as(usize, 1), program.statements.len);

    switch (program.statements[0].*) {
        .Expression => |expr_stmt| {
            switch (expr_stmt.expr.*) {
                .TableConstructor => |table| {
                    try std.testing.expectEqual(@as(usize, 3), table.fields.items.len);

                    // Check first field is array style
                    switch (table.fields.items[0].*) {
                        .ArrayStyle => {},
                        else => unreachable,
                    }

                    // Check second field is record style
                    switch (table.fields.items[1].*) {
                        .RecordStyle => |record| {
                            try std.testing.expectEqualStrings("x", record.name.value);
                        },
                        else => unreachable,
                    }

                    // Check third field is computed key
                    switch (table.fields.items[2].*) {
                        .ComputedKey => {},
                        else => unreachable,
                    }
                },
                else => unreachable,
            }
        },
        else => unreachable,
    }
}

test "table constructor - with semicolon separators" {
    const input = "{1; 2; 3}";

    const allocator = std.testing.allocator;
    var lexer = try lex.Lexer.init(allocator, input);
    var parser = try Parser.init(allocator, &lexer);
    defer lexer.deinit();
    defer parser.deinit();
    const program = try parser.parseProgram();
    try assertNoErrors(&parser);
    try std.testing.expectEqual(@as(usize, 1), program.statements.len);

    switch (program.statements[0].*) {
        .Expression => |expr_stmt| {
            switch (expr_stmt.expr.*) {
                .TableConstructor => |table| {
                    try std.testing.expectEqual(@as(usize, 3), table.fields.items.len);
                },
                else => unreachable,
            }
        },
        else => unreachable,
    }
}

test "table constructor - with trailing separator" {
    const test_cases = .{
        "{1, 2, 3,}",
        "{1, 2, 3;}",
        "{x = 1, y = 2,}",
        "{x = 1; y = 2;}",
    };

    const allocator = std.testing.allocator;
    inline for (test_cases) |input| {
        var lexer = try lex.Lexer.init(allocator, input);
        var parser = try Parser.init(allocator, &lexer);
        defer lexer.deinit();
        defer parser.deinit();
        const program = try parser.parseProgram();
        try assertNoErrors(&parser);
        try std.testing.expectEqual(@as(usize, 1), program.statements.len);

        switch (program.statements[0].*) {
            .Expression => |expr_stmt| {
                switch (expr_stmt.expr.*) {
                    .TableConstructor => |table| {
                        // Should have parsed fields correctly despite trailing separator
                        try std.testing.expect(table.fields.items.len > 0);
                    },
                    else => unreachable,
                }
            },
            else => unreachable,
        }
    }
}

test "table constructor - nested tables" {
    const input = "{{1, 2}, {3, 4}}";

    const allocator = std.testing.allocator;
    var lexer = try lex.Lexer.init(allocator, input);
    var parser = try Parser.init(allocator, &lexer);
    defer lexer.deinit();
    defer parser.deinit();
    const program = try parser.parseProgram();
    try assertNoErrors(&parser);
    try std.testing.expectEqual(@as(usize, 1), program.statements.len);

    switch (program.statements[0].*) {
        .Expression => |expr_stmt| {
            switch (expr_stmt.expr.*) {
                .TableConstructor => |table| {
                    try std.testing.expectEqual(@as(usize, 2), table.fields.items.len);

                    // Check both fields are array style containing tables
                    for (table.fields.items) |field| {
                        switch (field.*) {
                            .ArrayStyle => |array_expr| {
                                switch (array_expr.*) {
                                    .TableConstructor => |inner_table| {
                                        try std.testing.expectEqual(@as(usize, 2), inner_table.fields.items.len);
                                    },
                                    else => unreachable,
                                }
                            },
                            else => unreachable,
                        }
                    }
                },
                else => unreachable,
            }
        },
        else => unreachable,
    }
}

test "table constructor - string representation" {
    const test_cases = .{
        .{ .input = "{}", .expected = "{};" },
        .{ .input = "{1, 2, 3}", .expected = "{1, 2, 3};" },
        .{ .input = "{x = 10, y = 20}", .expected = "{x = 10, y = 20};" },
        .{ .input = "{[key] = val}", .expected = "{[key] = val};" },
        .{ .input = "{[1+2] = 3}", .expected = "{[(1 + 2)] = 3};" },
        .{ .input = "{1, x = 2, [y] = 3}", .expected = "{1, x = 2, [y] = 3};" },
        .{ .input = "{{1, 2}, {3, 4}}", .expected = "{{1, 2}, {3, 4}};" },
        // With trailing separators
        .{ .input = "{1, 2, 3,}", .expected = "{1, 2, 3};" },
        .{ .input = "{1, 2, 3;}", .expected = "{1, 2, 3};" },
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

test "table constructor - in assignment" {
    const input = "local t = {1, 2, 3}";

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
            try std.testing.expect(assign.is_local);
            try std.testing.expectEqualStrings("t", assign.names.names.items[0].value);

            switch (assign.expr.*) {
                .TableConstructor => |table| {
                    try std.testing.expectEqual(@as(usize, 3), table.fields.items.len);
                },
                else => unreachable,
            }
        },
        else => unreachable,
    }
}

test "index expression - basic" {
    const input = "t[1]";

    const allocator = std.testing.allocator;
    var lexer = try lex.Lexer.init(allocator, input);
    var parser = try Parser.init(allocator, &lexer);
    defer lexer.deinit();
    defer parser.deinit();
    const program = try parser.parseProgram();
    try assertNoErrors(&parser);

    var writer = std.Io.Writer.Allocating.init(allocator);
    defer writer.deinit();
    try program.write(&writer.writer);
    try std.testing.expectEqualStrings("t[1];", writer.written());
}

test "index expression - with expression" {
    const input = "arr[i + 1]";

    const allocator = std.testing.allocator;
    var lexer = try lex.Lexer.init(allocator, input);
    var parser = try Parser.init(allocator, &lexer);
    defer lexer.deinit();
    defer parser.deinit();
    const program = try parser.parseProgram();
    try assertNoErrors(&parser);

    var writer = std.Io.Writer.Allocating.init(allocator);
    defer writer.deinit();
    try program.write(&writer.writer);
    try std.testing.expectEqualStrings("arr[(i + 1)];", writer.written());
}

test "member expression - basic" {
    const input = "t.x";

    const allocator = std.testing.allocator;
    var lexer = try lex.Lexer.init(allocator, input);
    var parser = try Parser.init(allocator, &lexer);
    defer lexer.deinit();
    defer parser.deinit();
    const program = try parser.parseProgram();
    try assertNoErrors(&parser);

    var writer = std.Io.Writer.Allocating.init(allocator);
    defer writer.deinit();
    try program.write(&writer.writer);
    try std.testing.expectEqualStrings("t.x;", writer.written());
}

test "member expression - chained" {
    const input = "obj.foo.bar";

    const allocator = std.testing.allocator;
    var lexer = try lex.Lexer.init(allocator, input);
    var parser = try Parser.init(allocator, &lexer);
    defer lexer.deinit();
    defer parser.deinit();
    const program = try parser.parseProgram();
    try assertNoErrors(&parser);

    var writer = std.Io.Writer.Allocating.init(allocator);
    defer writer.deinit();
    try program.write(&writer.writer);
    try std.testing.expectEqualStrings("obj.foo.bar;", writer.written());
}

test "index and member expression - chained" {
    const input = "t.foo[0].bar";

    const allocator = std.testing.allocator;
    var lexer = try lex.Lexer.init(allocator, input);
    var parser = try Parser.init(allocator, &lexer);
    defer lexer.deinit();
    defer parser.deinit();
    const program = try parser.parseProgram();
    try assertNoErrors(&parser);

    var writer = std.Io.Writer.Allocating.init(allocator);
    defer writer.deinit();
    try program.write(&writer.writer);
    try std.testing.expectEqualStrings("t.foo[0].bar;", writer.written());
}

test "index expression - nested" {
    const input = "matrix[x][y]";

    const allocator = std.testing.allocator;
    var lexer = try lex.Lexer.init(allocator, input);
    var parser = try Parser.init(allocator, &lexer);
    defer lexer.deinit();
    defer parser.deinit();
    const program = try parser.parseProgram();
    try assertNoErrors(&parser);

    var writer = std.Io.Writer.Allocating.init(allocator);
    defer writer.deinit();
    try program.write(&writer.writer);
    try std.testing.expectEqualStrings("matrix[x][y];", writer.written());
}

test "index expression - precedence with multiplication" {
    const input = "t[0] * 2";

    const allocator = std.testing.allocator;
    var lexer = try lex.Lexer.init(allocator, input);
    var parser = try Parser.init(allocator, &lexer);
    defer lexer.deinit();
    defer parser.deinit();
    const program = try parser.parseProgram();
    try assertNoErrors(&parser);

    var writer = std.Io.Writer.Allocating.init(allocator);
    defer writer.deinit();
    try program.write(&writer.writer);
    try std.testing.expectEqualStrings("(t[0] * 2);", writer.written());
}

test "member expression - precedence with addition" {
    const input = "obj.x + 5";

    const allocator = std.testing.allocator;
    var lexer = try lex.Lexer.init(allocator, input);
    var parser = try Parser.init(allocator, &lexer);
    defer lexer.deinit();
    defer parser.deinit();
    const program = try parser.parseProgram();
    try assertNoErrors(&parser);

    var writer = std.Io.Writer.Allocating.init(allocator);
    defer writer.deinit();
    try program.write(&writer.writer);
    try std.testing.expectEqualStrings("(obj.x + 5);", writer.written());
}

test "index and member - complex chain" {
    const input = "a[1].b[2].c";

    const allocator = std.testing.allocator;
    var lexer = try lex.Lexer.init(allocator, input);
    var parser = try Parser.init(allocator, &lexer);
    defer lexer.deinit();
    defer parser.deinit();
    const program = try parser.parseProgram();
    try assertNoErrors(&parser);

    var writer = std.Io.Writer.Allocating.init(allocator);
    defer writer.deinit();
    try program.write(&writer.writer);
    try std.testing.expectEqualStrings("a[1].b[2].c;", writer.written());
}

test "function call - no arguments" {
    const input = "foo()";

    const allocator = std.testing.allocator;
    var lexer = try lex.Lexer.init(allocator, input);
    var parser = try Parser.init(allocator, &lexer);
    defer lexer.deinit();
    defer parser.deinit();
    const program = try parser.parseProgram();
    try assertNoErrors(&parser);

    var writer = std.Io.Writer.Allocating.init(allocator);
    defer writer.deinit();
    try program.write(&writer.writer);
    try std.testing.expectEqualStrings("foo();", writer.written());
}

test "function call - single argument" {
    const input = "print(42)";

    const allocator = std.testing.allocator;
    var lexer = try lex.Lexer.init(allocator, input);
    var parser = try Parser.init(allocator, &lexer);
    defer lexer.deinit();
    defer parser.deinit();
    const program = try parser.parseProgram();
    try assertNoErrors(&parser);

    var writer = std.Io.Writer.Allocating.init(allocator);
    defer writer.deinit();
    try program.write(&writer.writer);
    try std.testing.expectEqualStrings("print(42);", writer.written());
}

test "function call - multiple arguments" {
    const input = "add(1, 2, 3)";

    const allocator = std.testing.allocator;
    var lexer = try lex.Lexer.init(allocator, input);
    var parser = try Parser.init(allocator, &lexer);
    defer lexer.deinit();
    defer parser.deinit();
    const program = try parser.parseProgram();
    try assertNoErrors(&parser);

    var writer = std.Io.Writer.Allocating.init(allocator);
    defer writer.deinit();
    try program.write(&writer.writer);
    try std.testing.expectEqualStrings("add(1, 2, 3);", writer.written());
}

test "function call - string literal argument" {
    const input = "print \"hello\"";

    const allocator = std.testing.allocator;
    var lexer = try lex.Lexer.init(allocator, input);
    var parser = try Parser.init(allocator, &lexer);
    defer lexer.deinit();
    defer parser.deinit();
    const program = try parser.parseProgram();
    try assertNoErrors(&parser);

    var writer = std.Io.Writer.Allocating.init(allocator);
    defer writer.deinit();
    try program.write(&writer.writer);
    try std.testing.expectEqualStrings("print\"hello\";", writer.written());
}

test "function call - table constructor argument" {
    const input = "config {x = 1, y = 2}";

    const allocator = std.testing.allocator;
    var lexer = try lex.Lexer.init(allocator, input);
    var parser = try Parser.init(allocator, &lexer);
    defer lexer.deinit();
    defer parser.deinit();
    const program = try parser.parseProgram();
    try assertNoErrors(&parser);

    var writer = std.Io.Writer.Allocating.init(allocator);
    defer writer.deinit();
    try program.write(&writer.writer);
    try std.testing.expectEqualStrings("config{x = 1, y = 2};", writer.written());
}

test "method call - no arguments" {
    const input = "obj:method()";

    const allocator = std.testing.allocator;
    var lexer = try lex.Lexer.init(allocator, input);
    var parser = try Parser.init(allocator, &lexer);
    defer lexer.deinit();
    defer parser.deinit();
    const program = try parser.parseProgram();
    try assertNoErrors(&parser);

    var writer = std.Io.Writer.Allocating.init(allocator);
    defer writer.deinit();
    try program.write(&writer.writer);
    try std.testing.expectEqualStrings("obj:method();", writer.written());
}

test "method call - with arguments" {
    const input = "obj:fn(1, 2)";

    const allocator = std.testing.allocator;
    var lexer = try lex.Lexer.init(allocator, input);
    var parser = try Parser.init(allocator, &lexer);
    defer lexer.deinit();
    defer parser.deinit();
    const program = try parser.parseProgram();
    try assertNoErrors(&parser);

    var writer = std.Io.Writer.Allocating.init(allocator);
    defer writer.deinit();
    try program.write(&writer.writer);
    try std.testing.expectEqualStrings("obj:fn(1, 2);", writer.written());
}

test "method call - string argument" {
    const input = "obj:log\"error\"";

    const allocator = std.testing.allocator;
    var lexer = try lex.Lexer.init(allocator, input);
    var parser = try Parser.init(allocator, &lexer);
    defer lexer.deinit();
    defer parser.deinit();
    const program = try parser.parseProgram();
    try assertNoErrors(&parser);

    var writer = std.Io.Writer.Allocating.init(allocator);
    defer writer.deinit();
    try program.write(&writer.writer);
    try std.testing.expectEqualStrings("obj:log\"error\";", writer.written());
}

test "function call - chained calls" {
    const input = "foo()()";

    const allocator = std.testing.allocator;
    var lexer = try lex.Lexer.init(allocator, input);
    var parser = try Parser.init(allocator, &lexer);
    defer lexer.deinit();
    defer parser.deinit();
    const program = try parser.parseProgram();
    try assertNoErrors(&parser);

    var writer = std.Io.Writer.Allocating.init(allocator);
    defer writer.deinit();
    try program.write(&writer.writer);
    try std.testing.expectEqualStrings("foo()();", writer.written());
}

test "function call - complex expression" {
    const input = "add(x + 1, y * 2)";

    const allocator = std.testing.allocator;
    var lexer = try lex.Lexer.init(allocator, input);
    var parser = try Parser.init(allocator, &lexer);
    defer lexer.deinit();
    defer parser.deinit();
    const program = try parser.parseProgram();
    try assertNoErrors(&parser);

    var writer = std.Io.Writer.Allocating.init(allocator);
    defer writer.deinit();
    try program.write(&writer.writer);
    try std.testing.expectEqualStrings("add((x + 1), (y * 2));", writer.written());
}

test "function call - mixed with member access" {
    const input = "obj.method(42)";

    const allocator = std.testing.allocator;
    var lexer = try lex.Lexer.init(allocator, input);
    var parser = try Parser.init(allocator, &lexer);
    defer lexer.deinit();
    defer parser.deinit();
    const program = try parser.parseProgram();
    try assertNoErrors(&parser);

    var writer = std.Io.Writer.Allocating.init(allocator);
    defer writer.deinit();
    try program.write(&writer.writer);
    try std.testing.expectEqualStrings("obj.method(42);", writer.written());
}

test "function call - mixed with index access" {
    const input = "funcs[1](x)";

    const allocator = std.testing.allocator;
    var lexer = try lex.Lexer.init(allocator, input);
    var parser = try Parser.init(allocator, &lexer);
    defer lexer.deinit();
    defer parser.deinit();
    const program = try parser.parseProgram();
    try assertNoErrors(&parser);

    var writer = std.Io.Writer.Allocating.init(allocator);
    defer writer.deinit();
    try program.write(&writer.writer);
    try std.testing.expectEqualStrings("funcs[1](x);", writer.written());
}

test "builtin expression - standalone" {
    const input = "$puts";

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
                .CBuiltin => |builtin| {
                    try std.testing.expectEqualStrings("$puts", builtin.name);
                },
                else => {
                    std.debug.print("Expected CBuiltin expression, got {s}\n", .{@tagName(e.expr.*)});
                    return error.WrongExpressionType;
                },
            }
        },
        else => return error.WrongStatementType,
    }
}

test "builtin expression - as function call" {
    const input = "$puts(\"hello\")";

    const allocator = std.testing.allocator;
    var lexer = try lex.Lexer.init(allocator, input);
    var parser = try Parser.init(allocator, &lexer);
    defer lexer.deinit();
    defer parser.deinit();
    const program = try parser.parseProgram();
    try assertNoErrors(&parser);

    var writer = std.Io.Writer.Allocating.init(allocator);
    defer writer.deinit();
    try program.write(&writer.writer);
    try std.testing.expectEqualStrings("$puts(\"hello\");", writer.written());
}

test "builtin expression - in assignment" {
    const input = "local x = $malloc(100)";

    const allocator = std.testing.allocator;
    var lexer = try lex.Lexer.init(allocator, input);
    var parser = try Parser.init(allocator, &lexer);
    defer lexer.deinit();
    defer parser.deinit();
    const program = try parser.parseProgram();
    try assertNoErrors(&parser);

    var writer = std.Io.Writer.Allocating.init(allocator);
    defer writer.deinit();
    try program.write(&writer.writer);
    try std.testing.expectEqualStrings("local x = $malloc(100);", writer.written());
}

test "builtin expression - multiple builtins" {
    const input =
        \\$printf("x = %d\n", 42);
        \\local ptr = $malloc(256);
        \\$free(ptr)
    ;

    const allocator = std.testing.allocator;
    var lexer = try lex.Lexer.init(allocator, input);
    var parser = try Parser.init(allocator, &lexer);
    defer lexer.deinit();
    defer parser.deinit();
    const program = try parser.parseProgram();
    try assertNoErrors(&parser);

    try std.testing.expectEqual(@as(usize, 3), program.statements.len);

    var writer = std.Io.Writer.Allocating.init(allocator);
    defer writer.deinit();
    try program.write(&writer.writer);
    const expected =
        \\$printf("x = %d\n", 42);
        \\local ptr = $malloc(256);
        \\$free(ptr);
        \\
    ;
    try std.testing.expectEqualStrings(expected, writer.written());
}
