const std = @import("std");
const token = @import("token.zig");
const lex = @import("lexer.zig");
const ast = @import("ast.zig");
const TokenType = token.TokenType;
const AllocError = std.mem.Allocator.Error;
const LexError = lex.Lexer.LexError;

/// Parser struct that handles recursive descent pratt parsing of moglang
pub const Parser = struct {
    lexer: *lex.Lexer,
    current_token: *token.Token,
    peek_token: *token.Token,
    parser_error: ?*const []u8,
    alloc: std.heap.ArenaAllocator,

    // Parser error
    pub const ParserError = error{fail};
    const SuperError = (LexError || ParserError || AllocError || std.fmt.ParseIntError);
    const PrefixParseFn = *const fn (*Parser) SuperError!*ast.Expression;
    const InfixParseFn = *const fn (*Parser, *ast.Expression) SuperError!*ast.Expression;

    // Precedence defines precedence for Pratt parsing infix operators
    // Ordered from lowest to highest precedence (matching Lua 5.4 spec)
    const Precedence = enum(u8) {
        lowest,
        logical_or, // or
        logical_and, // and
        comparison, // <, >, <=, >=, ~=, ==
        bitwise_or, // |
        bitwise_xor, // ~
        bitwise_and, // &
        bitwise_shift, // <<, >>
        concat, // .. (right-associative)
        sum, // +, -
        product, // *, /, //, %
        unary, // not, #, -, ~ (unary)
        exponent, // ^ (right-associative)
        call, // function calls

        const TokenPrecedenceMap = std.StaticStringMap(Precedence).initComptime(.{
            .{ "t_or", Precedence.logical_or },
            .{ "t_and", Precedence.logical_and },
            .{ "eq", Precedence.comparison },
            .{ "neq", Precedence.comparison },
            .{ "lt", Precedence.comparison },
            .{ "gt", Precedence.comparison },
            .{ "lte", Precedence.comparison },
            .{ "gte", Precedence.comparison },
            .{ "pipe", Precedence.bitwise_or },
            .{ "tilde", Precedence.bitwise_xor },
            .{ "ampersand", Precedence.bitwise_and },
            .{ "lshift", Precedence.bitwise_shift },
            .{ "rshift", Precedence.bitwise_shift },
            .{ "dotdot", Precedence.concat },
            .{ "plus", Precedence.sum },
            .{ "minus", Precedence.sum },
            .{ "asterisk", Precedence.product },
            .{ "slash", Precedence.product },
            .{ "doubleslash", Precedence.product },
            .{ "percent", Precedence.product },
            .{ "caret", Precedence.exponent },
            .{ "lparen", Precedence.call },
        });

        fn ofToken(t: TokenType) Precedence {
            return TokenPrecedenceMap.get(@tagName(t)) orelse Precedence.lowest;
        }
    };

    const InfixMap = std.StaticStringMap(InfixParseFn).initComptime(.{
        // Arithmetic operators
        .{ "plus", &parseInfixExpression },
        .{ "minus", &parseInfixExpression },
        .{ "asterisk", &parseInfixExpression },
        .{ "slash", &parseInfixExpression },
        .{ "doubleslash", &parseInfixExpression },
        .{ "percent", &parseInfixExpression },
        .{ "caret", &parseInfixExpression },
        // Comparison operators
        .{ "eq", &parseInfixExpression },
        .{ "neq", &parseInfixExpression },
        .{ "lt", &parseInfixExpression },
        .{ "gt", &parseInfixExpression },
        .{ "lte", &parseInfixExpression },
        .{ "gte", &parseInfixExpression },
        // Logical operators
        .{ "t_and", &parseInfixExpression },
        .{ "t_or", &parseInfixExpression },
        // Bitwise operators
        .{ "ampersand", &parseInfixExpression },
        .{ "pipe", &parseInfixExpression },
        .{ "tilde", &parseInfixExpression },
        .{ "lshift", &parseInfixExpression },
        .{ "rshift", &parseInfixExpression },
        // String concatenation
        .{ "dotdot", &parseInfixExpression },
    });
    const PrefixMap = std.StaticStringMap(PrefixParseFn).initComptime(.{
        // Literals
        .{ "ident", &parseIdentifierExpression },
        .{ "int", &parseIntegerExpression },
        .{ "float", &parseFloatExpression },
        .{ "string", &parseStringExpression },
        .{ "true", &parseBooleanExpression },
        .{ "false", &parseBooleanExpression },
        .{ "nil", &parseNilExpression },
        // Unary operators
        .{ "not", &parsePrefixExpression },
        .{ "minus", &parsePrefixExpression },
        .{ "hash", &parsePrefixExpression },
        .{ "tilde", &parsePrefixExpression },
        // Control flow
        .{ "t_if", &parseConditionalExpression },
        // Grouping
        .{ "lparen", &parseParenthesizedExpression },
    });

    /// Initialize the Parser
    /// This creates an arena allocator to create AST nodes
    pub fn init(allocator: std.mem.Allocator, lexer: *lex.Lexer) !Parser {
        const alloc = std.heap.ArenaAllocator.init(allocator);

        // Read first two tokens to initialize stat
        const first = (try lexer.nextToken());
        const second = (try lexer.nextToken());

        const parser = Parser{
            .lexer = lexer,
            .current_token = first,
            .peek_token = second,
            .parser_error = null,
            .alloc = alloc,
        };
        return parser;
    }

    /// Deinits the parser
    /// Frees all AST nodes allocated by the parser
    pub fn deinit(self: *Parser) void {
        self.alloc.deinit();
    }

    /// Read another token
    fn nextToken(self: *Parser) !void {
        self.current_token = self.peek_token;
        const new_token = try self.alloc.allocator().create(token.Token);
        new_token.* = (try self.lexer.nextToken()).*;
        self.peek_token = new_token;
    }

    fn setPeekError(self: *Parser, t: TokenType) !void {
        const msg = try std.fmt.allocPrint(
            self.alloc.allocator(),
            "expected next token to be {s}, got {s} instead at position {d}..{d}",
            .{
                @tagName(t),
                @tagName(self.peek_token.type),
                self.peek_token.start_pos,
                self.peek_token.end_pos,
            },
        );
        self.parser_error = &msg;
    }

    fn currentTokenIs(self: *Parser, t: TokenType) bool {
        return self.current_token.type == t;
    }

    fn peekTokenIs(self: *Parser, t: TokenType) bool {
        return self.peek_token.type == t;
    }

    // TODO: change this to just try with error, not returning boolean
    fn expectAndPeek(self: *Parser, t: TokenType) bool {
        if (self.peekTokenIs(t)) {
            self.nextToken() catch return false;
            return true;
        }
        self.setPeekError(t) catch {};
        return false;
    }

    fn currentPrecedence(self: *Parser) Precedence {
        return Precedence.ofToken(self.current_token.*.type);
    }
    fn peekPrecedence(self: *Parser) Precedence {
        return Precedence.ofToken(self.peek_token.*.type);
    }

    // parseBlock parses until it reaches an `else` or `end`
    fn parseBlock(self: *Parser) !*ast.Block {
        var block = try self.alloc.allocator().create(ast.Block);
        var statements = try std.ArrayList(*ast.Statement).initCapacity(self.alloc.allocator(), 8);
        while (!self.currentTokenIs(.t_else) and !self.currentTokenIs(.end)) {
            const stmt = try self.parseStatement();
            try statements.append(self.alloc.allocator(), stmt);
            try self.nextToken();
        }
        block.statements = statements.items;

        return block;
    }

    /// Parses a mog program, which is a collection of statements
    /// Recursively descends to return a Program AST node
    pub fn parseProgram(self: *Parser) !*ast.Program {
        var program = try self.alloc.allocator().create(ast.Program);
        var statements = try std.ArrayList(*ast.Statement).initCapacity(self.alloc.allocator(), 64);
        while (self.current_token.type != .eof) {
            const stmt = try self.parseStatement();
            try statements.append(self.alloc.allocator(), stmt);
            try self.nextToken();
        }
        program.statements = statements.items;

        return program;
    }

    fn parseStatement(self: *Parser) !*ast.Statement {
        const stmt = try self.alloc.allocator().create(ast.Statement);
        switch (self.current_token.type) {
            .local => {
                stmt.* = .{ .Let = try self.parseLetStatement() };
            },
            .t_return => {
                stmt.* = .{ .Return = try self.parseReturnStatement() };
            },
            else => {
                stmt.* = .{ .Expression = try self.parseExpressionStatement() };
            },
        }
        return stmt;
    }

    fn parseReturnStatement(self: *Parser) !*ast.ReturnStatement {
        const rs = try self.alloc.allocator().create(ast.ReturnStatement);
        rs.token = self.current_token.*;
        if (self.peekTokenIs(TokenType.semicolon)) {
            rs.expr = null;
            try self.nextToken();
            return rs;
        }
        try self.nextToken();
        rs.expr = try self.parseExpression(Precedence.lowest);
        if (self.peekTokenIs(TokenType.semicolon)) {
            try self.nextToken();
        }
        return rs;
    }

    fn parseLetStatement(self: *Parser) !*ast.LetStatement {
        const ls = try self.alloc.allocator().create(ast.LetStatement);
        ls.token = self.current_token.*;
        if (!self.expectAndPeek(.ident)) {
            return ParserError.fail;
        }
        ls.name = try self.parseIdentifier();
        if (!self.expectAndPeek(.assign)) {
            return ParserError.fail;
        }
        try self.nextToken();

        const expr = try self.parseExpression(Precedence.lowest);
        ls.expr = expr;

        if (self.peekTokenIs(TokenType.semicolon)) {
            try self.nextToken();
        }

        return ls;
    }

    fn parseExpressionStatement(self: *Parser) !*ast.ExpressionStatement {
        const es = try self.alloc.allocator().create(ast.ExpressionStatement);
        es.expr = try self.parseExpression(Precedence.lowest);
        if (self.peekTokenIs(TokenType.semicolon)) {
            try self.nextToken();
        }
        return es;
    }

    fn parsePrefixExpression(self: *Parser) SuperError!*ast.Expression {
        const expr = try self.alloc.allocator().create(ast.Expression);
        const prefix_expr = try self.alloc.allocator().create(ast.PrefixExpression);

        prefix_expr.token = self.current_token.*;
        prefix_expr.operator = prefix_expr.token.literal;
        try self.nextToken();
        prefix_expr.expression = try self.parseExpression(Precedence.lowest);

        expr.* = .{ .Prefix = prefix_expr };
        return expr;
    }

    fn parseInfixExpression(self: *Parser, left: *ast.Expression) SuperError!*ast.Expression {
        const expr = try self.alloc.allocator().create(ast.Expression);
        const infix_expr = try self.alloc.allocator().create(ast.InfixExpression);

        infix_expr.token = self.current_token.*;
        infix_expr.operator = infix_expr.token.literal;
        infix_expr.left = left;
        const prec = (self.currentPrecedence());
        try self.nextToken();

        // Right-associative operators: ^, ..
        // For right-associativity, we parse the right side with one less precedence
        const is_right_assoc = std.mem.eql(u8, infix_expr.operator, "^") or
            std.mem.eql(u8, infix_expr.operator, "..");
        const right_prec = if (is_right_assoc)
            @as(Precedence, @enumFromInt(@intFromEnum(prec) - 1))
        else
            prec;

        infix_expr.right = try self.parseExpression(right_prec);

        expr.* = .{ .Infix = infix_expr };
        return expr;
    }

    fn parseConditionalExpression(self: *Parser) SuperError!*ast.Expression {
        const expr = try self.alloc.allocator().create(ast.Expression);
        const cond_expr = try self.alloc.allocator().create(ast.ConditionalExpression);
        cond_expr.token = self.current_token.*;
        try self.nextToken();

        // on condition
        cond_expr.condition = try self.parseExpression(.lowest);
        if (!self.expectAndPeek(.then)) {
            return ParserError.fail;
        }
        try self.nextToken();
        cond_expr.then_block = try self.parseBlock();
        if (self.currentTokenIs(.t_else)) {
            try self.nextToken();
            cond_expr.else_block = try self.parseBlock();
            if (self.currentTokenIs(.t_else)) {
                return ParserError.fail;
            }
        } else {
            cond_expr.else_block = null;
        }
        expr.* = .{ .Conditional = cond_expr };
        return expr;
    }

    fn parseParenthesizedExpression(self: *Parser) SuperError!*ast.Expression {
        // drop references to the parens
        try self.nextToken();
        const expr = try self.parseExpression(.lowest);
        if (!self.expectAndPeek(.rparen)) {
            return ParserError.fail;
        }
        return expr;
    }

    fn parseExpression(self: *Parser, precedence: Precedence) SuperError!*ast.Expression {
        var left = try self.alloc.allocator().create(ast.Expression);
        const prefix = PrefixMap.get(@tagName(self.current_token.type));
        if (prefix == null) {
            const err = try std.fmt.allocPrint(self.alloc.allocator(), "no prefix function found for token {s}", .{@tagName(self.current_token.type)});
            self.parser_error = &err;
            return ParserError.fail;
        }
        left = try prefix.?(self);

        while (!self.peekTokenIs(TokenType.semicolon) and @intFromEnum(precedence) < @intFromEnum(self.peekPrecedence())) {
            const infix = InfixMap.get(@tagName(self.peek_token.type)) orelse return left;
            try self.nextToken();
            left = try infix(self, left);
        }
        return left;
    }

    fn parseIdentifier(self: *Parser) !*ast.Identifier {
        const ident = try self.alloc.allocator().create(ast.Identifier);
        ident.token = self.current_token.*;
        ident.value = self.current_token.*.literal;

        return ident;
    }

    fn parseIdentifierExpression(self: *Parser) !*ast.Expression {
        const expr = try self.alloc.allocator().create(ast.Expression);
        const ident = try self.parseIdentifier();
        expr.* = .{ .Identifier = ident };
        return expr;
    }

    fn parseIntegerExpression(self: *Parser) !*ast.Expression {
        const expr = try self.alloc.allocator().create(ast.Expression);
        const int = try self.alloc.allocator().create(ast.NumberLiteral);
        int.value = .{ .Integer = try std.fmt.parseInt(i32, self.current_token.literal, 10) };
        expr.* = .{ .Number = int };
        return expr;
    }

    fn parseFloatExpression(self: *Parser) !*ast.Expression {
        const expr = try self.alloc.allocator().create(ast.Expression);
        const float = try self.alloc.allocator().create(ast.NumberLiteral);
        float.value = .{ .Float = try std.fmt.parseFloat(f32, self.current_token.literal) };
        expr.* = .{ .Number = float };
        return expr;
    }

    fn parseStringExpression(self: *Parser) !*ast.Expression {
        const expr = try self.alloc.allocator().create(ast.Expression);
        const str = try self.alloc.allocator().create(ast.StringLiteral);
        str.token = self.current_token.*;

        // cut out the string literal quotations to keep the value only
        const len = self.current_token.end_pos - self.current_token.start_pos;
        str.value = self.current_token.literal[1..len];
        expr.* = .{ .String = str };
        return expr;
    }

    fn parseNilExpression(self: *Parser) !*ast.Expression {
        const expr = try self.alloc.allocator().create(ast.Expression);
        const nil = try self.alloc.allocator().create(ast.Nil);
        nil.token = self.current_token.*;
        expr.* = .{ .Nil = nil };
        return expr;
    }

    /// Parse a boolean expression - errors if value is not true or false
    fn parseBooleanExpression(self: *Parser) !*ast.Expression {
        std.log.info("parsing boolean expression", .{});
        const expr = try self.alloc.allocator().create(ast.Expression);
        const boolean = try self.alloc.allocator().create(ast.BooleanLiteral);
        boolean.token = self.current_token.*;
        if (std.mem.eql(u8, self.current_token.literal, "true")) {
            boolean.value = true;
        } else if (std.mem.eql(u8, self.current_token.literal, "false")) {
            boolean.value = false;
        } else {
            return ParserError.fail;
        }
        expr.* = .{ .Boolean = boolean };
        return expr;
    }
};

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
    try testLetStatement(program.statements[0], "x");
    try testLetStatement(program.statements[1], "foo");
}

test "statement errors" {
    const test_cases = .{
        .{
            .input = "local = 10;",
            .expected_error = "expected next token to be ident, got assign instead",
        },
        .{
            .input = "local x 10;",
            .expected_error = "expected next token to be assign, got int instead",
        },
        .{
            .input = "local x 10;",
            .expected_error = "expected next token to be assign, got int instead",
        },
    };

    const allocator = std.testing.allocator;

    inline for (test_cases) |test_case| {
        var lexer = try lex.Lexer.init(allocator, test_case.input);
        var parser = try Parser.init(allocator, &lexer);
        defer lexer.deinit();
        defer parser.deinit();
        _ = parser.parseProgram() catch {
            const error_msg = if (parser.parser_error != null) parser.parser_error.?.* else "";
            try std.testing.expectStringStartsWith(error_msg, test_case.expected_error);
        };
    }
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

fn testLetStatement(s: *ast.Statement, name: []const u8) !void {
    try std.testing.expectEqualStrings("local", s.tokenLiteral());

    switch (s.*) {
        .Let => |ls| {
            try std.testing.expectEqualStrings(name, ls.name.value);
            try std.testing.expectEqualStrings(name, ls.name.tokenLiteral());
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
