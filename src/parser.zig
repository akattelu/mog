const std = @import("std");
const token = @import("token.zig");
const lex = @import("lexer.zig");
const ast = @import("ast.zig");

pub const Parser = struct {
    lexer: *lex.Lexer,
    current_token: *token.Token,
    peek_token: *token.Token,
    parser_error: ?*const []u8,
    alloc: std.heap.ArenaAllocator,

    pub fn init(allocator: std.mem.Allocator, lexer: *lex.Lexer) !Parser {
        const first = try lexer.nextToken();
        const second = try lexer.nextToken();
        const alloc = std.heap.ArenaAllocator.init(allocator);
        return Parser{
            .lexer = lexer,
            .current_token = first,
            .peek_token = second,
            .parser_error = null,
            .alloc = alloc,
        };
    }

    pub fn deinit(self: *Parser) void {
        self.alloc.deinit();
    }

    pub fn nextToken(self: *Parser) !void {
        self.current_token = self.peek_token;
        self.peek_token = try self.lexer.nextToken();
    }

    fn setPeekError(self: *Parser, t: token.TokenType) !void {
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

    fn currentTokenIs(self: *Parser, t: token.TokenType) bool {
        return self.current_token.type == t;
    }

    fn peekTokenIs(self: *Parser, t: token.TokenType) bool {
        return self.peek_token.type == t;
    }

    fn expectAndPeek(self: *Parser, t: token.TokenType) bool {
        if (self.peekTokenIs(t)) {
            self.nextToken() catch return false;
            return true;
        }
        self.setPeekError(t) catch {};
        return false;
    }

    pub fn parseProgram(self: *Parser) !*ast.Program {
        var program = try self.alloc.allocator().create(ast.Program);
        var statements = std.ArrayList(*ast.Statement).init(self.alloc.allocator());
        while (self.current_token.type != .eof) {
            const stmt = try self.parseStatement();
            try statements.append(stmt);
            try self.nextToken();
        }
        program.statements = statements.items;

        return program;
    }

    fn parseStatement(self: *Parser) !*ast.Statement {
        const stmt = try self.alloc.allocator().create(ast.Statement);
        switch (self.current_token.type) {
            .let => {
                stmt.* = .{ .Let = try self.parseLetStatement() };
            },
            .t_return => {
                stmt.* = .{ .Return = try self.parseReturnStatement() };
            },
            else => {
                unreachable;
            },
        }
        return stmt;
    }

    const ParserError = error{fail};

    fn parseReturnStatement(self: *Parser) !*ast.ReturnStatement {
        const rs = try self.alloc.allocator().create(ast.ReturnStatement);
        rs.token = self.current_token.*;
        rs.expr = null;
        try self.nextToken();
        // NOTE: skip until semicolon for now
        while (self.current_token.type != .semicolon) {
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
        ls.expr = null;

        // NOTE: skip until semicolon for now
        while (self.current_token.type != .semicolon) {
            try self.nextToken();
        }
        return ls;
    }

    fn parseIdentifier(self: *Parser) !*ast.Identifier {
        const ident = try self.alloc.allocator().create(ast.Identifier);
        ident.token = self.current_token.*;
        ident.value = self.current_token.*.literal;

        return ident;
    }
};

test "create parser over string" {
    const input = "let x = 3";
    const allocator = std.testing.allocator;
    var lexer = try lex.Lexer.init(allocator, input);
    defer lexer.deinit();

    var parser = try Parser.init(allocator, lexer);
    defer parser.deinit();
    try std.testing.expectEqualStrings(parser.current_token.literal, "let");
    try std.testing.expectEqualStrings(parser.peek_token.literal, "x");
}

test "let statements" {
    const input =
        \\let x = 3;
        \\let foo = 400;
    ;

    const allocator = std.testing.allocator;
    const lexer = try lex.Lexer.init(allocator, input);
    var parser = try Parser.init(allocator, lexer);
    defer lexer.deinit();
    defer parser.deinit();
    const program = try parser.parseProgram();
    try assertNoErrors(&parser);
    try std.testing.expectEqual(2, program.statements.len);
    try testLetStatement(program.statements[0], "x");
    try testLetStatement(program.statements[1], "foo");
}

test "let statement errors" {
    const test_cases = .{
        .{
            .input = "let = 10;",
            .expected_error = "expected next token to be ident",
        },
        .{
            .input = "let x 10;",
            .expected_error = "expected next token to be assign",
        },
    };

    const allocator = std.testing.allocator;

    inline for (test_cases) |test_case| {
        const lexer = try lex.Lexer.init(allocator, test_case.input);
        var parser = try Parser.init(allocator, lexer);
        defer lexer.deinit();
        defer parser.deinit();
        _ = parser.parseProgram() catch {
            const error_msg = if (parser.parser_error != null) parser.parser_error.?.* else "";
            try std.testing.expect(std.mem.indexOf(u8, error_msg, test_case.expected_error) != null);
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
    const lexer = try lex.Lexer.init(allocator, input);
    var parser = try Parser.init(allocator, lexer);
    defer lexer.deinit();
    defer parser.deinit();
    const program = try parser.parseProgram();
    try assertNoErrors(&parser);
    try std.testing.expectEqual(@as(usize, 3), program.statements.len);
    try testReturnStatement(program.statements[0], "");
    try testReturnStatement(program.statements[1], "");
    try testReturnStatement(program.statements[2], "");
}

fn testLetStatement(s: *ast.Statement, name: []const u8) !void {
    try std.testing.expectEqualStrings("let", s.tokenLiteral());

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
        },
        else => unreachable,
    }
}

fn assertNoErrors(p: *Parser) !void {
    try std.testing.expectEqual(p.parser_error, null);
}
