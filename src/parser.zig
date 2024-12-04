const std = @import("std");
const token = @import("token.zig");
const lex = @import("lexer.zig");
const ast = @import("ast.zig");

pub const Parser = struct {
    lexer: *lex.Lexer,
    current_token: *token.Token,
    peek_token: *token.Token,
    alloc: std.heap.ArenaAllocator,

    pub fn init(allocator: std.mem.Allocator, lexer: *lex.Lexer) !Parser {
        const first = try lexer.nextToken();
        const second = try lexer.nextToken();
        const alloc = std.heap.ArenaAllocator.init(allocator);
        return Parser{
            .lexer = lexer,
            .current_token = first,
            .peek_token = second,
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
        try self.nextToken();
        ls.name = try self.parseIdentifier();
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
        \\let y = 5;
        \\let foo = 400;
    ;

    const allocator = std.testing.allocator;
    const lexer = try lex.Lexer.init(allocator, input);
    var parser = try Parser.init(allocator, lexer);
    defer lexer.deinit();
    defer parser.deinit();
    const program = (try parser.parseProgram()).*;
    try std.testing.expectEqual(3, program.statements.len);
    try testLetStatement(program.statements[0], "x");
    try testLetStatement(program.statements[1], "y");
    try testLetStatement(program.statements[2], "foo");
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
    const program = (try parser.parseProgram()).*;
    try std.testing.expectEqual(3, program.statements.len);
    try testReturnStatement(program.statements[0], "x");
    try testReturnStatement(program.statements[1], "y");
    try testReturnStatement(program.statements[2], "foo");
}

fn testLetStatement(s: *ast.Statement, name: []const u8) !void {
    try std.testing.expectEqualStrings("let", s.tokenLiteral());
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
    try std.testing.expectEqualStrings("return", s.tokenLiteral());

    switch (s.*) {
        .Return => {
            // TODO: test the expression
        },
        else => unreachable,
    }
}
