const std = @import("std");
const token = @import("token.zig");

const NodeType = enum { Statement, Expression, Program };

const Node = union(NodeType) {
    Statement: Statement,
    Expression: Expression,
    Program: Program,

    pub fn tokenLiteral(self: *const Node) []const u8 {
        switch (self.*) {
            .Statement => |n| return n.tokenLiteral(),
            .Expression => |n| return n.tokenLiteral(),
            .Expression => |n| return n.tokenLiteral(),
        }
    }
};

const StatementTypes = enum { Let };
const Statement = union(StatementTypes) {
    Let: LetStatement,
    pub fn tokenLiteral(self: *const Statement) []const u8 {
        switch (self.*) {
            .Let => |n| return n.tokenLiteral(),
        }
    }
};

const LetStatement = struct {
    token: token.Token,
    name: *Identifier,
    expr: *Expression,
    pub fn tokenLiteral(self: *const LetStatement) []const u8 {
        return self.token.literal;
    }
};

const ExpressionTypes = enum {
    Identifier,
};
const Expression = union(ExpressionTypes) {
    pub fn tokenLiteral(self: *const Statement) []const u8 {
        switch (self.*) {
            .Identifier => |n| return n.tokenLiteral(),
        }
    }
};

const Identifier = struct {
    token: token.Token,
    value: []u8,
    pub fn tokenLiteral(self: *const Identifier) []const u8 {
        return self.token.literal;
    }
};

const Program = struct {
    statements: []Statement,

    pub fn tokenLiteral(self: *const Program) []const u8 {
        if (self.statements.len > 0) {
            return self.statements[0].tokenLiteral();
        } else {
            return "";
        }
    }
};

pub const Parser = struct {
    lexer: *token.Lexer,
    current_token: *token.Token,
    peek_token: *token.Token,

    pub fn init(lex: *token.Lexer) !Parser {
        const first = try lex.nextToken();
        const second = try lex.nextToken();
        return Parser{
            .lexer = lex,
            .current_token = first,
            .peek_token = second,
        };
    }

    pub fn deinit(self: *Parser) void {
        return self.lexer.deinit();
    }

    pub fn nextToken(self: *Parser) !void {
        self.current_token = self.peek_token;
        self.peek_token = try self.lexer.nextToken();
    }
};

test "create parser over string" {
    const input = "let x = 3";
    const allocator = std.testing.allocator;
    var lexer = token.Lexer.init(allocator, input);
    defer lexer.deinit();

    const parser = try Parser.init(&lexer);
    try std.testing.expectEqualStrings(parser.current_token.literal, "let");
    try std.testing.expectEqualStrings(parser.peek_token.literal, "x");
}
