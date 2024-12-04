const std = @import("std");
const token = @import("token.zig");

pub const NodeType = enum { Statement, Expression, Program };

pub const Node = union(NodeType) {
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

pub const StatementTypes = enum { Let };
pub const Statement = union(StatementTypes) {
    Let: *LetStatement,
    pub fn tokenLiteral(self: *const Statement) []const u8 {
        switch (self.*) {
            .Let => |n| return n.tokenLiteral(),
        }
    }
};

pub const LetStatement = struct {
    token: token.Token,
    name: *Identifier,
    expr: ?*Expression,
    pub fn tokenLiteral(self: *const LetStatement) []const u8 {
        return self.token.literal;
    }
};

pub const ExpressionTypes = enum {
    Identifier,
};
pub const Expression = union(ExpressionTypes) {
    Identifier: Identifier,
    pub fn tokenLiteral(self: *const Statement) []const u8 {
        switch (self.*) {
            .Identifier => |n| return n.tokenLiteral(),
        }
    }
};

pub const Identifier = struct {
    token: token.Token,
    value: []const u8,
    pub fn tokenLiteral(self: *const Identifier) []const u8 {
        return self.token.literal;
    }
};

pub const Program = struct {
    statements: []*Statement,

    pub fn tokenLiteral(self: *const Program) []const u8 {
        if (self.statements.len > 0) {
            return self.statements[0].tokenLiteral();
        } else {
            return "";
        }
    }
};
