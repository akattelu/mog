const std = @import("std");
const token = @import("token.zig");
const AnyWriter = std.io.AnyWriter;

pub const NodeType = enum { Statement, Expression, Program };

pub const Node = union(NodeType) {
    Statement: Statement,
    Expression: Expression,
    Program: Program,

    pub fn tokenLiteral(self: *const Node) []const u8 {
        switch (self.*) {
            .Statement => |n| return n.tokenLiteral(),
            .Expression => |n| return n.tokenLiteral(),
            .Program => |n| return n.tokenLiteral(),
        }
    }

    pub fn string(self: *const Node, writer: AnyWriter) !void {
        switch (self.*) {
            .Statement => |n| try n.string(writer),
            .Expression => |n| try n.string(writer),
            .Program => |n| try n.string(writer),
        }
    }
};

pub const StatementTypes = enum { Let, Return, Expression };
pub const Statement = union(StatementTypes) {
    Let: *LetStatement,
    Return: *ReturnStatement,
    Expression: *ExpressionStatement,
    pub fn tokenLiteral(self: *const Statement) []const u8 {
        switch (self.*) {
            .Let => |n| return n.tokenLiteral(),
            .Return => |n| return n.tokenLiteral(),
            .Expression => |n| return n.tokenLiteral(),
        }
    }
    pub fn string(self: *const Statement, writer: AnyWriter) !void {
        switch (self.*) {
            .Let => |n| try n.string(writer),
            .Return => |n| try n.string(writer),
            .Expression => |n| try n.string(writer),
        }
    }
};

pub const ReturnStatement = struct {
    token: token.Token,
    // TODO: make this required
    expr: ?*Expression,

    pub fn tokenLiteral(self: *const ReturnStatement) []const u8 {
        return self.token.literal;
    }

    pub fn string(self: *const ReturnStatement, writer: AnyWriter) !void {
        try writer.write("return ");
        if (self.expr != null) {
            try self.expr.?.string(writer);
        }
        try writer.write(";");
    }
};

pub const LetStatement = struct {
    token: token.Token,
    name: *Identifier,
    // TODO: make this required
    expr: ?*Expression,
    pub fn tokenLiteral(self: *const LetStatement) []const u8 {
        return self.token.literal;
    }
    pub fn string(self: *const LetStatement, writer: AnyWriter) !void {
        try writer.write("let ");
        try self.name.string(writer);
        try writer.write(" = ");
        if (self.expr != null) {
            try self.expr.?.string(writer);
        }
        try writer.write(";");
    }
};

pub const ExpressionStatement = struct {
    token: token.Token,
    expr: *Expression,

    pub fn tokenLiteral(self: *const ExpressionStatement) []const u8 {
        return self.token.literal;
    }
    pub fn string(self: *const ExpressionStatement, writer: AnyWriter) !void {
        try self.expr.string(writer);
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
    pub fn string(self: *const Statement, writer: AnyWriter) !void {
        switch (self.*) {
            .Identifier => |n| try n.string(writer),
        }
    }
};

pub const Identifier = struct {
    token: token.Token,
    value: []const u8,
    pub fn tokenLiteral(self: *const Identifier) []const u8 {
        return self.token.literal;
    }
    pub fn string(self: *const Identifier, writer: AnyWriter) !void {
        try writer.write(self.value);
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
    pub fn string(self: *const Program, writer: AnyWriter) !void {
        for (self.statements) |stmt| {
            try stmt.string(writer);
            try stmt.string("\n");
        }
    }
};
