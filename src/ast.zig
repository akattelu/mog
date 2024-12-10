const std = @import("std");
const token = @import("token.zig");
const AllocatorError = std.mem.Allocator.Error;

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
    pub fn write(self: *const Statement, writer: anytype) !void {
        switch (self.*) {
            .Let => |n| try n.write(writer),
            .Return => |n| try n.write(writer),
            .Expression => |n| try n.write(writer),
        }
        _ = try writer.writeAll(";");
    }
};

pub const ReturnStatement = struct {
    token: token.Token,
    // TODO: make this required
    expr: ?*Expression,

    pub fn tokenLiteral(self: *const ReturnStatement) []const u8 {
        return self.token.literal;
    }

    pub fn write(self: *const ReturnStatement, writer: anytype) !void {
        _ = try writer.writeAll("return ");
        if (self.expr != null) {
            try self.expr.?.write(writer);
        }
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
    pub fn write(self: *const LetStatement, writer: anytype) !void {
        _ = try writer.writeAll("let ");
        try self.name.write(writer);
        _ = try writer.writeAll(" = ");
        if (self.expr != null) {
            try self.expr.?.write(writer);
        }
    }
};

pub const ExpressionStatement = struct {
    expr: *Expression,

    pub fn tokenLiteral(self: *const ExpressionStatement) []const u8 {
        return self.expr.tokenLiteral();
    }
    pub fn write(self: *const ExpressionStatement, writer: anytype) !void {
        try self.expr.write(writer);
    }
};

pub const ExpressionTypes = enum { Identifier, Integer, Prefix, Infix };
pub const Expression = union(ExpressionTypes) {
    Identifier: *Identifier,
    Integer: *Integer,
    Prefix: *PrefixExpression,
    Infix: *InfixExpression,

    pub fn tokenLiteral(self: *const Expression) []const u8 {
        return self.tokenLiteral();
    }

    pub fn write(self: *const Expression, writer: anytype) !void {
        switch (self.*) {
            .Identifier => |n| try n.write(writer),
            .Integer => |n| try n.write(writer),
            .Prefix => |n| try n.write(writer),
            .Infix => |n| try n.write(writer),
        }
    }
};

pub const Identifier = struct {
    token: token.Token,
    value: []const u8,
    pub fn tokenLiteral(self: *const Identifier) []const u8 {
        return self.token.literal;
    }
    pub fn write(self: *const Identifier, writer: anytype) !void {
        _ = try writer.writeAll(self.value);
    }
};

pub const InfixExpression = struct {
    token: token.Token,
    operator: []const u8,
    left: *Expression,
    right: *Expression,

    pub fn tokenLiteral(self: *const InfixExpression) []const u8 {
        return self.token.literal;
    }

    pub fn write(self: *InfixExpression, writer: anytype) (AllocatorError || std.fs.File.Writer.Error)!void {
        _ = try writer.writeAll("(");
        try self.left.write(writer);
        _ = try writer.writeAll(" ");
        _ = try writer.writeAll(self.operator);
        _ = try writer.writeAll(" ");
        try self.right.write(writer);
        _ = try writer.writeAll(")");
    }
};

pub const PrefixExpression = struct {
    token: token.Token,
    operator: []const u8,
    expression: *Expression,

    pub fn tokenLiteral(self: *const PrefixExpression) []const u8 {
        return self.token.literal;
    }

    pub fn write(self: *PrefixExpression, writer: anytype) (AllocatorError || std.fs.File.Writer.Error)!void {
        _ = try writer.writeAll("(");
        _ = try writer.writeAll(self.operator);
        _ = try writer.writeAll(" ");
        try self.expression.write(writer);
        _ = try writer.writeAll(")");
    }
};

pub const Integer = struct {
    token: token.Token,
    value: i32,
    pub fn tokenLiteral(self: *const Integer) []const u8 {
        return self.token.literal;
    }
    pub fn write(self: *const Integer, writer: anytype) !void {
        try writer.print("{d}", .{self.value});
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
    pub fn write(self: *const Program, writer: anytype) !void {
        if (self.statements.len == 1) {
            try self.statements[0].write(writer);
        } else {
            for (self.statements) |stmt| {
                try stmt.write(writer);
                try writer.writeAll("\n");
            }
        }
    }
};
