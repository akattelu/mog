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
    expr: *Expression,
    pub fn tokenLiteral(self: *const LetStatement) []const u8 {
        return self.token.literal;
    }
    pub fn write(self: *const LetStatement, writer: anytype) !void {
        _ = try writer.writeAll("let ");
        try self.name.write(writer);
        _ = try writer.writeAll(" = ");
        try self.expr.write(writer);
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

pub const ExpressionTypes = enum { Identifier, Integer, Prefix, Infix, Conditional, Boolean };
pub const Expression = union(ExpressionTypes) {
    Identifier: *Identifier,
    Integer: *IntegerLiteral,
    Prefix: *PrefixExpression,
    Infix: *InfixExpression,
    Conditional: *ConditionalExpression,
    Boolean: *BooleanLiteral,

    pub fn tokenLiteral(self: *const Expression) []const u8 {
        return self.tokenLiteral();
    }

    pub fn write(self: *const Expression, writer: anytype) !void {
        switch (self.*) {
            .Identifier => |n| try n.write(writer),
            .Integer => |n| try n.write(writer),
            .Boolean => |n| try n.write(writer),
            .Prefix => |n| try n.write(writer),
            .Infix => |n| try n.write(writer),
            .Conditional => |n| try n.write(writer),
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

pub const ConditionalExpression = struct {
    token: token.Token,
    condition: *Expression,
    then_block: *Block,
    else_block: ?*Block,

    pub fn tokenLiteral(self: *const PrefixExpression) []const u8 {
        return self.token.literal;
    }

    pub fn write(self: *ConditionalExpression, writer: anytype) (AllocatorError || std.fs.File.Writer.Error)!void {
        _ = try writer.writeAll("if ");
        try self.condition.write(writer);
        _ = try writer.writeAll(" then ");
        try self.then_block.write(writer);
        if (self.else_block != null) {
            _ = try writer.writeAll(" else ");
            try self.else_block.?.write(writer);
        }
        _ = try writer.writeAll(" end");
    }
};

// blocks are also collections of statements
pub const Block = Program;

pub const IntegerLiteral = struct {
    token: token.Token,
    value: i32,
    pub fn tokenLiteral(self: *const IntegerLiteral) []const u8 {
        return self.token.literal;
    }
    pub fn write(self: *const IntegerLiteral, writer: anytype) !void {
        try writer.print("{d}", .{self.value});
    }
};

pub const BooleanLiteral = struct {
    token: token.Token,
    value: bool,

    pub fn tokenLiteral(self: *const BooleanLiteral) []const u8 {
        return self.token.literal;
    }

    pub fn write(self: *const BooleanLiteral, writer: anytype) !void {
        if (self.value) {
            try writer.print("{s}", .{self.value});
        } else {
            try writer.print("{s}", .{self.value});
        }
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
