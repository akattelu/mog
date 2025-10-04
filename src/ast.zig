const std = @import("std");
const token = @import("token.zig");
const Writer = std.Io.Writer;
const AllocatorError = std.mem.Allocator.Error;

/// The different types of statements in the AST.
pub const StatementTypes = enum { Let, Return, Expression };

/// A tagged union representing any statement in the language.
/// Statements are the top-level constructs that make up a program.
pub const Statement = union(StatementTypes) {
    Let: *LetStatement,
    Return: *ReturnStatement,
    Expression: *ExpressionStatement,

    /// Returns the literal text of the first token in this statement.
    /// Useful for debugging and error messages.
    pub fn tokenLiteral(self: *const Statement) []const u8 {
        switch (self.*) {
            .Let => |n| return n.tokenLiteral(),
            .Return => |n| return n.tokenLiteral(),
            .Expression => |n| return n.tokenLiteral(),
        }
    }

    /// Writes the statement to the given writer in valid source code format.
    /// All statements are terminated with a semicolon.
    pub fn write(self: *const Statement, writer: *Writer) !void {
        switch (self.*) {
            .Let => |n| try n.write(writer),
            .Return => |n| try n.write(writer),
            .Expression => |n| try n.write(writer),
        }
        _ = try writer.writeAll(";");
    }
};

/// Represents a return statement that exits a function with an optional value.
/// Example: `return 42` or `return`
pub const ReturnStatement = struct {
    /// The 'return' keyword token
    token: token.Token,
    /// The optional expression to return (null for bare return statements)
    expr: ?*Expression,

    /// Returns the literal text of the return keyword.
    pub fn tokenLiteral(self: *const ReturnStatement) []const u8 {
        return self.token.literal;
    }

    /// Writes the return statement to the given writer.
    /// Format: "return <expr>" or "return" if expr is null.
    pub fn write(self: *const ReturnStatement, writer: *Writer) !void {
        _ = try writer.writeAll("return ");
        if (self.expr != null) {
            try self.expr.?.write(writer);
        }
    }
};

/// Represents a local variable declaration with initialization.
/// Example: `local x = 42` or `local name = "value"`
pub const LetStatement = struct {
    /// The 'local' keyword token
    token: token.Token,
    /// The identifier being declared
    name: *Identifier,
    /// The expression assigned to the variable
    expr: *Expression,

    /// Returns the literal text of the 'local' keyword.
    pub fn tokenLiteral(self: *const LetStatement) []const u8 {
        return self.token.literal;
    }

    /// Writes the let statement to the given writer.
    /// Format: "local <name> = <expr>"
    pub fn write(self: *const LetStatement, writer: *Writer) !void {
        _ = try writer.writeAll("local ");
        try self.name.write(writer);
        _ = try writer.writeAll(" = ");
        try self.expr.write(writer);
    }
};

/// Represents a statement that consists of a single expression.
/// This allows expressions to appear as standalone statements.
/// Example: `x + 5` or `foo()` as a complete statement.
pub const ExpressionStatement = struct {
    /// The expression that makes up this statement
    expr: *Expression,

    /// Returns the literal text of the first token in the expression.
    pub fn tokenLiteral(self: *const ExpressionStatement) []const u8 {
        return self.expr.tokenLiteral();
    }

    /// Writes the expression statement to the given writer.
    /// Simply delegates to the expression's write method.
    pub fn write(self: *const ExpressionStatement, writer: *Writer) !void {
        try self.expr.write(writer);
    }
};

/// The different types of expressions in the AST.
pub const ExpressionTypes = enum { Identifier, Integer, Prefix, Infix, Conditional, Boolean, Nil };

/// A tagged union representing any expression in the language.
/// Expressions are constructs that evaluate to values.
pub const Expression = union(ExpressionTypes) {
    Identifier: *Identifier,
    Integer: *IntegerLiteral,
    Prefix: *PrefixExpression,
    Infix: *InfixExpression,
    Conditional: *ConditionalExpression,
    Boolean: *BooleanLiteral,
    Nil: *Nil,

    /// Returns the literal text of the first token in this expression.
    /// Useful for debugging and error messages.
    pub fn tokenLiteral(self: *const Expression) []const u8 {
        return switch (self.*) {
            .Identifier => |n| n.tokenLiteral(),
            .Integer => |n| n.tokenLiteral(),
            .Boolean => |n| n.tokenLiteral(),
            .Prefix => |n| n.tokenLiteral(),
            .Infix => |n| n.tokenLiteral(),
            .Conditional => |n| n.tokenLiteral(),
            .Nil => |n| n.tokenLiteral(),
        };
    }

    /// Writes the expression to the given writer in valid source code format.
    /// Delegates to the specific expression type's write method.
    pub fn write(self: *const Expression, writer: *Writer) Writer.Error!void {
        switch (self.*) {
            .Identifier => |n| try n.write(writer),
            .Integer => |n| try n.write(writer),
            .Boolean => |n| try n.write(writer),
            .Prefix => |n| try n.write(writer),
            .Infix => |n| try n.write(writer),
            .Conditional => |n| try n.write(writer),
            .Nil => |n| try n.write(writer),
        }
    }
};

/// Represents a variable or function name identifier.
/// Example: `x`, `myVariable`, `calculateSum`
pub const Identifier = struct {
    /// The identifier token
    token: token.Token,
    /// The actual name/value of the identifier
    value: []const u8,

    /// Returns the literal text of the identifier token.
    pub fn tokenLiteral(self: *const Identifier) []const u8 {
        return self.token.literal;
    }

    /// Writes the identifier name to the given writer.
    pub fn write(self: *const Identifier, writer: *Writer) !void {
        _ = try writer.writeAll(self.value);
    }
};

/// Represents a binary infix operation with left and right operands.
/// Example: `x + y`, `a == b`, `5 * 3`
pub const InfixExpression = struct {
    /// The operator token (+, -, *, /, ==, !=, <, >, etc.)
    token: token.Token,
    /// The operator symbol as a string
    operator: []const u8,
    /// The left-hand operand
    left: *Expression,
    /// The right-hand operand
    right: *Expression,

    /// Returns the literal text of the operator token.
    pub fn tokenLiteral(self: *const InfixExpression) []const u8 {
        return self.token.literal;
    }

    /// Writes the infix expression to the given writer.
    /// Format: "(<left> <operator> <right>)" with parentheses for grouping.
    pub fn write(self: *InfixExpression, writer: *Writer) !void {
        _ = try writer.writeAll("(");
        try self.left.write(writer);
        _ = try writer.writeAll(" ");
        _ = try writer.writeAll(self.operator);
        _ = try writer.writeAll(" ");
        try self.right.write(writer);
        _ = try writer.writeAll(")");
    }
};

/// Represents a unary prefix operation applied to an expression.
/// Example: `-x`, `not y`, `~z`
pub const PrefixExpression = struct {
    /// The operator token (-, not, ~, etc.)
    token: token.Token,
    /// The operator symbol as a string
    operator: []const u8,
    /// The expression the operator is applied to
    expression: *Expression,

    /// Returns the literal text of the operator token.
    pub fn tokenLiteral(self: *const PrefixExpression) []const u8 {
        return self.token.literal;
    }

    /// Writes the prefix expression to the given writer.
    /// Format: "(<operator><expr>)" or "(<operator> <expr>)" for word operators.
    /// Word operators like 'not' get a space, symbol operators like '-' do not.
    pub fn write(self: *PrefixExpression, writer: *Writer) !void {
        _ = try writer.writeAll("(");
        _ = try writer.writeAll(self.operator);
        // Only add space for word operators (not, and, or)
        if (std.mem.eql(u8, self.operator, "not")) {
            _ = try writer.writeAll(" ");
        }
        try self.expression.write(writer);
        _ = try writer.writeAll(")");
    }
};

/// Represents an if-then-else conditional expression.
/// Example: `if x > 0 then print(x) else print(-x) end`
pub const ConditionalExpression = struct {
    /// The 'if' keyword token
    token: token.Token,
    /// The boolean condition to evaluate
    condition: *Expression,
    /// The block of statements to execute if condition is true
    then_block: *Block,
    /// The optional block of statements to execute if condition is false
    else_block: ?*Block,

    /// Returns the literal text of the 'if' keyword.
    pub fn tokenLiteral(self: *const ConditionalExpression) []const u8 {
        return self.token.literal;
    }

    /// Writes the conditional expression to the given writer.
    /// Format: "if <condition> then <then_block> [else <else_block>] end"
    pub fn write(self: *ConditionalExpression, writer: *Writer) !void {
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

/// A block is a collection of statements, just like a Program.
/// Used within conditional expressions and other control flow constructs.
pub const Block = Program;

/// Represents an integer literal value in the source code.
/// Example: `42`, `-10`, `0`
pub const IntegerLiteral = struct {
    /// The integer token
    token: token.Token,
    /// The parsed integer value (currently limited to i32)
    value: i32,

    /// Returns the literal text of the integer token.
    pub fn tokenLiteral(self: *const IntegerLiteral) []const u8 {
        return self.token.literal;
    }

    /// Writes the integer value to the given writer in decimal format.
    pub fn write(self: *const IntegerLiteral, writer: *Writer) !void {
        try writer.print("{d}", .{self.value});
    }
};

/// Represents a boolean literal value in the source code.
/// Example: `true`, `false`
pub const BooleanLiteral = struct {
    /// The boolean keyword token
    token: token.Token,
    /// The boolean value (true or false)
    value: bool,

    /// Returns the literal text of the boolean token.
    pub fn tokenLiteral(self: *const BooleanLiteral) []const u8 {
        return self.token.literal;
    }

    /// Writes the boolean value to the given writer as "true" or "false".
    pub fn write(self: *const BooleanLiteral, writer: *Writer) !void {
        if (self.value) {
            try writer.print("true", .{});
        } else {
            try writer.print("false", .{});
        }
    }
};

/// Represents the nil literal
pub const Nil = struct {
    /// The nil token
    token: token.Token,

    /// Returns the literal text of the integer token.
    pub fn tokenLiteral(self: *const Nil) []const u8 {
        return self.token.literal;
    }

    /// Writes the integer value to the given writer in decimal format.
    pub fn write(_: *const Nil, writer: *Writer) !void {
        try writer.print("nil", .{});
    }
};

/// Represents the root node of the AST, containing all top-level statements.
/// A Program is essentially a collection of statements that make up the source code.
pub const Program = struct {
    /// The list of statements that make up this program
    statements: []*Statement,

    /// Returns the literal text of the first token in the program.
    /// Returns an empty string if the program has no statements.
    pub fn tokenLiteral(self: *const Program) []const u8 {
        if (self.statements.len > 0) {
            return self.statements[0].tokenLiteral();
        } else {
            return "";
        }
    }

    /// Writes the program to the given writer in valid source code format.
    /// Single-statement programs are written inline; multi-statement programs
    /// have each statement on its own line.
    pub fn write(self: *const Program, writer: *Writer) !void {
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
