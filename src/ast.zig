const std = @import("std");
const token = @import("token.zig");
const Writer = std.Io.Writer;
const AllocatorError = std.mem.Allocator.Error;
const testing = std.testing;

/// The different types of statements in the AST.
pub const StatementTypes = enum { Assignment, Return, Expression };

// Numbers can be integers or floats
pub const NumberType = enum { Integer, Float };

/// A tagged union representing any statement in the language.
/// Statements are the top-level constructs that make up a program.
pub const Statement = union(StatementTypes) {
    Assignment: *AssignmentStatement,
    Return: *ReturnStatement,
    Expression: *ExpressionStatement,

    /// Returns the literal text of the first token in this statement.
    /// Useful for debugging and error messages.
    pub fn tokenLiteral(self: *const Statement) []const u8 {
        switch (self.*) {
            .Assignment => |n| return n.tokenLiteral(),
            .Return => |n| return n.tokenLiteral(),
            .Expression => |n| return n.tokenLiteral(),
        }
    }

    /// Writes the statement to the given writer in valid source code format.
    /// All statements are terminated with a semicolon.
    pub fn write(self: *const Statement, writer: *Writer) !void {
        switch (self.*) {
            .Assignment => |n| try n.write(writer),
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

/// Represents an assignment statement, either local or global.
/// Examples: `local x = 42`, `x, y = 1, 2`, `local a, b = foo()`
pub const AssignmentStatement = struct {
    /// The token (either 'local' keyword or first identifier)
    token: token.Token,
    /// The list of identifiers being assigned to
    names: *NameList,
    /// The expression assigned to the variables
    expr: *Expression,
    /// Whether this is a local declaration
    is_local: bool,

    /// Returns the literal text of the token.
    pub fn tokenLiteral(self: *const AssignmentStatement) []const u8 {
        return self.token.literal;
    }

    /// Writes the assignment statement to the given writer.
    /// Format: "[local] <names> = <expr>"
    pub fn write(self: *const AssignmentStatement, writer: *Writer) !void {
        if (self.is_local) {
            _ = try writer.writeAll("local ");
        }
        try self.names.write(writer);
        _ = try writer.writeAll(" = ");
        try self.expr.write(writer);
    }
};

// Represents one or more identifiers
// Used in assignment statements where there are multiple left hand side variables
pub const NameList = struct {
    // The token pointing to the first identifier
    token: token.Token,

    /// List of names on LHS
    names: std.ArrayList(*Identifier),

    /// Allocator used for adding more elements and printing
    allocator: std.mem.Allocator,

    /// Create a NameList with one identifier
    /// Caller must call deinit() to free memory
    pub fn init(allocator: std.mem.Allocator, name: *Identifier) !NameList {
        var names = try std.ArrayList(*Identifier).initCapacity(allocator, 3);
        try names.append(allocator, name);
        return .{ .token = name.token, .names = names, .allocator = allocator };
    }

    /// Add a new identifier to the NameList
    pub fn add(self: *NameList, name: *Identifier) !void {
        try self.names.append(self.allocator, name);
    }

    /// Clear memory in the NameList
    pub fn deinit(self: *NameList) void {
        self.names.deinit(self.allocator);
    }

    /// Returns the literal token text for the first name
    pub fn tokenLiteral(self: *const NameList) []const u8 {
        if (self.names.items.len > 0) {
            return self.names.items[0].value;
        }
        return "";
    }

    // Writes comma separated name list to writer
    pub fn write(self: *const NameList, writer: *Writer) !void {
        if (self.names.items.len <= 1) {
            try self.names.items[0].write(writer);
            return;
        }

        var i: usize = 0;
        while (i < self.names.items.len - 1) : (i += 1) {
            try self.names.items[i].write(writer);
            try writer.writeAll(", ");
        }
        try self.names.items[i].write(writer);
        return;
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
pub const ExpressionTypes = enum { Identifier, Number, String, Prefix, Infix, Conditional, Boolean, Nil, Varargs, FunctionDef };

/// A tagged union representing any expression in the language.
/// Expressions are constructs that evaluate to values.
pub const Expression = union(ExpressionTypes) {
    Identifier: *Identifier,
    Number: *NumberLiteral,
    String: *StringLiteral,
    Prefix: *PrefixExpression,
    Infix: *InfixExpression,
    Conditional: *ConditionalExpression,
    Boolean: *BooleanLiteral,
    Nil: *Nil,
    Varargs: *Varargs,
    FunctionDef: *FunctionDefExpression,

    /// Returns the literal text of the first token in this expression.
    /// Useful for debugging and error messages.
    pub fn tokenLiteral(self: *const Expression) []const u8 {
        return switch (self.*) {
            .Identifier => |n| n.tokenLiteral(),
            .Number => |n| n.tokenLiteral(),
            .String => |n| n.tokenLiteral(),
            .Boolean => |n| n.tokenLiteral(),
            .Prefix => |n| n.tokenLiteral(),
            .Infix => |n| n.tokenLiteral(),
            .Conditional => |n| n.tokenLiteral(),
            .Nil => |n| n.tokenLiteral(),
            .Varargs => |n| n.tokenLiteral(),
            .FunctionDef => |n| n.tokenLiteral(),
        };
    }

    /// Writes the expression to the given writer in valid source code format.
    /// Delegates to the specific expression type's write method.
    pub fn write(self: *const Expression, writer: *Writer) Writer.Error!void {
        switch (self.*) {
            .Identifier => |n| try n.write(writer),
            .Number => |n| try n.write(writer),
            .String => |n| try n.write(writer),
            .Boolean => |n| try n.write(writer),
            .Prefix => |n| try n.write(writer),
            .Infix => |n| try n.write(writer),
            .Conditional => |n| try n.write(writer),
            .Nil => |n| try n.write(writer),
            .Varargs => |n| try n.write(writer),
            .FunctionDef => |n| try n.write(writer),
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

/// Represents an integer or float literal value
/// The inner `value` field is a union over i32 or f32
pub const NumberLiteral = struct {
    token: token.Token,
    value: union(NumberType) {
        Integer: i32,
        Float: f32,
    },

    pub fn tokenLiteral(self: *const NumberLiteral) []const u8 {
        return self.token.literal;
    }

    pub fn write(self: *const NumberLiteral, writer: *Writer) !void {
        switch (self.value) {
            .Integer => |i| {
                try writer.print("{d}", .{i});
            },
            .Float => |f| {
                try writer.print("{d}", .{f});
            },
        }
    }
};

/// Represents a string literal value in the source code.
/// Example: `"hello"`, `"world"`, `"foo bar"`
pub const StringLiteral = struct {
    /// The string token
    token: token.Token,
    /// The string value (as it appears in source, including quotes)
    value: []const u8,

    /// Returns the literal text of the string token.
    pub fn tokenLiteral(self: *const StringLiteral) []const u8 {
        return self.token.literal;
    }

    /// Writes the string value to the given writer with quotes.
    pub fn write(self: *const StringLiteral, writer: *Writer) !void {
        _ = try writer.print("\"{s}\"", .{self.value});
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

/// Represents the varargs expression (...)
/// Used in function parameters and expressions to represent variable arguments.
/// Example: `function foo(...) end` or `return ...`
pub const Varargs = struct {
    /// The ... token
    token: token.Token,

    /// Returns the literal text of the varargs token.
    pub fn tokenLiteral(self: *const Varargs) []const u8 {
        return self.token.literal;
    }

    /// Writes the varargs to the given writer as "...".
    pub fn write(_: *const Varargs, writer: *Writer) !void {
        try writer.print("...", .{});
    }
};

/// Represents a list of function parameters
/// Can contain named parameters and/or varargs
/// Example: `x, y, z` or `a, b, ...` or just `...`
pub const ParamList = struct {
    /// The token of the first parameter
    token: token.Token,
    /// List of parameter names
    names: std.ArrayList(*Identifier),
    /// Whether this function accepts varargs
    has_varargs: bool,
    /// Allocator used for managing the list
    allocator: std.mem.Allocator,

    /// Create a ParamList with one identifier
    pub fn init(allocator: std.mem.Allocator, name: *Identifier) !ParamList {
        var names = try std.ArrayList(*Identifier).initCapacity(allocator, 3);
        try names.append(allocator, name);
        return .{ .token = name.token, .names = names, .has_varargs = false, .allocator = allocator };
    }

    /// Create a ParamList with only varargs
    pub fn initVarargs(allocator: std.mem.Allocator, varargs_token: token.Token) !ParamList {
        const names = std.ArrayList(*Identifier).initCapacity(allocator, 0) catch |err| return err;
        return .{ .token = varargs_token, .names = names, .has_varargs = true, .allocator = allocator };
    }

    /// Add a new parameter to the list
    pub fn add(self: *ParamList, name: *Identifier) !void {
        try self.names.append(self.allocator, name);
    }

    /// Mark this parameter list as having varargs
    pub fn setVarargs(self: *ParamList) void {
        self.has_varargs = true;
    }

    /// Clear memory
    pub fn deinit(self: *ParamList) void {
        self.names.deinit();
    }

    /// Returns the literal token text
    pub fn tokenLiteral(self: *const ParamList) []const u8 {
        if (self.names.items.len > 0) {
            return self.names.items[0].value;
        }
        return "...";
    }

    /// Writes comma-separated parameter list to writer
    pub fn write(self: *const ParamList, writer: *Writer) !void {
        if (self.names.items.len == 0 and self.has_varargs) {
            try writer.writeAll("...");
            return;
        }

        var i: usize = 0;
        while (i < self.names.items.len) : (i += 1) {
            try self.names.items[i].write(writer);
            if (i < self.names.items.len - 1 or self.has_varargs) {
                try writer.writeAll(", ");
            }
        }

        if (self.has_varargs) {
            try writer.writeAll("...");
        }
    }
};

/// Represents a function body including parameters and block
/// Example: `(x, y) return x + y end`
pub const FunctionBody = struct {
    /// The opening paren token
    token: token.Token,
    /// Optional parameter list
    params: ?*ParamList,
    /// The function body block
    block: *Block,

    /// Returns the literal text of the token
    pub fn tokenLiteral(self: *const FunctionBody) []const u8 {
        return self.token.literal;
    }

    /// Writes the function body to writer
    /// Format: "([params]) <block> end"
    pub fn write(self: *const FunctionBody, writer: *Writer) !void {
        try writer.writeAll("(");
        if (self.params != null) {
            try self.params.?.write(writer);
        }
        try writer.writeAll(")");
        if (self.block.statements.len > 0) {
            try writer.writeAll(" ");
        }
        try self.block.write(writer);
        try writer.writeAll(" end");
    }
};

/// Represents a function definition expression
/// Example: `function(x, y) return x + y end`
pub const FunctionDefExpression = struct {
    /// The 'function' keyword token
    token: token.Token,
    /// The function body
    body: *FunctionBody,

    /// Returns the literal text of the 'function' keyword
    pub fn tokenLiteral(self: *const FunctionDefExpression) []const u8 {
        return self.token.literal;
    }

    /// Writes the function definition to writer
    /// Format: "function<body>"
    pub fn write(self: *const FunctionDefExpression, writer: *Writer) !void {
        try writer.writeAll("function");
        try self.body.write(writer);
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
