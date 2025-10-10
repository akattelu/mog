const std = @import("std");
const token = @import("token.zig");
const PrettyPrinter = @import("pretty_printer.zig").PrettyPrinter;
const Writer = std.Io.Writer;
const AllocatorError = std.mem.Allocator.Error;
const testing = std.testing;

/// The different types of statements in the AST.
pub const StatementTypes = enum { Assignment, Return, Expression, FunctionDeclaration, Do, While, Repeat, ForNumeric, ForGeneric, Break };

// Numbers can be integers or floats
pub const NumberType = enum { Integer, Float };

/// A tagged union representing any statement in the language.
/// Statements are the top-level constructs that make up a program.
pub const Statement = union(StatementTypes) {
    Assignment: *AssignmentStatement,
    Return: *ReturnStatement,
    Expression: *ExpressionStatement,
    FunctionDeclaration: *FunctionDeclaration,
    Do: *DoStatement,
    While: *WhileStatement,
    Repeat: *RepeatStatement,
    ForNumeric: *ForNumericStatement,
    ForGeneric: *ForGenericStatement,
    Break: *BreakStatement,

    /// Returns the literal text of the first token in this statement.
    /// Useful for debugging and error messages.
    pub fn tokenLiteral(self: *const Statement) []const u8 {
        switch (self.*) {
            .Assignment => |n| return n.tokenLiteral(),
            .Return => |n| return n.tokenLiteral(),
            .Expression => |n| return n.tokenLiteral(),
            .FunctionDeclaration => |n| return n.tokenLiteral(),
            .Do => |n| return n.tokenLiteral(),
            .While => |n| return n.tokenLiteral(),
            .Repeat => |n| return n.tokenLiteral(),
            .ForNumeric => |n| return n.tokenLiteral(),
            .ForGeneric => |n| return n.tokenLiteral(),
            .Break => |n| return n.tokenLiteral(),
        }
    }

    /// Writes the statement to the given writer in valid source code format.
    /// All statements are terminated with a semicolon.
    pub fn write(self: *const Statement, writer: *Writer) Writer.Error!void {
        switch (self.*) {
            .Assignment => |n| try n.write(writer),
            .Return => |n| try n.write(writer),
            .Expression => |n| try n.write(writer),
            .FunctionDeclaration => |n| try n.write(writer),
            .Do => |n| try n.write(writer),
            .While => |n| try n.write(writer),
            .Repeat => |n| try n.write(writer),
            .ForNumeric => |n| try n.write(writer),
            .ForGeneric => |n| try n.write(writer),
            .Break => |n| try n.write(writer),
        }
        _ = try writer.writeAll(";");
    }

    /// Pretty prints the statement
    pub fn pretty(self: *const Statement, pp: *PrettyPrinter) Writer.Error!void {
        try pp.write("");
        switch (self.*) {
            .Assignment => |n| try n.pretty(pp),
            .Return => |n| try n.pretty(pp),
            .Expression => |n| try n.pretty(pp),
            .FunctionDeclaration => |n| try n.pretty(pp),
            .Do => |n| try n.pretty(pp),
            .While => |n| try n.pretty(pp),
            .Repeat => |n| try n.pretty(pp),
            .ForNumeric => |n| try n.pretty(pp),
            .ForGeneric => |n| try n.pretty(pp),
            .Break => |n| try n.pretty(pp),
        }
    }
};

/// Represents a return statement that exits a function with an optional value.
/// Example: `return 42` or `return`
pub const ReturnStatement = struct {
    /// The 'return' keyword token
    token: token.Token,
    /// The optional expression to return (null for bare return statements)
    /// TODO: handle multiple return values
    expr: ?*Expression,

    /// Returns the literal text of the return keyword.
    pub fn tokenLiteral(self: *const ReturnStatement) []const u8 {
        return self.token.literal;
    }

    /// Writes the return statement to the given writer.
    /// Format: "return <expr>" or "return" if expr is null.
    pub fn write(self: *const ReturnStatement, writer: *Writer) Writer.Error!void {
        _ = try writer.writeAll("return ");
        if (self.expr != null) {
            try self.expr.?.write(writer);
        }
    }

    /// Pretty prints the return statement
    pub fn pretty(self: *const ReturnStatement, pp: *PrettyPrinter) Writer.Error!void {
        try pp.write("return ");
        if (self.expr != null) {
            try self.expr.?.pretty(pp);
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
    pub fn write(self: *const AssignmentStatement, writer: *Writer) Writer.Error!void {
        if (self.is_local) {
            _ = try writer.writeAll("local ");
        }
        try self.names.write(writer);
        _ = try writer.writeAll(" = ");
        try self.expr.write(writer);
    }

    /// Pretty prints the assignment statement
    pub fn pretty(self: *const AssignmentStatement, pp: *PrettyPrinter) Writer.Error!void {
        if (self.is_local) {
            try pp.writeInline("local ");
            try self.names.pretty(pp);
        } else {
            try self.names.pretty(pp);
        }
        try pp.writeInline(" = ");
        try self.expr.pretty(pp);
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
    pub fn write(self: *const NameList, writer: *Writer) Writer.Error!void {
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

    /// Pretty prints comma separated name list
    pub fn pretty(self: *const NameList, pp: *PrettyPrinter) Writer.Error!void {
        if (self.names.items.len <= 1) {
            try self.names.items[0].pretty(pp);
            return;
        }

        var i: usize = 0;
        while (i < self.names.items.len - 1) : (i += 1) {
            try self.names.items[i].pretty(pp);
            try pp.write(", ");
        }
        try self.names.items[i].pretty(pp);
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
    pub fn write(self: *const ExpressionStatement, writer: *Writer) Writer.Error!void {
        try self.expr.write(writer);
    }

    /// Pretty prints the expression statement
    pub fn pretty(self: *const ExpressionStatement, pp: *PrettyPrinter) Writer.Error!void {
        try self.expr.pretty(pp);
    }
};

/// Represents a do-end block statement.
/// Example: `do x = 5 end`
pub const DoStatement = struct {
    /// The 'do' keyword token
    token: token.Token,
    /// The block of statements to execute
    block: *Block,

    /// Returns the literal text of the 'do' keyword.
    pub fn tokenLiteral(self: *const DoStatement) []const u8 {
        return self.token.literal;
    }

    /// Writes the do statement to the given writer.
    /// Format: "do <block> end"
    pub fn write(self: *const DoStatement, writer: *Writer) Writer.Error!void {
        _ = try writer.writeAll("do ");
        try self.block.write(writer);
        _ = try writer.writeAll(" end");
    }

    /// Pretty prints the do statement
    pub fn pretty(self: *const DoStatement, pp: *PrettyPrinter) Writer.Error!void {
        try pp.write("do");
        try pp.nl();
        if (self.block.statements.len > 0) {
            pp.indent();
            for (self.block.statements) |stmt| {
                try stmt.pretty(pp);
                try pp.nl();
            }
            pp.dedent();
        }
        try pp.write("end");
    }
};

/// Represents a while loop statement.
/// Example: `while x > 0 do x = x - 1 end`
pub const WhileStatement = struct {
    /// The 'while' keyword token
    token: token.Token,
    /// The condition expression to evaluate
    condition: *Expression,
    /// The block of statements to execute while condition is true
    block: *Block,

    /// Returns the literal text of the 'while' keyword.
    pub fn tokenLiteral(self: *const WhileStatement) []const u8 {
        return self.token.literal;
    }

    /// Writes the while statement to the given writer.
    /// Format: "while <condition> do <block> end"
    pub fn write(self: *const WhileStatement, writer: *Writer) Writer.Error!void {
        _ = try writer.writeAll("while ");
        try self.condition.write(writer);
        _ = try writer.writeAll(" do ");
        try self.block.write(writer);
        _ = try writer.writeAll(" end");
    }

    /// Pretty prints the while statement
    pub fn pretty(self: *const WhileStatement, pp: *PrettyPrinter) Writer.Error!void {
        try pp.write("while ");
        try self.condition.pretty(pp);
        try pp.write(" do");
        try pp.nl();
        if (self.block.statements.len > 0) {
            pp.indent();
            for (self.block.statements) |stmt| {
                try stmt.pretty(pp);
                try pp.nl();
            }
            pp.dedent();
        }
        try pp.write("end");
    }
};

/// Represents a repeat-until loop statement.
/// Example: `repeat x = x - 1 until x <= 0`
pub const RepeatStatement = struct {
    /// The 'repeat' keyword token
    token: token.Token,
    /// The block of statements to execute
    block: *Block,
    /// The condition expression to evaluate after each iteration
    condition: *Expression,

    /// Returns the literal text of the 'repeat' keyword.
    pub fn tokenLiteral(self: *const RepeatStatement) []const u8 {
        return self.token.literal;
    }

    /// Writes the repeat statement to the given writer.
    /// Format: "repeat <block> until <condition>"
    pub fn write(self: *const RepeatStatement, writer: *Writer) Writer.Error!void {
        _ = try writer.writeAll("repeat ");
        try self.block.write(writer);
        _ = try writer.writeAll(" until ");
        try self.condition.write(writer);
    }

    /// Pretty prints the repeat statement
    pub fn pretty(self: *const RepeatStatement, pp: *PrettyPrinter) Writer.Error!void {
        try pp.write("repeat");
        try pp.nl();
        if (self.block.statements.len > 0) {
            pp.indent();
            for (self.block.statements) |stmt| {
                try stmt.pretty(pp);
                try pp.nl();
            }
            pp.dedent();
        }
        try pp.write("until ");
        try self.condition.pretty(pp);
    }
};

/// Represents a numeric for loop statement.
/// Example: `for i = 1, 10, 2 do print(i) end`
pub const ForNumericStatement = struct {
    /// The 'for' keyword token
    token: token.Token,
    /// The loop variable name
    var_name: *Identifier,
    /// The initial value expression
    start: *Expression,
    /// The final value expression
    end: *Expression,
    /// The optional step value expression (defaults to 1 if null)
    step: ?*Expression,
    /// The block of statements to execute
    block: *Block,

    /// Returns the literal text of the 'for' keyword.
    pub fn tokenLiteral(self: *const ForNumericStatement) []const u8 {
        return self.token.literal;
    }

    /// Writes the numeric for statement to the given writer.
    /// Format: "for <var> = <start>, <end>[, <step>] do <block> end"
    pub fn write(self: *const ForNumericStatement, writer: *Writer) Writer.Error!void {
        _ = try writer.writeAll("for ");
        try self.var_name.write(writer);
        _ = try writer.writeAll(" = ");
        try self.start.write(writer);
        _ = try writer.writeAll(", ");
        try self.end.write(writer);
        if (self.step != null) {
            _ = try writer.writeAll(", ");
            try self.step.?.write(writer);
        }
        _ = try writer.writeAll(" do ");
        try self.block.write(writer);
        _ = try writer.writeAll(" end");
    }

    /// Pretty prints the numeric for statement
    pub fn pretty(self: *const ForNumericStatement, pp: *PrettyPrinter) Writer.Error!void {
        try pp.write("for ");
        try self.var_name.pretty(pp);
        try pp.write(" = ");
        try self.start.pretty(pp);
        try pp.write(", ");
        try self.end.pretty(pp);
        if (self.step != null) {
            try pp.write(", ");
            try self.step.?.pretty(pp);
        }
        try pp.write(" do");
        try pp.nl();
        if (self.block.statements.len > 0) {
            pp.indent();
            for (self.block.statements) |stmt| {
                try stmt.pretty(pp);
                try pp.nl();
            }
            pp.dedent();
        }
        try pp.write("end");
    }
};

/// Represents a generic for loop statement (iterator-based).
/// Example: `for k, v in pairs(t) do print(k, v) end`
pub const ForGenericStatement = struct {
    /// The 'for' keyword token
    token: token.Token,
    /// The list of loop variable names
    names: *NameList,
    /// The list of iterator expressions
    expressions: std.ArrayList(*Expression),
    /// The block of statements to execute
    block: *Block,
    /// Allocator used for managing the expressions list
    allocator: std.mem.Allocator,

    /// Clear memory in the ForGenericStatement
    pub fn deinit(self: *ForGenericStatement) void {
        self.expressions.deinit(self.allocator);
    }

    /// Returns the literal text of the 'for' keyword.
    pub fn tokenLiteral(self: *const ForGenericStatement) []const u8 {
        return self.token.literal;
    }

    /// Writes the generic for statement to the given writer.
    /// Format: "for <names> in <expressions> do <block> end"
    pub fn write(self: *const ForGenericStatement, writer: *Writer) Writer.Error!void {
        _ = try writer.writeAll("for ");
        try self.names.write(writer);
        _ = try writer.writeAll(" in ");

        // Write comma-separated expression list
        if (self.expressions.items.len > 0) {
            var i: usize = 0;
            while (i < self.expressions.items.len - 1) : (i += 1) {
                try self.expressions.items[i].write(writer);
                _ = try writer.writeAll(", ");
            }
            try self.expressions.items[i].write(writer);
        }

        _ = try writer.writeAll(" do ");
        try self.block.write(writer);
        _ = try writer.writeAll(" end");
    }

    /// Pretty prints the generic for statement
    pub fn pretty(self: *const ForGenericStatement, pp: *PrettyPrinter) Writer.Error!void {
        try pp.write("for ");
        try self.names.pretty(pp);
        try pp.write(" in ");

        // Write comma-separated expression list
        if (self.expressions.items.len > 0) {
            var i: usize = 0;
            while (i < self.expressions.items.len - 1) : (i += 1) {
                try self.expressions.items[i].pretty(pp);
                try pp.write(", ");
            }
            try self.expressions.items[i].pretty(pp);
        }

        try pp.write(" do");
        try pp.nl();
        if (self.block.statements.len > 0) {
            pp.indent();
            for (self.block.statements) |stmt| {
                try stmt.pretty(pp);
                try pp.nl();
            }
            pp.dedent();
        }
        try pp.write("end");
    }
};

/// Represents a break statement.
/// Example: `break`
pub const BreakStatement = struct {
    /// The 'break' keyword token
    token: token.Token,

    /// Returns the literal text of the 'break' keyword.
    pub fn tokenLiteral(self: *const BreakStatement) []const u8 {
        return self.token.literal;
    }

    /// Writes the break statement to the given writer.
    /// Format: "break"
    pub fn write(_: *const BreakStatement, writer: *Writer) Writer.Error!void {
        _ = try writer.writeAll("break");
    }

    /// Pretty prints the break statement
    pub fn pretty(_: *const BreakStatement, pp: *PrettyPrinter) Writer.Error!void {
        try pp.write("break");
    }
};

/// The different types of expressions in the AST.
pub const ExpressionTypes = enum { Identifier, Number, String, Prefix, Infix, Conditional, Boolean, Nil, Varargs, FunctionDef, TableConstructor, Index, Member, FunctionCall, MethodCall };

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
    TableConstructor: *TableConstructor,
    Index: *IndexExpression,
    Member: *MemberExpression,
    FunctionCall: *FunctionCallExpression,
    MethodCall: *MethodCallExpression,

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
            .TableConstructor => |n| n.tokenLiteral(),
            .Index => |n| n.tokenLiteral(),
            .Member => |n| n.tokenLiteral(),
            .FunctionCall => |n| n.tokenLiteral(),
            .MethodCall => |n| n.tokenLiteral(),
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
            .TableConstructor => |n| try n.write(writer),
            .Index => |n| try n.write(writer),
            .Member => |n| try n.write(writer),
            .FunctionCall => |n| try n.write(writer),
            .MethodCall => |n| try n.write(writer),
        }
    }

    /// Pretty prints the expression
    pub fn pretty(self: *const Expression, pp: *PrettyPrinter) Writer.Error!void {
        switch (self.*) {
            .Identifier => |n| try n.pretty(pp),
            .Number => |n| try n.pretty(pp),
            .String => |n| try n.pretty(pp),
            .Boolean => |n| try n.pretty(pp),
            .Prefix => |n| try n.pretty(pp),
            .Infix => |n| try n.pretty(pp),
            .Conditional => |n| try n.pretty(pp),
            .Nil => |n| try n.pretty(pp),
            .Varargs => |n| try n.pretty(pp),
            .FunctionDef => |n| try n.pretty(pp),
            .TableConstructor => |n| try n.pretty(pp),
            .Index => |n| try n.pretty(pp),
            .Member => |n| try n.pretty(pp),
            .FunctionCall => |n| try n.pretty(pp),
            .MethodCall => |n| try n.pretty(pp),
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
    pub fn write(self: *const Identifier, writer: *Writer) Writer.Error!void {
        _ = try writer.writeAll(self.value);
    }

    /// Pretty prints the identifier
    pub fn pretty(self: *const Identifier, pp: *PrettyPrinter) Writer.Error!void {
        // instead of write() and print() to keep expressions on single lines without adding indentation
        try pp.writeInline(self.value);
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
    pub fn write(self: *InfixExpression, writer: *Writer) Writer.Error!void {
        _ = try writer.writeAll("(");
        try self.left.write(writer);
        _ = try writer.writeAll(" ");
        _ = try writer.writeAll(self.operator);
        _ = try writer.writeAll(" ");
        try self.right.write(writer);
        _ = try writer.writeAll(")");
    }

    /// Pretty prints the infix expression
    pub fn pretty(self: *const InfixExpression, pp: *PrettyPrinter) Writer.Error!void {
        try pp.writeInline("(");
        try self.left.pretty(pp);
        try pp.writeInline(" ");
        try pp.writeInline(self.operator);
        try pp.writeInline(" ");
        try self.right.pretty(pp);
        try pp.writeInline(")");
    }
};

/// Represents an index access expression.
/// Example: `t[1]`, `arr[i + 1]`, `matrix[x][y]`
pub const IndexExpression = struct {
    /// The opening bracket token
    token: token.Token,
    /// The expression being indexed (left side)
    object: *Expression,
    /// The index expression (inside brackets)
    index: *Expression,

    /// Returns the literal text of the opening bracket token.
    pub fn tokenLiteral(self: *const IndexExpression) []const u8 {
        return self.token.literal;
    }

    /// Writes the index expression to the given writer.
    /// Format: "<object>[<index>]"
    pub fn write(self: *const IndexExpression, writer: *Writer) Writer.Error!void {
        try self.object.write(writer);
        _ = try writer.writeAll("[");
        try self.index.write(writer);
        _ = try writer.writeAll("]");
    }

    /// Pretty prints the index expression
    pub fn pretty(self: *const IndexExpression, pp: *PrettyPrinter) Writer.Error!void {
        try self.object.pretty(pp);
        try pp.write("[");
        try self.index.pretty(pp);
        try pp.write("]");
    }
};

/// Represents a member access expression.
/// Example: `t.x`, `obj.field`, `foo.bar.baz`
pub const MemberExpression = struct {
    /// The dot token
    token: token.Token,
    /// The expression being accessed (left side)
    object: *Expression,
    /// The field name being accessed
    field: *Identifier,

    /// Returns the literal text of the dot token.
    pub fn tokenLiteral(self: *const MemberExpression) []const u8 {
        return self.token.literal;
    }

    /// Writes the member expression to the given writer.
    /// Format: "<object>.<field>"
    pub fn write(self: *const MemberExpression, writer: *Writer) Writer.Error!void {
        try self.object.write(writer);
        _ = try writer.writeAll(".");
        try self.field.write(writer);
    }

    /// Pretty prints the member expression
    pub fn pretty(self: *const MemberExpression, pp: *PrettyPrinter) Writer.Error!void {
        try self.object.pretty(pp);
        try pp.write(".");
        try self.field.pretty(pp);
    }
};

/// Represents the different types of function call arguments
pub const CallArgsType = enum { ExpressionList, TableConstructor, StringLiteral };

/// Represents the arguments to a function call
/// Can be: (explist), {table}, or "string"
pub const CallArgs = union(CallArgsType) {
    ExpressionList: std.ArrayList(*Expression),
    TableConstructor: *TableConstructor,
    StringLiteral: *StringLiteral,

    /// Writes the call arguments to the given writer
    pub fn write(self: *const CallArgs, writer: *Writer) Writer.Error!void {
        switch (self.*) {
            .ExpressionList => |exprs| {
                _ = try writer.writeAll("(");
                if (exprs.items.len > 0) {
                    var i: usize = 0;
                    while (i < exprs.items.len - 1) : (i += 1) {
                        try exprs.items[i].write(writer);
                        _ = try writer.writeAll(", ");
                    }
                    try exprs.items[i].write(writer);
                }
                _ = try writer.writeAll(")");
            },
            .TableConstructor => |table| try table.write(writer),
            .StringLiteral => |str| try str.write(writer),
        }
    }

    /// Pretty prints the call arguments
    pub fn pretty(self: *const CallArgs, pp: *PrettyPrinter) Writer.Error!void {
        switch (self.*) {
            .ExpressionList => |exprs| {
                try pp.write("(");
                if (exprs.items.len > 0) {
                    var i: usize = 0;
                    while (i < exprs.items.len - 1) : (i += 1) {
                        try exprs.items[i].pretty(pp);
                        try pp.writeInline(", ");
                    }
                    try exprs.items[i].pretty(pp);
                }
                try pp.writeInline(")");
            },
            .TableConstructor => |table| try table.pretty(pp),
            .StringLiteral => |str| try str.pretty(pp),
        }
    }
};

/// Represents a function call expression.
/// Example: `foo()`, `print("hello")`, `func{x = 1}`
pub const FunctionCallExpression = struct {
    /// The opening paren, brace, or string token
    token: token.Token,
    /// The function expression being called
    function: *Expression,
    /// The arguments
    args: CallArgs,

    /// Returns the literal text of the token
    pub fn tokenLiteral(self: *const FunctionCallExpression) []const u8 {
        return self.token.literal;
    }

    /// Writes the function call to the given writer.
    /// Format: "<function><args>"
    pub fn write(self: *const FunctionCallExpression, writer: *Writer) Writer.Error!void {
        try self.function.write(writer);
        try self.args.write(writer);
    }

    /// Pretty prints the function call
    pub fn pretty(self: *const FunctionCallExpression, pp: *PrettyPrinter) Writer.Error!void {
        try self.function.pretty(pp);
        try self.args.pretty(pp);
    }
};

/// Represents a method call expression.
/// Example: `obj:method()`, `t:fn"string"`, `x:y{1, 2}`
pub const MethodCallExpression = struct {
    /// The colon token
    token: token.Token,
    /// The object expression
    object: *Expression,
    /// The method name
    method: *Identifier,
    /// The arguments
    args: CallArgs,

    /// Returns the literal text of the colon token
    pub fn tokenLiteral(self: *const MethodCallExpression) []const u8 {
        return self.token.literal;
    }

    /// Writes the method call to the given writer.
    /// Format: "<object>:<method><args>"
    pub fn write(self: *const MethodCallExpression, writer: *Writer) Writer.Error!void {
        try self.object.write(writer);
        _ = try writer.writeAll(":");
        try self.method.write(writer);
        try self.args.write(writer);
    }

    /// Pretty prints the method call
    pub fn pretty(self: *const MethodCallExpression, pp: *PrettyPrinter) Writer.Error!void {
        try self.object.pretty(pp);
        try pp.writeInline(":");
        try self.method.pretty(pp);
        try self.args.pretty(pp);
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
    pub fn write(self: *PrefixExpression, writer: *Writer) Writer.Error!void {
        _ = try writer.writeAll("(");
        _ = try writer.writeAll(self.operator);
        // Only add space for word operators (not, and, or)
        if (std.mem.eql(u8, self.operator, "not")) {
            _ = try writer.writeAll(" ");
        }
        try self.expression.write(writer);
        _ = try writer.writeAll(")");
    }

    /// Pretty prints the prefix expression
    pub fn pretty(self: *const PrefixExpression, pp: *PrettyPrinter) Writer.Error!void {
        try pp.write("(");
        try pp.write(self.operator);
        // Only add space for word operators (not, and, or)
        if (std.mem.eql(u8, self.operator, "not")) {
            try pp.write(" ");
        }
        try self.expression.pretty(pp);
        try pp.write(")");
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
    pub fn write(self: *ConditionalExpression, writer: *Writer) Writer.Error!void {
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

    /// Pretty prints the conditional expression
    pub fn pretty(self: *const ConditionalExpression, pp: *PrettyPrinter) Writer.Error!void {
        try pp.write("if ");
        try self.condition.pretty(pp);
        try pp.write(" then");
        try pp.nl();
        if (self.then_block.statements.len > 0) {
            pp.indent();
            for (self.then_block.statements) |stmt| {
                try stmt.pretty(pp);
                try pp.nl();
            }
            pp.dedent();
        }
        if (self.else_block != null) {
            try pp.write("else");
            try pp.nl();
            if (self.else_block.?.statements.len > 0) {
                pp.indent();
                for (self.else_block.?.statements) |stmt| {
                    try stmt.pretty(pp);
                    try pp.nl();
                }
                pp.dedent();
            }
        }
        try pp.write("end");
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

    pub fn write(self: *const NumberLiteral, writer: *Writer) Writer.Error!void {
        switch (self.value) {
            .Integer => |i| {
                try writer.print("{d}", .{i});
            },
            .Float => |f| {
                try writer.print("{d}", .{f});
            },
        }
    }

    /// Pretty prints the number literal
    pub fn pretty(self: *const NumberLiteral, pp: *PrettyPrinter) Writer.Error!void {
        switch (self.value) {
            .Integer => |i| {
                try pp.printInline("{d}", .{i});
            },
            .Float => |f| {
                try pp.printInline("{d}", .{f});
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
    pub fn write(self: *const StringLiteral, writer: *Writer) Writer.Error!void {
        _ = try writer.print("\"{s}\"", .{self.value});
    }

    /// Pretty prints the string literal
    pub fn pretty(self: *const StringLiteral, pp: *PrettyPrinter) Writer.Error!void {
        try pp.print("\"{s}\"", .{self.value});
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
    pub fn write(self: *const BooleanLiteral, writer: *Writer) Writer.Error!void {
        if (self.value) {
            try writer.print("true", .{});
        } else {
            try writer.print("false", .{});
        }
    }

    /// Pretty prints the boolean literal
    pub fn pretty(self: *const BooleanLiteral, pp: *PrettyPrinter) Writer.Error!void {
        if (self.value) {
            try pp.write("true");
        } else {
            try pp.write("false");
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
    pub fn write(_: *const Nil, writer: *Writer) Writer.Error!void {
        try writer.print("nil", .{});
    }

    /// Pretty prints the nil literal
    pub fn pretty(_: *const Nil, pp: *PrettyPrinter) Writer.Error!void {
        try pp.write("nil");
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
    pub fn write(_: *const Varargs, writer: *Writer) Writer.Error!void {
        try writer.print("...", .{});
    }

    /// Pretty prints the varargs
    pub fn pretty(_: *const Varargs, pp: *PrettyPrinter) Writer.Error!void {
        try pp.print("...", .{});
    }
};

/// The different types of table fields
pub const FieldType = enum { ArrayStyle, RecordStyle, ComputedKey };

/// Represents a single field in a table constructor
/// Three variants based on Lua syntax:
/// - ArrayStyle: just an expression (e.g., `42` in `{42, 43}`)
/// - RecordStyle: name = expr (e.g., `x = 10` in `{x = 10}`)
/// - ComputedKey: [expr] = expr (e.g., `[key] = val` in `{[key] = val}`)
pub const Field = union(FieldType) {
    ArrayStyle: *Expression,
    RecordStyle: struct {
        name: *Identifier,
        value: *Expression,
    },
    ComputedKey: struct {
        key: *Expression,
        value: *Expression,
    },

    /// Returns the literal text of the first token in this field
    pub fn tokenLiteral(self: *const Field) []const u8 {
        return switch (self.*) {
            .ArrayStyle => |expr| expr.tokenLiteral(),
            .RecordStyle => |record| record.name.tokenLiteral(),
            .ComputedKey => |computed| computed.key.tokenLiteral(),
        };
    }

    /// Writes the field to the given writer
    pub fn write(self: *const Field, writer: *Writer) Writer.Error!void {
        switch (self.*) {
            .ArrayStyle => |expr| try expr.write(writer),
            .RecordStyle => |record| {
                try record.name.write(writer);
                _ = try writer.writeAll(" = ");
                try record.value.write(writer);
            },
            .ComputedKey => |computed| {
                _ = try writer.writeAll("[");
                try computed.key.write(writer);
                _ = try writer.writeAll("] = ");
                try computed.value.write(writer);
            },
        }
    }

    /// Pretty prints the field
    pub fn pretty(self: *const Field, pp: *PrettyPrinter) Writer.Error!void {
        switch (self.*) {
            .ArrayStyle => |expr| try expr.pretty(pp),
            .RecordStyle => |record| {
                try record.name.pretty(pp);
                try pp.write(" = ");
                try record.value.pretty(pp);
            },
            .ComputedKey => |computed| {
                try pp.write("[");
                try computed.key.pretty(pp);
                try pp.write("] = ");
                try computed.value.pretty(pp);
            },
        }
    }
};

/// Represents a table constructor literal
/// Example: `{}`, `{1, 2, 3}`, `{x = 10, y = 20}`, `{[key] = value}`
pub const TableConstructor = struct {
    /// The opening brace token
    token: token.Token,
    /// List of fields in the table
    fields: std.ArrayList(*Field),
    /// Allocator used for managing the fields list
    allocator: std.mem.Allocator,

    /// Clear memory in the TableConstructor
    pub fn deinit(self: *TableConstructor) void {
        self.fields.deinit(self.allocator);
    }

    /// Returns the literal text of the opening brace token
    pub fn tokenLiteral(self: *const TableConstructor) []const u8 {
        return self.token.literal;
    }

    /// Writes the table constructor to the given writer
    /// Format: "{[field][, field]*}"
    pub fn write(self: *const TableConstructor, writer: *Writer) Writer.Error!void {
        _ = try writer.writeAll("{");

        if (self.fields.items.len > 0) {
            var i: usize = 0;
            while (i < self.fields.items.len - 1) : (i += 1) {
                try self.fields.items[i].write(writer);
                _ = try writer.writeAll(", ");
            }
            try self.fields.items[i].write(writer);
        }

        _ = try writer.writeAll("}");
    }

    // Pretty prints the table literal
    pub fn pretty(self: *const TableConstructor, pp: *PrettyPrinter) Writer.Error!void {
        try pp.write("{");

        if (self.fields.items.len > 0) {
            try pp.nl();
            var i: usize = 0;
            pp.indent();
            while (i < self.fields.items.len - 1) : (i += 1) {
                try pp.write("");
                try self.fields.items[i].pretty(pp);
                try pp.writeInline(", ");
                try pp.nl();
            }
            try pp.write("");
            try self.fields.items[i].pretty(pp);
            try pp.nl();
            pp.dedent();
        }

        try pp.write("}");
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
    pub fn write(self: *const ParamList, writer: *Writer) Writer.Error!void {
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

    /// Pretty prints the param list
    pub fn pretty(self: *const ParamList, pp: *PrettyPrinter) Writer.Error!void {
        if (self.names.items.len == 0 and self.has_varargs) {
            try pp.write("...");
            return;
        }

        var i: usize = 0;
        while (i < self.names.items.len) : (i += 1) {
            try self.names.items[i].pretty(pp);
            if (i < self.names.items.len - 1 or self.has_varargs) {
                try pp.write(", ");
            }
        }

        if (self.has_varargs) {
            try pp.write("...");
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
    pub fn write(self: *const FunctionBody, writer: *Writer) Writer.Error!void {
        try writer.writeAll("(");
        if (self.params != null) {
            try self.params.?.write(writer);
        }
        try writer.writeAll(")");
        if (self.block.statements.len > 0) {
            // FIXME
            try writer.writeAll(" ");
        }
        try self.block.write(writer);
        try writer.writeAll(" end");
    }

    /// Pretty prints the function body
    pub fn pretty(self: *const FunctionBody, pp: *PrettyPrinter) Writer.Error!void {
        try pp.write("(");
        if (self.params != null) {
            try self.params.?.pretty(pp);
        }
        try pp.write(")");
        try pp.nl();
        if (self.block.statements.len > 0) {
            pp.indent();
            for (self.block.statements) |stmt| {
                try stmt.pretty(pp);
                try pp.nl();
            }
            pp.dedent();
        }
        try pp.write("end");
        try pp.nl();
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
    pub fn write(self: *const FunctionDefExpression, writer: *Writer) Writer.Error!void {
        try writer.writeAll("function");
        try self.body.write(writer);
    }

    /// Pretty prints the function def exp
    pub fn pretty(self: *const FunctionDefExpression, pp: *PrettyPrinter) Writer.Error!void {
        try pp.write("function");
        try self.body.pretty(pp);
    }
};

/// Represents a function declaration
/// Different from the expression variant because its a statement and has a name in the declaration
/// Contains an optional `local` keyword
pub const FunctionDeclaration = struct {
    /// 'function' keyword token
    token: token.Token,
    /// Whether or not `local` was used in this declaration
    is_local: bool,
    /// Function name (arguably this should just be a string)
    name: *Identifier,
    /// Body
    body: *FunctionBody,

    /// Returns the literal text of the 'function' keyword
    pub fn tokenLiteral(self: *const FunctionDeclaration) []const u8 {
        return self.token.literal;
    }

    /// Writes the function definition to writer
    pub fn write(self: *const FunctionDeclaration, writer: *Writer) Writer.Error!void {
        if (self.is_local) {
            try writer.writeAll("local ");
        }
        try writer.print("function {s}", .{self.name.value});
        try self.body.write(writer);
    }

    /// Pretty print the function decl
    pub fn pretty(self: *const FunctionDeclaration, pp: *PrettyPrinter) Writer.Error!void {
        if (self.is_local) {
            try pp.write("local ");
        }
        try pp.print("function {s}", .{self.name.value});
        try self.body.pretty(pp);
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
    pub fn write(self: *const Program, writer: *Writer) std.Io.Writer.Error!void {
        if (self.statements.len == 1) {
            try self.statements[0].write(writer);
        } else {
            for (self.statements) |stmt| {
                try stmt.write(writer);
                try writer.writeAll("\n");
            }
        }
    }

    /// Pretty print the program
    pub fn pretty(self: *const Program, pp: *PrettyPrinter) Writer.Error!void {
        if (self.statements.len == 1) {
            try self.statements[0].pretty(pp);
        } else {
            for (self.statements) |stmt| {
                try stmt.pretty(pp);
                try pp.nl();
            }
        }
    }
};
