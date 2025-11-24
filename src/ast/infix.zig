const std = @import("std");
const token = @import("../token.zig");
const boxed = @import("../qbe/nan_box.zig");
const Expression = @import("../ast.zig").Expression;
const Compiler = @import("../qbe/compiler.zig").QBECompiler;
const PrettyPrinter = @import("../pretty_printer.zig").PrettyPrinter;
const Temporary = @import("../qbe/symbol_table.zig").Temporary;
const Writer = std.Io.Writer;

const number_operator_map = std.StaticStringMap([]const u8).initComptime(.{
    // Arithmetic operators
    .{ "+", "add" },
    .{ "-", "sub" },
    .{ "*", "mul" },
    .{ "/", "div" },
    .{ "%", "rem" },
    // Bitwise operators
    .{ "&", "and" },
    .{ "|", "or" },
    .{ "~", "xor" },
    .{ "<<", "shl" },
    .{ ">>", "sar" }, // Using arithmetic right shift (sar) for signed integers
});

const string_operator_map = std.StaticStringMap(void).initComptime(.{
    .{ "..", "add" }, // TODO: this shouldn't actually add
});

const comparison_map = std.StaticStringMap([]const u8).initComptime(.{
    // Comparison operators
    .{ "==", "ceq" },
    .{ "~=", "cne" },
    .{ "<", "cslt" },
    .{ ">", "csgt" },
    .{ "<=", "csle" },
    .{ ">=", "csge" },
});

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
        try pp.write("(");
        try self.left.pretty(pp);
        try pp.write(" ");
        try pp.write(self.operator);
        try pp.write(" ");
        try self.right.pretty(pp);
        try pp.write(")");
    }

    /// Compile lhs and rhs, use operator, and assign
    pub fn compile(self: *const InfixExpression, c: *Compiler) !*Temporary {
        // Compile left and right
        const lhs_temp = try self.left.compile(c);
        const rhs_temp = try self.right.compile(c);

        // Extract type information
        // const lhs_type = try boxed.emitType(c, lhs_temp);
        // const rhs_type = try boxed.emitType(c, rhs_temp);

        // Compare types

        // JNZ to post-error block
        // Write error
        // Die

        // Start block for post-error

        // Complete operation

        // Store back in nan box and put in temporary

        // Get operator from map
        const operator_instruction = number_operator_map.get(self.operator);
        const comparison_instruction = comparison_map.get(self.operator);
        if (operator_instruction == null and comparison_instruction == null) {
            return c.withError("Unexpected operator in infix expression: {s}", .{self.operator});
        }

        // Compute variable strings and defer free for expressions
        const lhs_var_name = try lhs_temp.print(c.alloc);
        const rhs_var_name = try rhs_temp.print(c.alloc);
        defer c.alloc.free(lhs_var_name);
        defer c.alloc.free(rhs_var_name);

        // Format and add instruction
        if (comparison_instruction != null) {
            // handle comparison instruction by appending result type to instr name
            return try c.emitAssignment(.d, "{s}{s} {s}, {s}", .{ comparison_instruction.?, "d", lhs_var_name, rhs_var_name });
        } else {
            // must be arithmetic / logical operator
            return try c.emitAssignment(.d, "{s} {s}, {s}", .{ operator_instruction.?, lhs_var_name, rhs_var_name });
        }
    }
};
