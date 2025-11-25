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

        // Create blocks ahead of time for naming
        const err_block = try c.functions.createBlock();
        const post_err_block = try c.functions.createBlock();

        // Extract type information
        const lhs_type = try boxed.emitType(c, lhs_temp);
        const rhs_type = try boxed.emitType(c, rhs_temp);

        // Compare types
        // NOTE: For now, only operations with the same type are valid
        const types_equal = try c.emitAssignment(lhs_type.datatype, "ceql %{s}, %{s}", .{ lhs_type.name, rhs_type.name });

        // JNZ to post-error block
        try c.emitRaw("jnz %{s}, @{s}, @{s}", .{ types_equal.name, post_err_block.label, err_block.label });

        // Write error and die
        try c.pushBlock(err_block);
        try c.emitPanic("Encountered type mismatch error");

        // Start block for post-error
        try c.pushBlock(post_err_block);

        // Unbox operands
        const lhs_unboxed = try c.emitUnboxing(lhs_temp);
        const rhs_unboxed = try c.emitUnboxing(rhs_temp);

        // Get operator from map
        const operator_instruction = number_operator_map.get(self.operator);
        const comparison_instruction = comparison_map.get(self.operator);
        if (operator_instruction == null and comparison_instruction == null) {
            return c.withError("Unexpected operator in infix expression: {s}", .{self.operator});
        }

        // Format and add instruction
        if (comparison_instruction != null) {
            // handle comparison instruction by appending result type to instr name
            const result = try c.emitAssignment(.l, "{s}{s} %{s}, %{s}", .{ comparison_instruction.?, "d", lhs_unboxed.name, rhs_unboxed.name });
            const boxed_result = c.emitBoxing(.bool, result);
            return boxed_result;
        } else {
            // must be arithmetic / logical operator
            const result = try c.emitAssignment(.l, "{s} %{s}, %{s}", .{ operator_instruction.?, lhs_unboxed.name, rhs_unboxed.name });
            const boxed_result = try c.emitRuntimeBoxing(lhs_temp, result);
            return boxed_result;
        }
    }
};
