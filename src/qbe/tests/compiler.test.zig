const std = @import("std");
const alloc = std.testing.allocator;
const Lexer = @import("../../lexer.zig").Lexer;
const Parser = @import("../../parser.zig").Parser;
const QBECompiler = @import("../compiler.zig").QBECompiler;
const Type = @import("../function.zig").Type;

/// Helper function to compile source code to QBE IR
/// Caller must free the returned string with allocator
fn compileToQBE(source: []const u8) ![]const u8 {
    // Create and run lexer
    var lexer = try Lexer.init(alloc, source);
    defer lexer.deinit();

    // Create and run parser
    var parser = try Parser.init(alloc, &lexer);
    defer parser.deinit();

    const program = try parser.parseProgram();

    // Create a compiler and compile
    var compiler = try QBECompiler.init(alloc);
    defer compiler.deinit();

    try program.compile(&compiler);

    // Output the QBE IR to a writer
    var writer = std.Io.Writer.Allocating.init(alloc);
    defer writer.deinit();

    try compiler.emit(&writer.writer);

    // Return owned copy of the output
    return try alloc.dupe(u8, writer.written());
}

/// Helper function to assert that IR output contains expected substrings
fn expectIRContains(ir: []const u8, expected: []const []const u8) !void {
    for (expected) |substring| {
        std.testing.expect(std.mem.indexOf(u8, ir, substring) != null) catch |err| {
            std.debug.print("Expected {s} when comparing ir, instead found: {s}", .{ substring, ir });
            return err;
        };
    }
}

test "compile simple puts statement" {
    const source = "$puts \"hello world\"";
    const ir = try compileToQBE(source);
    defer alloc.free(ir);

    // Verify the output contains expected components
    try expectIRContains(ir, &.{
        "data $str_0 = { b \"hello world\", b 0 }",
        "export function w $main()",
        "@start",
        "call $puts(l $str_0)",
        "ret",
    });
}

test "add instruction helper" {
    var writer = std.Io.Writer.Allocating.init(alloc);
    defer writer.deinit();
    var compiler = try QBECompiler.init(alloc);
    defer compiler.deinit();

    const temp = try compiler.addInstruction(.function, .w, "call $puts(w %str_0)");
    try compiler.emit(&writer.writer);

    try std.testing.expect(std.mem.indexOf(u8, writer.written(), "%var0 =w call $puts(w %str_0)") != null);
    try std.testing.expectEqual(temp.sigil, .function);
    try std.testing.expectEqualStrings(temp.name, "var0");
}

test "compile integer literals" {
    const source =
        \\0
        \\42
        \\9999999
    ;
    const ir = try compileToQBE(source);
    defer alloc.free(ir);

    try expectIRContains(ir, &.{
        "export function w $main()",
        "@start",
        "%var0 =l copy 0",
        "%var1 =l copy 42",
        "%var2 =l copy 9999999",
        "ret",
    });
}

test "compile float literals" {
    const source =
        \\3.14
        \\0.0
        \\123.456
    ;
    const ir = try compileToQBE(source);
    defer alloc.free(ir);

    try expectIRContains(ir, &.{
        "export function w $main()",
        "@start",
        "%var0 =d copy d_3.14",
        "%var1 =d copy d_0",
        "%var2 =d copy d_123.456",
        "ret",
    });
}

test "compile string literals" {
    const source =
        \\"hello world"
    ;

    const ir = try compileToQBE(source);
    defer alloc.free(ir);

    try expectIRContains(ir, &.{
        "data $str_0 = { b \"hello world\", b 0 }",
        "export function w $main()",
        "@start",
        "%var0 =l copy $str_0",
        "ret",
    });
}

test "compile builtin call with expression list" {
    const source =
        \\$printf("%s %ld", "hello", 2)
    ;

    const ir = try compileToQBE(source);
    defer alloc.free(ir);

    try expectIRContains(ir, &.{
        "data $str_0 = { b \"%s %ld\", b 0 }",
        "data $str_1 = { b \"hello\", b 0 }",
        "export function w $main()",
        "@start",
        "%var0 =l copy $str_0",
        "%var1 =l copy $str_1",
        "%var2 =l copy 2",
        "%var3 =w call $printf(l %var0, ..., l %var1, l %var2)",
        "ret",
    });
}

test "compile infix expressions" {
    const TestCase = struct {
        source: []const u8,
        lhs: []const u8,
        rhs: []const u8,
        instruction: []const u8,
        type: Type,
    };

    const test_cases = [_]TestCase{
        // Arithmetic operators
        .{
            .type = .l,
            .source = "2 + 5",
            .lhs = "2",
            .rhs = "5",
            .instruction = "add",
        },
        .{ .type = .l, .source = "10 - 3", .lhs = "10", .rhs = "3", .instruction = "sub" },
        .{ .type = .l, .source = "4 * 7", .lhs = "4", .rhs = "7", .instruction = "mul" },
        .{ .type = .l, .source = "20 / 5", .lhs = "20", .rhs = "5", .instruction = "div" },
        .{ .type = .l, .source = "17 % 5", .lhs = "17", .rhs = "5", .instruction = "rem" },

        // Bitwise operators
        .{ .type = .l, .source = "12 & 7", .lhs = "12", .rhs = "7", .instruction = "and" },
        .{ .type = .l, .source = "8 | 4", .lhs = "8", .rhs = "4", .instruction = "or" },
        .{ .type = .l, .source = "15 ~ 3", .lhs = "15", .rhs = "3", .instruction = "xor" },
        .{ .type = .l, .source = "5 << 2", .lhs = "5", .rhs = "2", .instruction = "shl" },
        .{ .type = .l, .source = "20 >> 2", .lhs = "20", .rhs = "2", .instruction = "sar" },

        // Comparison operators
        .{ .type = .l, .source = "5 == 5", .lhs = "5", .rhs = "5", .instruction = "ceql" },
        .{ .type = .d, .source = "5.5 == 5.5", .lhs = "5.5", .rhs = "5.5", .instruction = "ceqd" },
        .{ .type = .l, .source = "5 ~= 3", .lhs = "5", .rhs = "3", .instruction = "cnel" },
        .{ .type = .l, .source = "3 < 7", .lhs = "3", .rhs = "7", .instruction = "csltl" },
        .{ .type = .l, .source = "10 > 5", .lhs = "10", .rhs = "5", .instruction = "csgtl" },
        .{ .type = .l, .source = "5 <= 5", .lhs = "5", .rhs = "5", .instruction = "cslel" },
        .{ .type = .l, .source = "7 >= 3", .lhs = "7", .rhs = "3", .instruction = "csgel" },
    };

    for (test_cases) |tc| {
        const ir = try compileToQBE(tc.source);
        defer alloc.free(ir);

        const expected_lhs = try std.fmt.allocPrint(alloc, "%var0 ={s} copy {s}{s}", .{ @tagName(tc.type), if (tc.type == .d) "d_" else "", tc.lhs });
        defer alloc.free(expected_lhs);
        const expected_rhs = try std.fmt.allocPrint(alloc, "%var1 ={s} copy {s}{s}", .{ @tagName(tc.type), if (tc.type == .d) "d_" else "", tc.rhs });
        defer alloc.free(expected_rhs);
        const expected_instr = try std.fmt.allocPrint(alloc, "%var2 ={s} {s} %var0, %var1", .{ @tagName(tc.type), tc.instruction });
        defer alloc.free(expected_instr);

        try expectIRContains(ir, &.{ expected_lhs, expected_rhs, expected_instr });
    }

    // Test nested expressions separately
    {
        const source = "(2 + 3) * 4";
        const ir = try compileToQBE(source);
        defer alloc.free(ir);
        try expectIRContains(ir, &.{
            "%var0 =l copy 2",
            "%var1 =l copy 3",
            "%var2 =l add %var0, %var1",
            "%var3 =l copy 4",
            "%var4 =l mul %var2, %var3",
        });
    }
}
