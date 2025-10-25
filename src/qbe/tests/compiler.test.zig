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

test "create compiler" {
    var qbe = try QBECompiler.init(alloc);
    defer qbe.deinit();
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

test "compile prefix expressions" {
    const TestCase = struct {
        source: []const u8,
        rhs: []const u8,
        instruction: []const u8,
    };

    const test_cases = [_]TestCase{
        .{ .source = "-3", .rhs = "3", .instruction = "=l neg %var0" },
        .{ .source = "not 1", .rhs = "7", .instruction = "=l xor %var0, 1" },
        .{ .source = "~10", .rhs = "5", .instruction = "=l xor %var0, -1" },
    };

    for (test_cases) |tc| {
        const ir = try compileToQBE(tc.source);
        defer alloc.free(ir);

        try expectIRContains(ir, &.{tc.instruction});
    }
}

// test "compile assignment statement and identifier access" {
//     const source =
//         \\local x = 2
//         \\local y = 4
//         \\2 + x
//         \\y
//     ;

//     const ir = try compileToQBE(source);
//     defer alloc.free(ir);

//     try expectIRContains(ir, &.{ "%var0 =l copy 2", "%var1 =l copy %var0", "%var2 =l copy 4", "%var3 =l copy %var2", "%var4 =l copy 2", "%var5 =l add %var4, %var1" });
// }

// test "compile conditional expressions" {
//     const TestCase = struct {
//         name: []const u8,
//         source: []const u8,
//         expected: []const []const u8,
//     };

//     const test_cases = [_]TestCase{
//         // If/then/else/end with truthy condition
//         .{
//             .name = "if/then/else/end with truthy condition",
//             .source =
//             \\local x = 1
//             \\if (x) then
//             \\  $puts("then block")
//             \\else
//             \\  $puts("else block")
//             \\end
//             ,
//             .expected = &.{
//                 "%var0 =l copy 1",
//                 "%var1 =l copy %var0",
//                 "jnz %var1, @block0, @block1",
//                 "@block0",
//                 "%var2 =l copy $str_0",
//                 "%var3 =w call $puts(l %var2)",
//                 "jmp @block2",
//                 "@block1",
//                 "%var4 =l copy $str_1",
//                 "%var5 =w call $puts(l %var4)",
//                 "@block2",
//             },
//         },
//         // If/then/else/end with falsy condition
//         .{
//             .name = "if/then/else/end with falsy condition",
//             .source =
//             \\local y = 0
//             \\if (y) then
//             \\  $puts("should not run")
//             \\else
//             \\  $puts("should run")
//             \\end
//             ,
//             .expected = &.{
//                 "%var0 =l copy 0",
//                 "%var1 =l copy %var0",
//                 "jnz %var1, @block0, @block1",
//                 "@block0",
//                 "@block1",
//                 "@block2",
//             },
//         },
//         // If/then/end without else block
//         .{
//             .name = "if/then/end without else block",
//             .source =
//             \\local x = 1
//             \\if (x) then
//             \\  $puts("only then")
//             \\end
//             ,
//             .expected = &.{
//                 "%var0 =l copy 1",
//                 "%var1 =l copy %var0",
//                 "jnz %var1, @block0, @block1",
//                 "@block0",
//                 "%var2 =l copy $str_0",
//                 "%var3 =w call $puts(l %var2)",
//                 "jmp @block2",
//                 "@block1",
//                 "@block2",
//             },
//         },
//         // Nested conditionals
//         .{
//             .name = "nested conditionals",
//             .source =
//             \\local x = 1
//             \\local y = 1
//             \\if (x) then
//             \\  if (y) then
//             \\    $puts("nested")
//             \\  end
//             \\end
//             ,
//             .expected = &.{
//                 "jnz %var1, @block0, @block1",
//                 "jnz %var3, @block3, @block4",
//                 "jmp @block5",
//                 "jmp @block2",
//             },
//         },
//     };

//     for (test_cases) |tc| {
//         const ir = try compileToQBE(tc.source);
//         defer alloc.free(ir);

//         expectIRContains(ir, tc.expected) catch |err| {
//             std.debug.print("\nTest case '{s}' failed\n", .{tc.name});
//             return err;
//         };
//     }
// }

// test "compile loop statements" {
//     const TestCase = struct {
//         name: []const u8,
//         source: []const u8,
//         must_contain: []const []const u8,
//     };

//     const test_cases = [_]TestCase{
//         // Do-end block with local variable
//         .{
//             .name = "do-end block with local variable",
//             .source =
//             \\do
//             \\  local x = 5
//             \\  $puts("in do block")
//             \\end
//             ,
//             .must_contain = &.{
//                 "jmp @block",
//                 "@block",
//                 "%var0 =l copy 5",
//                 "$puts",
//             },
//         },
//         // While loop with truthy condition
//         .{
//             .name = "while loop with truthy condition",
//             .source =
//             \\local x = 3
//             \\while (x) do
//             \\  $puts("looping")
//             \\  local x = x - 1
//             \\end
//             ,
//             .must_contain = &.{
//                 "jmp @block",
//                 "@block",
//                 "jnz %",
//                 "@block",
//                 "sub",
//                 "jmp @block",
//             },
//         },
//         // While loop with falsy condition (should skip)
//         .{
//             .name = "while loop with falsy condition",
//             .source =
//             \\local x = 0
//             \\while (x) do
//             \\  $puts("should not run")
//             \\end
//             ,
//             .must_contain = &.{
//                 "%var0 =l copy 0",
//                 "jnz %",
//             },
//         },
//         // Repeat-until loop (executes at least once)
//         .{
//             .name = "repeat-until loop",
//             .source =
//             \\local x = 0
//             \\repeat
//             \\  $puts("repeat body")
//             \\  local x = 1
//             \\until (x)
//             ,
//             .must_contain = &.{
//                 "jmp @block",
//                 "@block",
//                 "$puts",
//                 "jnz %",
//             },
//         },
//         // Nested while loops
//         .{
//             .name = "nested while loops",
//             .source =
//             \\local i = 2
//             \\while (i) do
//             \\  local j = 2
//             \\  while (j) do
//             \\    $puts("nested")
//             \\  end
//             \\end
//             ,
//             .must_contain = &.{
//                 "jnz %",
//                 "jnz %",
//                 "$puts",
//                 "jmp @block",
//                 "jmp @block",
//             },
//         },
//         // Do block with multiple statements
//         .{
//             .name = "do block with multiple statements",
//             .source =
//             \\do
//             \\  local a = 1
//             \\  local b = 2
//             \\  local c = a + b
//             \\end
//             ,
//             .must_contain = &.{
//                 "@block",
//                 "%var0 =l copy 1",
//                 "%var2 =l copy 2",
//                 "add",
//             },
//         },
//     };

//     for (test_cases) |tc| {
//         const ir = try compileToQBE(tc.source);
//         defer alloc.free(ir);

//         expectIRContains(ir, tc.must_contain) catch |err| {
//             std.debug.print("\nTest case '{s}' failed\n", .{tc.name});
//             std.debug.print("Generated IR:\n{s}\n", .{ir});
//             return err;
//         };
//     }
// }
