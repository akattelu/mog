const std = @import("std");
const alloc = std.testing.allocator;
const Lexer = @import("../../lexer.zig").Lexer;
const Parser = @import("../../parser.zig").Parser;
const QBECompiler = @import("../compiler.zig").QBECompiler;

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
        try std.testing.expect(std.mem.indexOf(u8, ir, substring) != null);
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
