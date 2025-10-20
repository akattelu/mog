const std = @import("std");
const alloc = std.testing.allocator;
const Lexer = @import("../../lexer.zig").Lexer;
const Parser = @import("../../parser.zig").Parser;
const QBECompiler = @import("../compiler.zig").QBECompiler;

test "compile simple puts statement" {

    // Inline test source code
    const source = "$puts \"hello world\"";

    // Create and run parser (mimicking build function from main.zig)
    var lexer = try Lexer.init(alloc, source);
    defer lexer.deinit();

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

    const output = writer.written();

    // Verify the output contains expected components
    try std.testing.expect(std.mem.indexOf(u8, output, "data $str_0 = { b \"hello world\", b 0 }") != null);
    try std.testing.expect(std.mem.indexOf(u8, output, "export function w $main()") != null);
    try std.testing.expect(std.mem.indexOf(u8, output, "@start") != null);
    try std.testing.expect(std.mem.indexOf(u8, output, "call $puts(l $str_0)") != null);
    try std.testing.expect(std.mem.indexOf(u8, output, "ret") != null);
}

test "add instruction helper" {
    var writer = std.Io.Writer.Allocating.init(alloc);
    defer writer.deinit();
    var compiler = try QBECompiler.init(alloc);
    defer compiler.deinit();

    const temp = try compiler.symbol_table.createTemporary(.function);
    try compiler.addInstruction(temp, .w, "call $puts(w %str_0)");

    try compiler.emit(&writer.writer);

    try std.testing.expect(std.mem.indexOf(u8, writer.written(), "%var0 =w call $puts(w %str_0)") != null);
}
