const std = @import("std");
const t = std.testing;
const alloc = std.testing.allocator;
const boxed = @import("../nan_box.zig");
const QBECompiler = @import("../compiler.zig").QBECompiler;
const Temporary = @import("../symbol_table.zig").Temporary;

test "emitType extracts type bits correctly" {
    var compiler = try QBECompiler.init(alloc);
    defer compiler.deinit();

    // Create a boxed value temporary - this would contain a NaN-tagged value
    const boxed_temp = try compiler.emitAssignment(.d, "copy d_0", .{});

    // Call emitType to extract the type bits
    const type_temp = try boxed.emitType(&compiler, boxed_temp);

    // Capture the emitted IR
    var writer = std.Io.Writer.Allocating.init(alloc);
    defer writer.deinit();
    try compiler.emit(&writer.writer);
    const ir = writer.written();

    try t.expect(std.mem.indexOf(u8, ir, "cast") != null);
    try t.expect(std.mem.indexOf(u8, ir, "shr") != null);
    try t.expect(std.mem.indexOf(u8, ir, ", 48") != null);
    try t.expect(std.mem.indexOf(u8, ir, "and") != null);
    try t.expect(std.mem.indexOf(u8, ir, ", 7") != null);
    try t.expectEqualStrings("var3", type_temp.name);
}

test "emitValue unboxes boolean correctly" {
    var compiler = try QBECompiler.init(alloc);
    defer compiler.deinit();

    // Create a boxed boolean temporary
    const boxed_bool = try compiler.emitAssignment(.d, "copy d_0", .{});

    // Call emitValue to unbox the boolean
    const unboxed = try boxed.emitValue(.bool, &compiler, boxed_bool);

    // Capture the emitted IR
    var writer = std.Io.Writer.Allocating.init(alloc);
    defer writer.deinit();
    try compiler.emit(&writer.writer);
    const ir = writer.written();

    // Test case 1: Verify cast instruction is emitted (d -> l)
    try t.expect(std.mem.indexOf(u8, ir, "cast") != null);

    // Test case 2: Verify and instruction with mask value 1
    try t.expect(std.mem.indexOf(u8, ir, "and") != null);
    try t.expect(std.mem.indexOf(u8, ir, ", 1") != null);

    // Test case 3: Verify temporary chain - var0 (input), var1 (cast), var2 (masked result)
    try t.expectEqualStrings("var2", unboxed.name);
    try t.expect(std.mem.indexOf(u8, ir, "%var0") != null);
    try t.expect(std.mem.indexOf(u8, ir, "%var1") != null);
    try t.expect(std.mem.indexOf(u8, ir, "%var2") != null);
}

test "expect number error" {
    var compiler = try QBECompiler.init(alloc);
    defer compiler.deinit();

    const t1 = try compiler.emitAssignment(.l, "copy {d}", .{boxed.BoxedValue.fromInt(42)});
    try boxed.emitExpectNumber(&compiler, t1);

    var writer = std.Io.Writer.Allocating.init(alloc);
    defer writer.deinit();
    try compiler.emit(&writer.writer);
    const ir = writer.written();

    // Expect shift right for type checking
    try t.expect(std.mem.indexOf(u8, ir, "shr") != null);
    // try t.expect(std.mem.indexOf(u8, ir, "jnz") != null);
}
