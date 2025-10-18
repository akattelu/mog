const std = @import("std");
const instruction = @import("../instruction.zig");

test "instruction emit and free" {
    // Initialize writer
    const alloc = std.testing.allocator;
    var writer = std.Io.Writer.Allocating.init(alloc);
    defer writer.deinit();

    const i = try alloc.create(instruction.Instruction);
    defer alloc.destroy(i);

    var call = try instruction.Call.init(alloc, "test");
    try call.add_arg(.w, "arg1");
    defer call.deinit();

    i.* = .{ .lhs = null, .rhs = .{ .call = &call }, .assign_type = null };

    try i.emit(&writer.writer);

    try std.testing.expectEqualStrings(
        \\call test(w $arg1)
    , writer.written());
}
