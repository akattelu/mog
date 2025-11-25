const std = @import("std");
const data = @import("../data.zig");
const DataDefinition = data.DataDefinition;
const Data = data.Data;

test "writing full data section" {
    // Initialize writer
    const alloc = std.testing.allocator;
    var writer = std.Io.Writer.Allocating.init(alloc);
    defer writer.deinit();

    // Make data list
    var section: Data = .init(alloc);
    _ = try section.addString("hello");
    _ = try section.addString("world");
    _ = try section.addString("again");

    // Write data
    const expected =
        \\data $str_0 = { b "hello", b 0 }
        \\data $str_1 = { b "world", b 0 }
        \\data $str_2 = { b "again", b 0 }
        \\
        \\
    ;

    try section.write(&writer.writer);
    section.deinit();
    try std.testing.expectEqualStrings(expected, writer.written());
}
