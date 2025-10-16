const std = @import("std");
const data = @import("../data.zig");
const DataDefinition = data.DataDefinition;
const Data = data.Data;

test "writing string data definition" {
    // Make writer
    const alloc = std.testing.allocator;
    var writer = std.Io.Writer.Allocating.init(alloc);
    defer writer.deinit();

    const test_cases = [_]struct { input: struct { name: []const u8, string: []const u8 }, expected: []const u8 }{
        .{ .input = .{ .name = "str_0", .string = "hello world" }, .expected = 
        \\data $str_0 = { b "hello world", b 0 }
        },
        .{ .input = .{ .name = "str_1", .string = "hello world again" }, .expected = 
        \\data $str_1 = { b "hello world again", b 0 }
        },
    };

    inline for (test_cases) |case| {
        // Check writer contents
        const def = try DataDefinition.initString(alloc, case.input.name, case.input.string);
        defer def.deinit();
        try def.write(&writer.writer);
        try std.testing.expectEqualStrings(case.expected, writer.written());

        writer.clearRetainingCapacity();
    }
}

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

    try section.emit(&writer.writer);
    section.deinit();
    try std.testing.expectEqualStrings(expected, writer.written());
}
