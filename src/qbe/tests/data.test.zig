const std = @import("std");
const data = @import("../data.zig");
const DataDefinition = data.DataDefinition;
const Data = data.Data;

test "writing string data definition" {
    const alloc = std.testing.allocator;
    var writer = std.Io.Writer.Allocating.init(alloc);
    defer writer.deinit();
    const test_cases = [_]struct { input: struct { idx: u32, string: []const u8 }, expected: []const u8 }{
        .{ .input = .{ .idx = 0, .string = "hello world" }, .expected = 
        \\data $str_0 = { b "hello world", b 0 }
        },
        .{ .input = .{ .idx = 1, .string = "hello world again" }, .expected = 
        \\data $str_1 = { b "hello world again", b 0 }
        },
    };

    inline for (test_cases) |case| {
        const def = DataDefinition.fromString(case.input.idx, case.input.string);
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

    // Make sample data
    var d_1: DataDefinition = .fromString(0, "hello");
    var d_2: DataDefinition = .fromString(1, "world");
    var d_3: DataDefinition = .fromString(2, "again");

    // Make data list
    var section: Data = .init();
    section.add(&d_1);
    section.add(&d_2);
    section.add(&d_3);

    // Write data
    const expected =
        \\data $str_0 = { b "hello", b 0 }
        \\data $str_1 = { b "world", b 0 }
        \\data $str_2 = { b "again", b 0 }
        \\
        \\
    ;

    try section.emit(&writer.writer);
    try std.testing.expectEqualStrings(expected, writer.written());
}
