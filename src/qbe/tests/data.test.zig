const std = @import("std");
const data = @import("../data.zig");
const DataDefinition = data.DataDefinition;

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
