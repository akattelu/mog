const std = @import("std");
const Writer = std.Io.Writer;
const Token = @import("../token.zig").Token;

test "write" {
    // Create testing allocator
    const alloc = std.testing.allocator;

    // Create allocating writer interface
    var writer = Writer.Allocating.init(alloc);
    defer writer.deinit();

    // Write token
    const tok = Token{ .type = .ident, .literal = "hello", .start_pos = 0, .end_pos = 5 };
    try tok.write(&(writer.writer));

    const actual = writer.written();
    try std.testing.expectEqualStrings("hello[.ident]@0..5", actual);
}
