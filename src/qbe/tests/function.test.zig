const std = @import("std");
const function = @import("../function.zig");
const alloc = std.testing.allocator;

test "Function section create block" {
    var fs = function.FunctionSection.init(alloc);
    defer fs.deinit();

    const block1 = try fs.createBlock();
    const block2 = try fs.createBlock();

    try std.testing.expectEqualStrings("block0", block1.label);
    try std.testing.expectEqualStrings("block1", block2.label);
}
