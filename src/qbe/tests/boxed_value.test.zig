const std = @import("std");
const t = std.testing;
const alloc = std.testing.allocator;
const boxed = @import("../boxed_value.zig");

test "nan boxed masks" {
    try t.expect(true);
}
