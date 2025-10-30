const std = @import("std");
const t = std.testing;
const alloc = std.testing.allocator;
const boxed = @import("../boxed_value.zig");

test "nan boxed masks" {
    try t.expectEqual(boxed.BoxedValue.maskFor(.nil), 0);
    try t.expectEqual(boxed.BoxedValue.maskFor(.bool), 1);
    try t.expectEqual(boxed.BoxedValue.maskFor(.int), 2);
    try t.expectEqual(boxed.BoxedValue.maskFor(.double), 3);
    try t.expectEqual(boxed.BoxedValue.maskFor(.string), 4);
    try t.expectEqual(boxed.BoxedValue.maskFor(.table), 5);
    try t.expectEqual(boxed.BoxedValue.maskFor(.function), 6);
    try t.expectEqual(boxed.BoxedValue.maskFor(.builtin), 7);
}
