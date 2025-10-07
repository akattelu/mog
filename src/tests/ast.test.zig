const std = @import("std");
const testing = std.testing;
const token = @import("../token.zig");
const ast = @import("../ast.zig");

const Identifier = ast.Identifier;
const NameList = ast.NameList;

test "NameList Writer" {
    const alloc = testing.allocator;

    // Make 3 names
    const name1 = try testing.allocator.create(Identifier);
    defer testing.allocator.destroy(name1);
    name1.* = .{ .token = token.Token{ .type = .ident, .literal = "hello", .start_pos = 0, .end_pos = 5 }, .value = "hello" };

    const name2 = try testing.allocator.create(Identifier);
    defer testing.allocator.destroy(name2);
    name2.* = .{ .token = token.Token{ .type = .ident, .literal = "world", .start_pos = 0, .end_pos = 5 }, .value = "world" };

    const name3 = try testing.allocator.create(Identifier);
    defer testing.allocator.destroy(name3);
    name3.* = .{ .token = token.Token{ .type = .ident, .literal = "again", .start_pos = 0, .end_pos = 5 }, .value = "again" };

    // Initialize writer
    var writer = std.Io.Writer.Allocating.init(alloc);
    defer writer.deinit();

    // Test single name
    var nl = try NameList.init(alloc, name1);
    defer nl.deinit();

    try nl.write(&writer.writer);
    try testing.expectEqualStrings("hello", writer.written());
    writer.clearRetainingCapacity();

    // Test 3 names
    try nl.add(name2);
    try nl.add(name3);

    try nl.write(&writer.writer);
    try testing.expectEqualStrings("hello, world, again", writer.written());
}
