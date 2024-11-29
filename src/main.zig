const std = @import("std");
pub const token = @import("token.zig");

pub fn main() !void {}

test {
    std.testing.refAllDecls(@This());
}
