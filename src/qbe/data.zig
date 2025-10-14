const std = @import("std");

/// All the different types of definitions in the data section of the SSA
const DataDefinitionType = enum { string };

/// Union containing different types of data definitions such as stringsA
const DataDefinitionValue = union(DataDefinitionType) {
    string: []const u8,
};

pub const DataDefinition = struct {
    value: DataDefinitionValue,
    idx: u32,

    /// Initializes a new string data definition
    pub fn fromString(idx: u32, s: []const u8) DataDefinition {
        return .{ .value = .{ .string = s }, .idx = idx };
    }

    /// Write the definition to the writer (excludes newline)
    pub fn write(self: *const DataDefinition, writer: *std.Io.Writer) !void {
        switch (self.value) {
            .string => |val| {
                try writer.print(
                    \\data $str_{d} = {{ b "{s}", b 0 }}
                , .{ self.idx, val });
            },
        }
        return;
    }
};
