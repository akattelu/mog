const std = @import("std");
const List = std.DoublyLinkedList;

/// All the different types of definitions in the data section of the SSA
const DataDefinitionType = enum { string };

/// Union containing different types of data definitions such as stringsA
const DataDefinitionValue = union(DataDefinitionType) {
    string: []const u8,
};

/// A data definition containing an index and a union value
pub const DataDefinition = struct {
    value: DataDefinitionValue,
    idx: u32,
    node: List.Node,

    /// Initializes a new string data definition
    pub fn fromString(idx: u32, s: []const u8) DataDefinition {
        return .{ .value = .{ .string = s }, .idx = idx, .node = .{} };
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

/// Struct corresponding to data section of QBE IR (array of data definitions)
pub const Data = struct {
    items: List,

    /// Create new data list
    pub fn init() Data {
        return .{ .items = .{} };
    }

    /// Add a new DataDefinition to the list
    pub fn add(self: *Data, data: *DataDefinition) void {
        self.items.append(&data.node);
    }

    // Emit IR for the data section
    pub fn emit(self: *Data, writer: *std.Io.Writer) !void {
        var node = self.items.first;
        while (node) |def| {
            const a: *DataDefinition = @fieldParentPtr("node", def);
            try a.write(writer);
            try writer.writeByte('\n');
            node = a.node.next;
        }
        try writer.writeByte('\n');
    }
};
