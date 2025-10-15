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
    name: []const u8,
    node: List.Node,

    /// Initializes a new string data definition
    /// name and value must outlive the returned struct
    pub fn fromString(name: []const u8, value: []const u8) DataDefinition {
        return .{ .value = .{ .string = value }, .name = name, .node = .{} };
    }

    /// Write the definition to the writer (excludes newline)
    pub fn write(self: *const DataDefinition, writer: *std.Io.Writer) !void {
        switch (self.value) {
            .string => |val| {
                try writer.print(
                    \\data ${s} = {{ b "{s}", b 0 }}
                , .{ self.name, val });
            },
        }
        return;
    }
};

/// Struct corresponding to data section of QBE IR (array of data definitions)
pub const Data = struct {
    items: List,
    arena: std.heap.ArenaAllocator,

    /// Create new data list
    pub fn init(alloc: std.mem.Allocator) Data {
        const arena = std.heap.ArenaAllocator.init(alloc);
        return .{ .items = .{}, .arena = arena };
    }

    /// Deinit the strings and DataDefinitions allocated by this struct
    pub fn deinit(self: *Data) void {
        self.arena.deinit();
    }

    /// Add a new DataDefinition to the list
    fn add(self: *Data, data: *DataDefinition) void {
        self.items.append(&data.node);
    }

    /// Emit IR for the data section
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

    /// Add a string to the data section and return its name
    pub fn addString(self: *Data, s: []const u8) ![]const u8 {
        // Copy string just in case, to make sure it outlives scope
        var alloc = self.arena.allocator();
        const copiedString = try alloc.dupe(u8, s);

        // Get idx from linked list length
        const idx: u32 = @intCast(self.items.len());
        const name: []const u8 = try std.fmt.allocPrint(alloc, "str_{d}", .{idx});

        // Allocate, create, add DataDefinition
        const dd = try alloc.create(DataDefinition);
        dd.* = DataDefinition.fromString(name, copiedString);
        self.add(dd);
        return name;
    }
};
