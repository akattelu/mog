const std = @import("std");
const List = std.DoublyLinkedList;
const Allocator = std.mem.Allocator;

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
    alloc: Allocator,

    /// Initialize owned data with string namek:w
    ///  and value
    pub fn initString(allocator: Allocator, name: []const u8, value: []const u8) !DataDefinition {
        const copied_name = try allocator.dupe(u8, name);
        const copied_value = try allocator.dupe(u8, value);

        return .{ .name = copied_name, .value = .{ .string = copied_value }, .alloc = allocator, .node = .{} };
    }

    /// Free copied name and value
    pub fn deinit(self: *const DataDefinition) void {
        self.alloc.free(self.name);
        switch (self.value) {
            .string => {
                self.alloc.free(self.value.string);
            },
        }
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
    alloc: std.mem.Allocator,

    /// Create new data list
    pub fn init(alloc: std.mem.Allocator) Data {
        return .{ .items = .{}, .alloc = alloc };
    }

    /// Deinit the strings and DataDefinitions allocated by this struct
    pub fn deinit(self: *Data) void {
        var node = self.items.first;
        while (node) |def| {
            // Free name, then node
            const a: *DataDefinition = @fieldParentPtr("node", def);
            node = a.node.next;

            a.deinit();
            self.alloc.destroy(a);
        }
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
        // Get idx from linked list length
        const idx: u32 = @intCast(self.items.len());

        const name = try std.fmt.allocPrint(self.alloc, "str_{d}", .{idx});
        defer self.alloc.free(name);

        // Allocate, create, add DataDefinition
        const dd = try self.alloc.create(DataDefinition);
        dd.* = try DataDefinition.initString(self.alloc, name, s);
        self.add(dd);
        return dd.name;
    }
};
