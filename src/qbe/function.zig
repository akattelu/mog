const std = @import("std");
const Allocator = std.mem.Allocator;

/// QBE function linkage types
pub const Linkage = enum {
    none,
    @"export",
    thread,
    section,
};

/// QBE instruction data types
pub const Type = enum { w, l, s, d, sb, ub, sh, uh };

/// QBE basic block
/// A block is a sequence of instructions with a single entry point (the label)
/// and a single exit point (a terminating instruction like ret or jmp)
pub const Block = struct {
    /// Block label (without @ sigil, e.g., "start", "loop_body")
    label: []const u8,
    /// For now, store instruction as strings
    instructions: std.ArrayList([]const u8),
    /// Allocator for memory management
    alloc: Allocator,

    /// Initialize a new block with the given label
    pub fn init(allocator: Allocator, label: []const u8) !Block {
        const copied_label = try allocator.dupe(u8, label);
        return .{
            .label = copied_label,
            .instructions = std.ArrayList([]const u8).empty,
            .alloc = allocator,
        };
    }

    /// Deinitialize the block and free instruction list
    pub fn deinit(self: *Block) void {
        self.alloc.free(self.label);
        // Free each instruction string
        for (self.instructions.items) |instr| {
            self.alloc.free(instr);
        }
        self.instructions.deinit(self.alloc);
    }

    pub fn addInstruction(self: *Block, instr: []const u8) ![]const u8 {
        const owned = try self.alloc.dupe(u8, instr);
        try self.instructions.append(self.alloc, owned);
        return owned;
    }

    /// Caller is responsible for freeing
    pub fn getTemporaryVariableName(self: *Block) ![]const u8 {
        const idx = self.instructions.items.len;

        return try std.fmt.allocPrint(self.alloc, "%var{d}", .{idx});
    }

    /// Emit block to writer
    /// Format: @label followed by tab-indented instructions
    pub fn emit(self: *const Block, writer: *std.Io.Writer) !void {
        try writer.print("@{s}\n", .{self.label});
        for (self.instructions.items) |i| {
            try writer.writeByte('\t');
            try writer.writeAll(i);
            try writer.writeByte('\n');
        }
    }
};

/// QBE function definition
pub const Function = struct {
    /// Function name (without $ sigil)
    name: []const u8,
    /// Optional return type (null for void functions)
    return_type: ?Type,
    /// Linkage specification
    linkage: Linkage,
    /// Function parameters
    params: std.ArrayList(*Parameter),
    /// Map of block labels to blocks (maintains insertion order for emission)
    blocks: std.StringArrayHashMap(*Block),
    /// Allocator for memory management
    allocator: Allocator,
    /// Pointer to current block
    current_block: ?*Block,

    /// Initialize a new function with the given name and linkage
    /// name needs to outlive the struct lifetime
    pub fn init(
        allocator: Allocator,
        name: []const u8,
        return_type: ?Type,
        linkage: Linkage,
    ) !Function {
        // Copy name
        const copied_name = try allocator.dupe(u8, name);
        var func: Function = .{
            .name = copied_name,
            .return_type = return_type,
            .linkage = linkage,
            .params = std.ArrayList(*Parameter).empty,
            .blocks = std.StringArrayHashMap(*Block).init(allocator),
            .allocator = allocator,
            .current_block = null,
        };
        // every function starts with a @start block
        const start_block = try func.createBlock("start");
        func.current_block = start_block;
        return func;
    }

    /// Deinitialize the function and free all resources
    pub fn deinit(self: *Function) void {
        self.allocator.free(self.name);
        // Free parameters
        for (self.params.items) |param| {
            self.allocator.destroy(param);
        }
        self.params.deinit(self.allocator);
        // Free blocks
        var block_iter = self.blocks.iterator();
        while (block_iter.next()) |entry| {
            const block = entry.value_ptr.*;
            block.deinit();
            self.allocator.destroy(block);
        }
        self.blocks.deinit();
    }

    /// Add a parameter to the function signature with the given name and type
    pub fn createParameter(self: *Function, name: []const u8, t: Type) !*Parameter {
        const param = try self.allocator.create(Parameter);
        param.* = .{ .param_type = t, .name = name };

        try self.params.append(self.allocator, param);
        return param;
    }

    /// Create a block and add it to this function's list of blocks
    /// Returns a pointer to the created block
    pub fn createBlock(self: *Function, label: []const u8) !*Block {
        const block = try self.allocator.create(Block);
        block.* = try Block.init(self.allocator, label);
        try self.blocks.put(block.label, block);

        return block;
    }

    /// Get a block by its label (returns null if not found)
    pub fn getBlock(self: *Function, label: []const u8) ?*Block {
        return self.blocks.getPtr(label);
    }

    /// Emit function to writer
    /// Format: [linkage] function [return_type] $name(params) { blocks }
    pub fn emit(self: *const Function, writer: *std.Io.Writer) !void {
        // Emit linkage if not none
        switch (self.linkage) {
            .none => {},
            .@"export" => try writer.print("export ", .{}),
            .thread => try writer.print("thread ", .{}),
            .section => try writer.print("section ", .{}),
        }

        // Emit function keyword
        try writer.print("function ", .{});

        // Emit return type if present
        if (self.return_type) |rtype| {
            try writer.print("{s} ", .{@tagName(rtype)});
        }

        // Emit function name
        try writer.print("${s}(", .{self.name});

        // Emit parameters
        for (self.params.items, 0..) |param, i| {
            if (i > 0) try writer.print(", ", .{});
            try writer.print("{s} %{s}", .{ @tagName(param.param_type), param.name });
        }

        try writer.print(") {{\n", .{});

        // Emit blocks in order
        var block_iter = self.blocks.iterator();
        while (block_iter.next()) |entry| {
            try entry.value_ptr.*.emit(writer);
        }

        try writer.print("}}\n", .{});
    }
};

/// Struct corresponding to function section of QBE IR (array of function definitions)
pub const FunctionSection = struct {
    functions: std.ArrayList(*Function),
    allocator: Allocator,

    /// Create new function section
    pub fn init(allocator: Allocator) FunctionSection {
        return .{
            .functions = std.ArrayList(*Function).empty,
            .allocator = allocator,
        };
    }

    /// Deinitialize the function section and free all functions
    pub fn deinit(self: *FunctionSection) void {
        for (self.functions.items) |func| {
            func.deinit();
            self.allocator.destroy(func);
        }
        self.functions.deinit(self.allocator);
    }

    /// Add a function to the section
    pub fn add(self: *FunctionSection, func: *Function) !void {
        try self.functions.append(self.allocator, func);
    }

    pub fn addMainFunction(self: *FunctionSection) !*Function {
        // Add main function
        const main = try self.allocator.create(Function);
        main.* = try Function.init(self.allocator, "main", .w, .@"export");
        try self.add(main);
        return main;
    }

    /// Emit IR for the function section
    pub fn emit(self: *const FunctionSection, writer: *std.Io.Writer) !void {
        for (self.functions.items, 0..) |func, i| {
            try func.emit(writer);
            // Add newline between functions (but not after the last one)
            if (i < self.functions.items.len - 1) {
                try writer.writeByte('\n');
            }
        }
    }
};

/// QBE function parameter
/// Represents a single parameter in a function signature
pub const Parameter = struct {
    /// Parameter name (temporary variable without % sigil)
    name: []const u8,
    /// Parameter type
    param_type: Type,
};
