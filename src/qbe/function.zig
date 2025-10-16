const std = @import("std");
const instruction = @import("instruction.zig");
const Instruction = instruction.Instruction;
const Call = instruction.Call;
const Type = instruction.Type;

/// QBE function linkage types
pub const Linkage = enum {
    none,
    @"export",
    thread,
    section,
};

/// QBE basic block
/// A block is a sequence of instructions with a single entry point (the label)
/// and a single exit point (a terminating instruction like ret or jmp)
pub const Block = struct {
    /// Block label (without @ sigil, e.g., "start", "loop_body")
    label: []const u8,
    /// Instructions in this block, ending with a terminating instruction
    instructions: std.ArrayList(*Instruction),
    /// Allocator for memory management
    alloc: std.mem.Allocator,

    /// Initialize a new block with the given label
    pub fn init(allocator: std.mem.Allocator, label: []const u8) !Block {
        const copied_label = try allocator.dupe(u8, label);
        return .{
            .label = copied_label,
            .instructions = try std.ArrayList(*Instruction).initCapacity(allocator, 5),
            .alloc = allocator,
        };
    }

    /// Deinitialize the block and free instruction list
    pub fn deinit(self: *Block) void {
        self.instructions.deinit(self.alloc);
        self.alloc.free(self.label);
    }

    /// Add an instruction to this block
    pub fn addInstruction(self: *Block, i: *Instruction) !void {
        try self.instructions.append(self.alloc, i);
    }

    pub fn addNewCall(self: *Block, name: []const u8) !*Call {
        const instr: *Instruction = try self.alloc.create(Instruction);
        const call: *Call = try self.alloc.create(Call);
        call.* = try Call.init(self.alloc, name);
        instr.* = .{ .lhs = null, .assign_type = null, .rhs = .{ .call = call } };
        try self.addInstruction(instr);
        return call;
    }

    /// Emit block to writer
    /// Format: @label followed by tab-indented instructions
    pub fn emit(self: *const Block, writer: *std.Io.Writer) !void {
        try writer.print("@{s}\n", .{self.label});
        for (self.instructions.items) |i| {
            try writer.writeByte('\t');
            try i.emit(writer);
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
    allocator: std.mem.Allocator,
    /// Pointer to current block
    current_block: *Block,

    /// Initialize a new function with the given name and linkage
    /// name needs to outlive the struct lifetime
    pub fn init(
        allocator: std.mem.Allocator,
        name: []const u8,
        return_type: ?Type,
        linkage: Linkage,
    ) !Function {

        // Copy name
        const copied_name = try allocator.dupe(u8, name);
        // every function starts with a @start block
        const start_block = try allocator.create(Block);
        start_block.* = try Block.init(allocator, "start");
        var func: Function = .{
            .name = copied_name,
            .return_type = return_type,
            .linkage = linkage,
            .params = try std.ArrayList(*Parameter).initCapacity(allocator, 4),
            .blocks = std.StringArrayHashMap(*Block).init(allocator),
            .allocator = allocator,
            .current_block = start_block,
        };
        try func.addBlock(start_block);
        return func;
    }

    /// Deinitialize the function and free all resources
    pub fn deinit(self: *Function) void {
        self.params.deinit(self.allocator);
        var block_iter = self.blocks.iterator();
        while (block_iter.next()) |entry| {
            const block = entry.value_ptr.*;
            block.deinit();
            self.allocator.destroy(block);
        }
        self.blocks.deinit();
        self.allocator.free(self.name);
    }

    /// Add a parameter to the function signature
    pub fn addParameter(self: *Function, param: *Parameter) !void {
        try self.params.append(self.allocator, param);
    }

    /// Add a block to the function body
    /// The block is moved into the function's ownership
    pub fn addBlock(self: *Function, block: *Block) !void {
        try self.blocks.put(block.label, block);
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
    allocator: std.mem.Allocator,

    /// Create new function section
    pub fn init(allocator: std.mem.Allocator) FunctionSection {
        return .{
            .functions = std.ArrayList(*Function).empty,
            .allocator = allocator,
        };
    }

    /// Deinitialize the function section and free all functions
    pub fn deinit(self: *FunctionSection) void {
        for (self.functions.items) |func| {
            func.deinit();
        }
        self.functions.deinit(self.allocator);
    }

    /// Add a function to the section
    pub fn add(self: *FunctionSection, func: *Function) !void {
        try self.functions.append(self.allocator, func);
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
