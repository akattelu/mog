const std = @import("std");
const function = @import("../function.zig");
const Function = function.Function;
const FunctionSection = function.FunctionSection;
const Block = function.Block;
const Instruction = function.Instruction;
const Call = function.Call;
const Ret = function.Ret;
const Parameter = function.Parameter;
const Type = function.Type;
const Linkage = function.Linkage;

test "emit simple function with return" {
    // Initialize writer
    const alloc = std.testing.allocator;
    var writer = std.Io.Writer.Allocating.init(alloc);
    defer writer.deinit();

    // Create function: function w $add(w %a, w %b) { @start ... }
    var func = try Function.init(alloc, "add", .w, .none);
    defer func.deinit();

    // Add parameters
    try func.addParameter(.{ .name = "a", .param_type = .w });
    try func.addParameter(.{ .name = "b", .param_type = .w });

    // Create start block
    var start_block = try Block.init(alloc, "start");

    // Add call instruction: %result =w call $internal_add(w %a, w %b)
    const args = [_][]const u8{ "w %a", "w %b" };
    try start_block.addInstruction(.{
        .lhs = "result",
        .assign_type = .w,
        .rhs = .{ .call = .{ .function_name = "internal_add", .args = &args } },
    });

    // Add return instruction: ret %result
    try start_block.addInstruction(.{
        .lhs = null,
        .assign_type = null,
        .rhs = .{ .ret = .{ .value = "%result" } },
    });

    try func.addBlock(start_block);

    // Emit and verify
    const expected = "function w $add(w %a, w %b) {\n" ++
        "@start\n" ++
        "\t%result =w call $internal_add(w %a, w %b)\n" ++
        "\tret %result\n" ++
        "}\n";

    try func.emit(&writer.writer);
    try std.testing.expectEqualStrings(expected, writer.written());
}

test "emit exported function with multiple blocks" {
    // Initialize writer
    const alloc = std.testing.allocator;
    var writer = std.Io.Writer.Allocating.init(alloc);
    defer writer.deinit();

    // Create exported function: export function w $main() { ... }
    var func = try Function.init(alloc, "main", .w, .@"export");
    defer func.deinit();

    // Create start block
    var start_block = try Block.init(alloc, "start");

    // Add call instruction: %r =w call $puts(l $str)
    const args = [_][]const u8{"l $str"};
    try start_block.addInstruction(.{
        .lhs = "r",
        .assign_type = .w,
        .rhs = .{ .call = .{ .function_name = "puts", .args = &args } },
    });

    // Add return instruction: ret 0
    try start_block.addInstruction(.{
        .lhs = null,
        .assign_type = null,
        .rhs = .{ .ret = .{ .value = "0" } },
    });

    try func.addBlock(start_block);

    // Emit and verify
    const expected = "export function w $main() {\n" ++
        "@start\n" ++
        "\t%r =w call $puts(l $str)\n" ++
        "\tret 0\n" ++
        "}\n";

    try func.emit(&writer.writer);
    try std.testing.expectEqualStrings(expected, writer.written());
}

test "emit void function with no parameters" {
    // Initialize writer
    const alloc = std.testing.allocator;
    var writer = std.Io.Writer.Allocating.init(alloc);
    defer writer.deinit();

    // Create void function: function $cleanup() { ... }
    var func = try Function.init(alloc, "cleanup", null, .none);
    defer func.deinit();

    // Create block
    var block = try Block.init(alloc, "entry");

    // Add void call: call $free()
    const no_args = [_][]const u8{};
    try block.addInstruction(.{
        .lhs = null,
        .assign_type = null,
        .rhs = .{ .call = .{ .function_name = "free", .args = &no_args } },
    });

    // Add void return: ret
    try block.addInstruction(.{
        .lhs = null,
        .assign_type = null,
        .rhs = .{ .ret = .{ .value = null } },
    });

    try func.addBlock(block);

    // Emit and verify
    const expected = "function $cleanup() {\n" ++
        "@entry\n" ++
        "\tcall $free()\n" ++
        "\tret\n" ++
        "}\n";

    try func.emit(&writer.writer);
    try std.testing.expectEqualStrings(expected, writer.written());
}

test "emit function section with multiple functions" {
    // Initialize writer
    const alloc = std.testing.allocator;
    var writer = std.Io.Writer.Allocating.init(alloc);
    defer writer.deinit();

    // Create function section
    var section = FunctionSection.init(alloc);
    defer section.deinit();

    // Create first function: function w $add(w %a, w %b)
    var func1 = try Function.init(alloc, "add", .w, .none);
    try func1.addParameter(.{ .name = "a", .param_type = .w });
    try func1.addParameter(.{ .name = "b", .param_type = .w });

    var block1 = try Block.init(alloc, "start");
    const args1 = [_][]const u8{ "w %a", "w %b" };
    try block1.addInstruction(.{
        .lhs = "result",
        .assign_type = .w,
        .rhs = .{ .call = .{ .function_name = "internal_add", .args = &args1 } },
    });
    try block1.addInstruction(.{
        .lhs = null,
        .assign_type = null,
        .rhs = .{ .ret = .{ .value = "%result" } },
    });
    try func1.addBlock(block1);

    // Create second function: export function w $main()
    var func2 = try Function.init(alloc, "main", .w, .@"export");

    var block2 = try Block.init(alloc, "start");
    const args2 = [_][]const u8{"l $str"};
    try block2.addInstruction(.{
        .lhs = "r",
        .assign_type = .w,
        .rhs = .{ .call = .{ .function_name = "puts", .args = &args2 } },
    });
    try block2.addInstruction(.{
        .lhs = null,
        .assign_type = null,
        .rhs = .{ .ret = .{ .value = "0" } },
    });
    try func2.addBlock(block2);

    // Add functions to section
    try section.add(&func1);
    try section.add(&func2);

    // Emit and verify
    const expected = "function w $add(w %a, w %b) {\n" ++
        "@start\n" ++
        "\t%result =w call $internal_add(w %a, w %b)\n" ++
        "\tret %result\n" ++
        "}\n" ++
        "\n" ++
        "export function w $main() {\n" ++
        "@start\n" ++
        "\t%r =w call $puts(l $str)\n" ++
        "\tret 0\n" ++
        "}\n";

    try section.emit(&writer.writer);
    try std.testing.expectEqualStrings(expected, writer.written());
}
