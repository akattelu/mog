const std = @import("std");
const function = @import("../function.zig");
const Function = function.Function;
const FunctionSection = function.FunctionSection;
const Block = function.Block;
const instruction = @import("../instruction.zig");
const Instruction = instruction.Instruction;
const Call = instruction.Call;
const Ret = instruction.Ret;
const Parameter = function.Parameter;
const Type = instruction.Type;
const Linkage = function.Linkage;

test {
    try std.testing.expect(true);
}
