const allocPrint = @import("std").fmt.allocPrint;
const Compiler = @import("compiler.zig").QBECompiler;
const Temporary = @import("symbol_table.zig").Temporary;

// NOTE: sign bit is technically unused for now but its set to 1
const NAN_TAG_BASE: u64 = 0xFFF8_0000_0000_0000;

pub const BoxedValueType = enum(u3) { nil, bool, int, double, string, table, function, builtin };
pub const BoxedValue = union(BoxedValueType) {
    nil,
    bool,
    int,
    double,
    string,
    table,
    function,
    builtin,

    pub const nil_value = NAN_TAG_BASE; // this works cause tag is 0b000 and theres no value

    inline fn maskFor(t: BoxedValueType) u64 {
        return @as(u64, @intFromEnum(t)) << 48;
    }

    /// Turns an i32 into a nan boxed u64
    pub fn fromInt(i: i32) u64 {
        const int64 = @as(u32, @bitCast(i));
        const typemask: u64 = maskFor(.int);
        return @bitCast(NAN_TAG_BASE | typemask | int64);
    }

    /// Turns boolean into nan boxed u64
    pub fn fromBoolean(b: bool) u64 {
        const lsb = @as(u64, @intFromBool(b));
        const typemask = maskFor(.bool);
        return @bitCast(NAN_TAG_BASE | typemask | lsb);
    }
};

/// Emit instructions that unbox the nan value and return the temporary its stored in
pub fn emitValue(comptime t: BoxedValueType, c: *Compiler, boxed: *Temporary) !*Temporary {
    return switch (t) {
        .bool => {
            const mask: u32 = @as(u32, 0b1);

            // Cast the d into w to drop bits and get the right type
            const cast_result_temp = try c.emitAssignment(.l, "cast %{s}", .{boxed.name});

            // Mask all but last bit and return result
            const masked_temp = try c.emitAssignment(.l, "and %{s}, {d}", .{ cast_result_temp.name, mask });
            return masked_temp;
        },
        else => {
            unreachable;
        },
    };
}

/// Extract the 3 type bits
pub fn emitType(c: *Compiler, boxed: *Temporary) !*Temporary {
    // The type tag is stored in bits 48-50 (3 bits)
    // We need to shift right by 48 bits to get those bits into the lower position
    const shifted_temp = try c.emitAssignment(.l, "shr %{s}, 48", .{boxed.name});

    // Mask to keep only the lower 3 bits (0b111 = 7)
    const mask: u32 = 0b111;
    const masked_temp = try c.emitAssignment(.l, "and %{s}, {d}", .{ shifted_temp.name, mask });

    return masked_temp;
}

pub fn emitExpectNumber(c: *Compiler, t1: *Temporary) !void {
    _ = try emitType(c, t1);

    const post_error = try c.functions.createBlock();
    try c.pushBlock(post_error);
}
