const std = @import("std");
const PrettyPrinter = @import("../pretty_printer.zig").PrettyPrinter;

test "basic write without indentation" {
    const allocator = std.testing.allocator;
    var writer = std.Io.Writer.Allocating.init(allocator);
    defer writer.deinit();

    var pp = PrettyPrinter.init(&writer.writer);

    try pp.write("hello world");

    try std.testing.expectEqualStrings("hello world", writer.written());
}

test "write with single indent level" {
    const allocator = std.testing.allocator;
    var writer = std.Io.Writer.Allocating.init(allocator);
    defer writer.deinit();

    var pp = PrettyPrinter.init(&writer.writer);

    pp.indent();
    try pp.write("indented line");

    try std.testing.expectEqualStrings("  indented line", writer.written());
}

test "print with single indent level" {
    const allocator = std.testing.allocator;
    var writer = std.Io.Writer.Allocating.init(allocator);
    defer writer.deinit();

    var pp = PrettyPrinter.init(&writer.writer);

    pp.indent();
    try pp.print("{s} {d}\n", .{ "hello", 10 });

    try std.testing.expectEqualStrings("  hello 10\n", writer.written());
}

test "write with multiple indent levels" {
    const allocator = std.testing.allocator;
    var writer = std.Io.Writer.Allocating.init(allocator);
    defer writer.deinit();

    var pp = PrettyPrinter.init(&writer.writer);

    pp.indent();
    pp.indent();
    pp.indent();
    try pp.write("deeply nested");

    // 3 levels * 2 spaces = 6 spaces
    try std.testing.expectEqualStrings("      deeply nested", writer.written());
}

test "indent and dedent cycling" {
    const allocator = std.testing.allocator;
    var writer = std.Io.Writer.Allocating.init(allocator);
    defer writer.deinit();

    var pp = PrettyPrinter.init(&writer.writer);

    try pp.write("no indent");
    try pp.nl();
    pp.indent();
    try pp.write("one level");
    try pp.nl();
    pp.indent();
    try pp.write("two levels");
    try pp.nl();
    pp.dedent();
    try pp.write("back to one");
    try pp.nl();
    pp.dedent();
    try pp.write("back to zero");
    try pp.nl();

    const expected =
        \\no indent
        \\  one level
        \\    two levels
        \\  back to one
        \\back to zero
        \\
    ;

    try std.testing.expectEqualStrings(expected, writer.written());
}

test "dedent below zero stays at zero" {
    const allocator = std.testing.allocator;
    var writer = std.Io.Writer.Allocating.init(allocator);
    defer writer.deinit();

    var pp = PrettyPrinter.init(&writer.writer);

    // Try to dedent when already at zero
    pp.dedent();
    pp.dedent();
    pp.dedent();
    try pp.write("still at zero");

    try std.testing.expectEqualStrings("still at zero", writer.written());
}

test "partial dedent when indent is less than INDENT_SPACES" {
    const allocator = std.testing.allocator;
    var writer = std.Io.Writer.Allocating.init(allocator);
    defer writer.deinit();

    var pp = PrettyPrinter.init(&writer.writer);

    // Manually set current_indent to 1 (less than INDENT_SPACES = 2)
    pp.current_indent = 1;
    pp.dedent();
    try pp.write("reset to zero");

    try std.testing.expectEqualStrings("reset to zero", writer.written());
}

test "newline output" {
    const allocator = std.testing.allocator;
    var writer = std.Io.Writer.Allocating.init(allocator);
    defer writer.deinit();

    var pp = PrettyPrinter.init(&writer.writer);

    try pp.write("line 1");
    try pp.nl();
    try pp.nl();
    try pp.write("line 2");
    try pp.nl();

    const expected =
        \\line 1
        \\
        \\line 2
        \\
    ;

    try std.testing.expectEqualStrings(expected, writer.written());
}

test "mixed indentation and newlines" {
    const allocator = std.testing.allocator;
    var writer = std.Io.Writer.Allocating.init(allocator);
    defer writer.deinit();

    var pp = PrettyPrinter.init(&writer.writer);

    try pp.write("function example()");
    try pp.nl();
    pp.indent();
    try pp.write("local x = 10");
    try pp.nl();
    try pp.nl();
    try pp.write("if x > 5 then");
    try pp.nl();
    pp.indent();
    try pp.write("print(x)");
    try pp.nl();
    pp.dedent();
    try pp.write("end");
    try pp.nl();
    pp.dedent();
    try pp.write("end");
    try pp.nl();

    const expected =
        \\function example()
        \\  local x = 10
        \\
        \\  if x > 5 then
        \\    print(x)
        \\  end
        \\end
        \\
    ;

    try std.testing.expectEqualStrings(expected, writer.written());
}

test "empty string write" {
    const allocator = std.testing.allocator;
    var writer = std.Io.Writer.Allocating.init(allocator);
    defer writer.deinit();

    var pp = PrettyPrinter.init(&writer.writer);

    pp.indent();
    try pp.write("");

    try std.testing.expectEqualStrings("  ", writer.written());
}

test "unicode content handling" {
    const allocator = std.testing.allocator;
    var writer = std.Io.Writer.Allocating.init(allocator);
    defer writer.deinit();

    var pp = PrettyPrinter.init(&writer.writer);

    pp.indent();
    try pp.write("hello 世界 🌍");

    try std.testing.expectEqualStrings("  hello 世界 🌍", writer.written());
}

test "multiple consecutive newlines" {
    const allocator = std.testing.allocator;
    var writer = std.Io.Writer.Allocating.init(allocator);
    defer writer.deinit();

    var pp = PrettyPrinter.init(&writer.writer);

    try pp.write("start");
    try pp.nl();
    try pp.nl();
    try pp.nl();
    try pp.nl();
    try pp.write("end");
    try pp.nl();

    const expected =
        \\start
        \\
        \\
        \\
        \\end
        \\
    ;

    try std.testing.expectEqualStrings(expected, writer.written());
}
