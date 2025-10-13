const std = @import("std");
const Writer = std.Io.Writer;

const INDENT_SPACES = 2;

pub const PrettyPrinter = struct {
    writer: *Writer,
    current_indent: u32,
    at_line_start: bool,

    /// Initialize pretty printer with std.Io.Writer
    pub fn init(writer: *Writer) PrettyPrinter {
        return PrettyPrinter{
            .writer = writer,
            .current_indent = 0,
            .at_line_start = true,
        };
    }

    /// Following writes will have an incremented prefix indent
    pub fn indent(self: *PrettyPrinter) void {
        self.current_indent += INDENT_SPACES;
    }

    /// Following writes will have an decremented prefix indent
    /// Cannot fall below 0 indent
    pub fn dedent(self: *PrettyPrinter) void {
        if (self.current_indent >= INDENT_SPACES) {
            self.current_indent -= INDENT_SPACES;
        } else {
            self.current_indent = 0;
        }
    }

    /// Write contents to the pretty printer with the current indentation
    pub fn write(self: *PrettyPrinter, content: []const u8) Writer.Error!void {
        if (self.at_line_start) {
            try self.writer.splatByteAll(' ', self.current_indent);
            self.at_line_start = false;
        }
        try self.writer.print("{s}", .{content});
    }

    /// Write a newline to the pretty printer
    pub fn nl(self: *PrettyPrinter) Writer.Error!void {
        try self.writer.writeByte('\n');
        self.at_line_start = true;
    }

    /// Write contents to the pretty printer with the current indentation
    pub fn print(self: *PrettyPrinter, comptime fmt: []const u8, args: anytype) Writer.Error!void {
        if (self.at_line_start) {
            try self.writer.splatByteAll(' ', self.current_indent);
            self.at_line_start = false;
        }
        try self.writer.print(fmt, args);
    }
};
