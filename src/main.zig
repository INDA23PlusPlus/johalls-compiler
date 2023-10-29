const std = @import("std");
const Allocator = std.mem.Allocator;

const token = @import("token.zig");
const tokenize = token.tokenize;
const Token = token.Token;

const astm = @import("ast.zig");
const AST = astm.AST;
const Statement = astm.Statement;
const Expression = astm.Expression;

pub fn main() !void {
    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    defer bw.flush() catch |err| {
        std.debug.print("flushing of stdout failed with error: '{}'\n", .{err});
    };

    const stdout = bw.writer();

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const gpa_allocator = gpa.allocator();

    const args = try std.process.argsAlloc(gpa_allocator);
    defer std.process.argsFree(gpa_allocator, args);

    if (args.len < 2) {
        std.debug.print("No D++ file provided.\n", .{});
        return;
    }

    const input_file = std.fs.cwd().openFile(args[1], .{}) catch |err| {
        std.debug.print("Opening file failed with error: '{}'\n", .{err});
        return;
    };
    defer input_file.close();

    const file_contents = try input_file.readToEndAlloc(gpa_allocator, 100 << 20); // if your file is more than 100MB, wtf are you doing?
    defer gpa_allocator.free(file_contents);

    var tokens = try tokenize(gpa_allocator, file_contents
    // "\nfn fib(n)"

    // \\
    // \\fn fib(n)
    // \\ {
    // \\  if n < 2 {
    // \\    return n;
    // \\  } else {
    // \\    return fib(n - 1) + fib(n - 2);
    // \\  }
    // \\}
    );
    defer tokens.deinit();
    var ast = try AST.parse(std.heap.page_allocator, tokens.items);
    defer ast.deinit();

    try ast.check();

    const c_str = try ast.as_c(gpa_allocator);
    defer c_str.deinit();
    try stdout.print("{s}\n", .{c_str.items});
}
