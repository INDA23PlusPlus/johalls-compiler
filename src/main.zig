const std = @import("std");
const Allocator = std.mem.Allocator;

const tokenization = @import("tokenize.zig");
const tokenize = tokenization.tokenize;
const Token = tokenization.Token;

const AST = @import("ast.zig").AST;

pub fn main() !void {
    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    defer bw.flush() catch |err| {
        std.debug.print("flushing of stdout failed with error: '{}'\n", .{err});
    };

    const stdout = bw.writer();

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len < 2) {
        try stdout.print("No D++ file provided.\n", .{});
        return;
    }

    const input_file = std.fs.cwd().openFile(args[1], .{}) catch |err| {
        std.debug.print("Opening file failed with error: '{}'\n", .{err});
        return;
    };

    defer input_file.close();
    const file_contents = try input_file.readToEndAlloc(allocator, 100 << 20); // if your file is more than 100MB, wtf are you doing?
    defer allocator.free(file_contents);

    var tokens = try tokenize(allocator, file_contents
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

    try stdout.print("Compiling: '{s}'\n", .{args[1]});

    for (tokens.items) |e| {
        if (e.tp == .lbrace or e.tp == .rbrace or e.tp == .semicolon) {
            try stdout.print("{s}\n", .{e.get_representation()});
        } else {
            try stdout.print("{s} ", .{e.get_representation()});
        }
    }

    var ast = try AST.parse(allocator, tokens.items);
    defer ast.deinit(allocator);

    try stdout.print("num tokens {}\n", .{tokens.items.len});
    try stdout.print("num functions {}\n", .{ast.functions.items.len});

    for (ast.functions.items) |fun| {
        try stdout.print("name: '{s}' number of parameters: {}\n", .{ fun.name_tok.get_representation(), fun.params.items.len });
    }
}
