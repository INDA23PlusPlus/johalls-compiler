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
        try stdout.print("No D++ file provided.\n", .{});
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

    try stdout.print("Compiling: '{s}'\n", .{args[1]});

    for (tokens.items) |e| {
        if (e.tp == .lbrace or e.tp == .rbrace or e.tp == .semicolon) {
            try stdout.print("{s}\n", .{e.get_representation()});
        } else {
            try stdout.print("{s} ", .{e.get_representation()});
        }
    }

    var ast = try AST.parse(std.heap.page_allocator, tokens.items);
    defer ast.deinit();

    try stdout.print("num tokens {}\n", .{tokens.items.len});
    try stdout.print("num functions {}\n", .{ast.functions.items.len});

    for (ast.functions.items) |fun| {
        try stdout.print("name: '{s}' number of parameters: {}\n", .{ fun.name_tok.get_representation(), fun.params.items.len });

        const utils = struct {
            fn print_expr(ex: Expression) void {
                switch (ex) {
                    .bin => |b| {
                        print_expr(b.lexpr.*);
                        std.debug.print(" {s} ", .{b.op.get_representation().?});
                        print_expr(b.rexpr.*);
                    },
                    .un => |u| {
                        std.debug.print("{s}", .{u.op.get_representation().?});
                        print_expr(u.expr.*);
                    },
                    .lit => |l| {
                        std.debug.print("{}", .{l.value});
                    },
                    .ident => |i| {
                        std.debug.print("{s}", .{i.name});
                    },
                    .paren => |p| {
                        std.debug.print("(", .{});
                        print_expr(p.child.*);
                        std.debug.print(")", .{});
                    },
                    .fn_call => |f| {
                        std.debug.print("{s}(", .{f.function_name.name});
                        var first = true;
                        for (f.arguments.items) |arg| {
                            if (!first) {
                                std.debug.print(", ", .{});
                            }
                            first = false;
                            print_expr(arg);
                        }
                        std.debug.print(")", .{});
                    },
                }
            }

            fn print_indentation(indentation_level: usize) void {
                for (0..indentation_level) |_| {
                    std.debug.print(" ", .{});
                }
            }

            fn print_statement(st: Statement, indentation_level: usize) void {
                switch (st) {
                    .assign => |a| {
                        print_indentation(indentation_level);
                        std.debug.print("let {s} = ", .{a.id.name});
                        print_expr(a.value);
                        std.debug.print(";\n", .{});
                    },
                    .conditional => |i| {
                        print_indentation(indentation_level);
                        std.debug.print("if ", .{});
                        print_expr(i.condition);
                        std.debug.print(" {s}\n", .{"{"});
                        for (i.true_branch.statements.items) |nx| {
                            print_statement(nx, indentation_level + 4);
                        }
                        print_indentation(indentation_level);
                        std.debug.print("{s}", .{"}"});
                        if (i.false_branch.statements.items.len > 0) {
                            std.debug.print(" else ", .{});
                            std.debug.print("{s}\n", .{"{"});
                            for (i.false_branch.statements.items) |nx| {
                                print_statement(nx, indentation_level + 4);
                            }
                            print_indentation(indentation_level);
                            std.debug.print("{s}\n", .{"}"});
                        } else {
                            std.debug.print("\n", .{});
                        }
                    },
                    .expr => |ex| {
                        print_indentation(indentation_level);
                        print_expr(ex);
                        std.debug.print(";\n", .{});
                    },
                    .ret => |ret| {
                        print_indentation(indentation_level);
                        std.debug.print("return ", .{});
                        print_expr(ret.returned_value);
                        std.debug.print(";\n", .{});
                    },
                }
            }
        };

        try stdout.print("formatted:\n", .{});

        try bw.flush();

        std.debug.print("fn {s}(", .{fun.name_tok.str});
        var first = true;
        for (fun.params.items) |param| {
            if (!first) {
                std.debug.print(", ", .{});
            }
            first = false;
            std.debug.print("{s}", .{param.str});
        }

        std.debug.print(") {s}\n", .{"{"});

        for (fun.children.statements.items) |statement| {
            utils.print_statement(statement, 4);
        }
        std.debug.print("{s}\n", .{"}"});
    }
}
