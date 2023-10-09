const std = @import("std");
const Allocator = std.mem.Allocator;

const TokenType = enum {
    identifier,
    literal,

    semicolon,

    if_kw,
    else_kw,
    fn_kw,
    return_kw,

    add,
    subtract,
    multiply,
    divide,

    bit_or,
    bit_and,
    bit_xor,

    log_or,
    log_and,

    le,
    le_eq,

    gr,
    gr_eq,

    eq,
    n_eq,

    lparen,
    rparen,
    lbrace,
    rbrace,
};

const Token = struct {
    tp: TokenType,
    str: []const u8,

    fn new(tp: TokenType, str: []const u8) Token {
        return .{ .tp = tp, .str = str };
    }
};

fn tokenize(allocator: Allocator, input_str: []const u8) !std.ArrayList(Token) {
    var res = std.ArrayList(Token).init(allocator);

    const utils = struct {
        fn append_res(str: []const u8, re: *std.ArrayList(Token)) !void {
            // std.debug.print("{s} {}\n", .{ str, str.len });
            if (str.len == 0) {
                return;
            }

            const repr = struct {
                tp: TokenType,
                str: []const u8,
            };

            const reprs = [_]repr{
                .{ .tp = .if_kw, .str = "if" },
                .{ .tp = .else_kw, .str = "else" },
                .{ .tp = .fn_kw, .str = "fn" },
                .{ .tp = .return_kw, .str = "return" },
                .{ .tp = .semicolon, .str = ";" },
                .{ .tp = .lparen, .str = "(" },
                .{ .tp = .rparen, .str = ")" },
                .{ .tp = .lbrace, .str = "{" },
                .{ .tp = .rbrace, .str = "}" },
                .{ .tp = .add, .str = "+" },
                .{ .tp = .subtract, .str = "-" },
                .{ .tp = .multiply, .str = "*" },
                .{ .tp = .divide, .str = "/" },
                .{ .tp = .bit_or, .str = "|" },
                .{ .tp = .bit_and, .str = "&" },
                .{ .tp = .bit_xor, .str = "^" },
                .{ .tp = .log_or, .str = "||" },
                .{ .tp = .log_and, .str = "&&" },
                .{ .tp = .le, .str = "<" },
                .{ .tp = .le_eq, .str = "<=" },
                .{ .tp = .gr, .str = ">" },
                .{ .tp = .gr_eq, .str = ">=" },
                .{ .tp = .eq, .str = "==" },
                .{ .tp = .n_eq, .str = "!=" },
            };

            inline for (reprs) |rep| {
                if (std.mem.eql(u8, str, rep.str)) {
                    try re.append(Token.new(rep.tp, str));
                    return;
                }
            }
            const first_char = str[0];
            if ('0' <= first_char and first_char <= '9') {
                try re.append(Token.new(.literal, str));
            } else {
                try re.append(Token.new(.identifier, str));
            }
        }
    };

    var start: usize = 0;
    var end: usize = 0;
    for (input_str, 0..) |ch, i| {
        if (std.mem.indexOf(u8, " \t\r\n", &[_]u8{ch}) != null) {
            try utils.append_res(input_str[start..end], &res);

            // discard whitespace
            start = i + 1;
            end = i + 1;
            continue;
        }

        const ch_is_paren_or_semicolon = std.mem.indexOf(u8, "{}();", &[1]u8{ch}) != null;
        const buf_is_paren = std.mem.indexOfAny(u8, input_str[start..end], "{}()") != null;
        if (ch_is_paren_or_semicolon or buf_is_paren) {
            try utils.append_res(input_str[start..end], &res);
            start = i;
            end = i;
        }

        end += 1;
    }
    if (start < input_str.len) {
        try utils.append_res(input_str[start..end], &res);
    }

    return res;
}

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

    var tokens = try tokenize(allocator,

    file_contents
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
        const slice = switch (e.tp) {
            .identifier => "[identifier]",
            .literal => "[literal]",
            .semicolon => ";",

            .if_kw => "if",
            .else_kw => "else",
            .fn_kw => "fn",
            .return_kw => "return",

            .add => "+",
            .subtract => "-",
            .multiply => "*",
            .divide => "/",
            .bit_or => "|",
            .bit_and => "&",
            .bit_xor => "^",
            .log_or => "||",
            .log_and => "&&",
            .le => "<",
            .le_eq => "<=",
            .gr => ">",
            .gr_eq => ">=",
            .eq => "==",
            .n_eq => "!=",

            .lparen => "(",
            .rparen => ")",
            .lbrace => "{",
            .rbrace => "}",
        };

        if (e.tp == .lbrace or e.tp == .rbrace or e.tp == .semicolon) {
            try stdout.print("{s}\n", .{slice});
        } else {
            try stdout.print("{s} ", .{slice});
        }
    }
}

test "simple test" {
    var list = std.ArrayList(i32).init(std.testing.allocator);
    defer list.deinit(); // try commenting this out and see if zig detects the memory leak!
    try list.append(42);
    try std.testing.expectEqual(@as(i32, 42), list.pop());
}
