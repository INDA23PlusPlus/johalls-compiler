const std = @import("std");
const Allocator = std.mem.Allocator;

const TokenType = enum {
    identifier,
    literal,

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
    idx: usize,

    fn new(tp: TokenType, idx: usize) Token {
        return .{ .tp = tp, .idx = idx };
    }
};

fn tokenize(allocator: Allocator, str: []const u8) !std.ArrayList(Token) {
    var res = std.ArrayList(Token).init(allocator);

    // try res.append(Token.new(.identifier, 0));

    var buf = std.ArrayList(u8).init(allocator);
    defer buf.deinit();

    const utils = struct {
        fn clear_buf(bu: *std.ArrayList(u8), re: *std.ArrayList(Token), idx: usize) !void {
            // std.debug.print("{s} {}\n", .{ bu.items, bu.items.len });
            if (bu.items.len == 0) {
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

            var matched = false;
            inline for (reprs) |rep| {
                if (std.mem.eql(u8, bu.items, rep.str)) {
                    try re.append(Token.new(rep.tp, idx));
                    matched = true;
                }
            }
            if (!matched) {
                const first_char = bu.items[0];
                if ('0' <= first_char and first_char <= '9') {
                    try re.append(Token.new(.literal, idx));
                } else {
                    try re.append(Token.new(.identifier, idx));
                }
            }

            bu.clearRetainingCapacity();
        }
    };

    for (str, 0..) |ch, i| {
        if (std.mem.indexOf(u8, " \t\r\n", &[1]u8{ch}) != null) {
            try utils.clear_buf(&buf, &res, i - buf.items.len);
            continue;
        }
        const ch_is_paren = std.mem.indexOf(u8, "{}()", &[1]u8{ch}) != null;
        const buf_is_paren = std.mem.indexOfAny(u8, buf.items, "{}()") != null;
        if (ch_is_paren or buf_is_paren) {
            try utils.clear_buf(&buf, &res, i - buf.items.len);
        }

        try buf.append(ch);
    }
    try utils.clear_buf(&buf, &res, str.len - buf.items.len);

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

    var tokens = try tokenize(allocator, file_contents

    // \\
    // \\fn fib(n) {
    // \\  if n < 2 {
    // \\    return n;
    // \\  } else {
    // \\    return fib(n - 1) + fib(n - 2);
    // \\  }
    // \\}
    );
    defer tokens.deinit();

    for (tokens.items) |e| {
        const slice = switch (e.tp) {
            .identifier => "[identifier]",
            .literal => "[literal]",

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

        if (e.tp == .rparen or e.tp == .lbrace or e.tp == .rbrace) {
            try stdout.print("{s}\n", .{slice});
        } else {
            try stdout.print("{s} ", .{slice});
        }
    }

    try stdout.print("Compiling: '{s}'\n", .{args[1]});
}

test "simple test" {
    var list = std.ArrayList(i32).init(std.testing.allocator);
    defer list.deinit(); // try commenting this out and see if zig detects the memory leak!
    try list.append(42);
    try std.testing.expectEqual(@as(i32, 42), list.pop());
}
