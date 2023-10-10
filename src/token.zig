const std = @import("std");
const Allocator = std.mem.Allocator;

pub const TokenType = enum {
    identifier,
    literal,

    comma,
    semicolon,

    if_kw,
    else_kw,
    fn_kw,
    return_kw,

    add,
    subtract,
    multiply,
    divide,
    modulo,

    bit_or,
    bit_and,
    bit_xor,
    bit_not,

    log_or,
    log_and,
    log_not,

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

    pub fn get_representation(self: TokenType) ?[]const u8 {
        for (TokenRepresentations) |repr| {
            if (repr.tp == self) {
                return repr.str;
            }
        }
        return null;
    }

    pub fn get_precedence(self: TokenType) ?usize {
        return switch (self) {
            .multiply => 0,
            .divide => 0,
            .modulo => 0,

            .add => 1,
            .subtract => 1,

            .le => 2,
            .le_eq => 2,
            .gr => 2,
            .gr_eq => 2,

            .eq => 3,
            .n_eq => 3,

            .bit_and => 4,
            .bit_xor => 5,
            .bit_or => 6,

            .log_and => 7,
            .log_or => 8,

            else => null,
        };
    }
};

pub const Token = struct {
    tp: TokenType,
    str: []const u8,

    fn new(tp: TokenType, str: []const u8) Token {
        return .{ .tp = tp, .str = str };
    }

    pub fn get_representation(self: Token) []const u8 {
        return self.str;
    }

    pub fn is_unary_operator(self: Token) bool {
        return switch (self.tp) {
            .bit_not => true,
            .log_not => true,
            else => false,
        };
    }

    pub fn is_binary_operator(self: Token) bool {
        return switch (self.tp) {
            .add => true,
            .subtract => true,
            .multiply => true,
            .divide => true,
            .bit_or => true,
            .bit_and => true,
            .bit_xor => true,
            .log_or => true,
            .log_and => true,
            .le => true,
            .le_eq => true,
            .gr => true,
            .gr_eq => true,
            .eq => true,
            .n_eq => true,

            else => false,
        };
    }
};

const TokenStringRepr = struct {
    tp: TokenType,
    str: []const u8,
};

const TokenRepresentations = [_]TokenStringRepr{
    .{ .tp = .if_kw, .str = "if" },
    .{ .tp = .else_kw, .str = "else" },
    .{ .tp = .fn_kw, .str = "fn" },
    .{ .tp = .return_kw, .str = "return" },
    .{ .tp = .semicolon, .str = ";" },
    .{ .tp = .comma, .str = "," },
    .{ .tp = .lparen, .str = "(" },
    .{ .tp = .rparen, .str = ")" },
    .{ .tp = .lbrace, .str = "{" },
    .{ .tp = .rbrace, .str = "}" },
    .{ .tp = .add, .str = "+" },
    .{ .tp = .subtract, .str = "-" },
    .{ .tp = .multiply, .str = "*" },
    .{ .tp = .divide, .str = "/" },
    .{ .tp = .modulo, .str = "%" },
    .{ .tp = .bit_or, .str = "|" },
    .{ .tp = .bit_and, .str = "&" },
    .{ .tp = .bit_xor, .str = "^" },
    .{ .tp = .bit_not, .str = "~" },
    .{ .tp = .log_or, .str = "||" },
    .{ .tp = .log_and, .str = "&&" },
    .{ .tp = .log_not, .str = "!" },
    .{ .tp = .le, .str = "<" },
    .{ .tp = .le_eq, .str = "<=" },
    .{ .tp = .gr, .str = ">" },
    .{ .tp = .gr_eq, .str = ">=" },
    .{ .tp = .eq, .str = "==" },
    .{ .tp = .n_eq, .str = "!=" },
};

pub fn tokenize(allocator: Allocator, input_str: []const u8) !std.ArrayList(Token) {
    var res = std.ArrayList(Token).init(allocator);

    const utils = struct {
        fn append_res(str: []const u8, re: *std.ArrayList(Token)) !void {
            // std.debug.print("{s} {}\n", .{ str, str.len });
            if (str.len == 0) {
                return;
            }

            inline for (TokenRepresentations) |tok| {
                if (std.mem.eql(u8, str, tok.str)) {
                    try re.append(Token.new(tok.tp, str));
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
    var is_all_operator_chars = true;

    const delimiting_chars = "{}();!~+-/,";
    const operator_chars = "&|<>=";
    for (input_str, 0..) |ch, i| {
        if (std.mem.indexOf(u8, " \t\r\n", &[_]u8{ch}) != null) {
            try utils.append_res(input_str[start..end], &res);

            // discard whitespace
            start = i + 1;
            end = i + 1;
            is_all_operator_chars = true;
            continue;
        }

        const ch_is_operator_char = std.mem.indexOf(u8, operator_chars, &[_]u8{ch}) != null;
        const ch_is_delimiting_char = std.mem.indexOf(u8, delimiting_chars, &[_]u8{ch}) != null;
        const buf_is_delimiting_char = std.mem.indexOfAny(u8, input_str[start..end], delimiting_chars) != null;

        const buf_is_two_char_operator = is_all_operator_chars and end - start == 2;
        const buf_is_operation_followed_by_operator = !is_all_operator_chars and end - start >= 1 and ch_is_operator_char;
        const buf_is_operator_followed_by_operation = is_all_operator_chars and end - start == 1 and !ch_is_operator_char;

        if (ch_is_delimiting_char or
            buf_is_delimiting_char or
            buf_is_two_char_operator or
            buf_is_operation_followed_by_operator or
            buf_is_operator_followed_by_operation)
        {
            try utils.append_res(input_str[start..end], &res);
            start = i;
            end = i;
            is_all_operator_chars = true;
        }

        is_all_operator_chars = is_all_operator_chars and ch_is_operator_char;
        end += 1;
    }
    if (start < input_str.len) {
        try utils.append_res(input_str[start..end], &res);
    }

    return res;
}
