const std = @import("std");
const Allocator = std.mem.Allocator;
const Token = @import("tokenize.zig").Token;

const Literal = struct {
    value: i64,

    fn parse(tok: Token) Literal {
        return Literal{ .value = std.fmt.parseInt(i64, tok.str, 10) catch |e| std.debug.panic("{}\n", .{e}) };
    }
};

const Identifier = struct {
    name: []const u8,

    fn parse(tok: Token) Identifier {
        return Identifier{ .name = tok.str };
    }
};

const UnaryExpr = struct {
    op: enum {
        log_not,
        bit_not,
    },
    expr: *Expression,
    fn deinit(self: UnaryExpr, allocator: Allocator) void {
        self.expr.deinit(allocator);
        allocator.destroy(self.expr);
    }
};

const BinaryExpr = struct {
    op: enum {
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
    },
    lexpr: *Expression,
    rexpr: *Expression,
    fn deinit(self: BinaryExpr, allocator: Allocator) void {
        self.lexpr.deinit(allocator);
        self.rexpr.deinit(allocator);
        allocator.destroy(self.lexpr);
        allocator.destroy(self.rexpr);
    }
};

const FunctionCall = struct {
    function_name: Identifier,
    arguments: std.ArrayList(Expression),

    fn deinit(self: FunctionCall, allocator: Allocator) void {
        for (self.arguments.items) |arg| {
            arg.deinit(allocator);
        }
        self.arguments.deinit();
    }
};

const Expression = union(enum) {
    lit: Literal,
    ident: Identifier,
    bin: BinaryExpr,
    un: UnaryExpr,
    fn_call: FunctionCall,

    fn parse(allocator: Allocator, tokens: *[]const Token) !Expression {
        var s = tokens.*;
        var res: Expression = undefined;

        if (s[0].is_unary_operator()) {
            res = Expression{ .un = UnaryExpr{ .op = switch (s[0].tp) {
                .bit_not => .bit_not,
                .log_not => .log_not,
                else => unreachable,
            }, .expr = try allocator.create(Expression) } };
            tokens.* = tokens.*[1..];
            res.un.expr.* = try Expression.parse(allocator, tokens);
            return res;
        }

        if (s.len == 0) {
            return error.NotEnoughTokens;
        }

        if (s[0].tp == .literal) {
            res = Expression{ .lit = Literal.parse(s[0]) };
            s = s[1..];
        } else if (s[0].tp == .identifier) {
            res = Expression{ .ident = Identifier.parse(s[0]) };
            s = s[1..];
        }

        while (s.len > 0 and s[0].is_binary_operator()) {
            var temp = res;
            res = Expression{ .bin = BinaryExpr{ .lexpr = try allocator.create(Expression), .op = switch (s[0].tp) {
                .add => .add,
                .subtract => .subtract,
                .multiply => .multiply,
                .divide => .divide,
                .bit_or => .bit_or,
                .bit_and => .bit_and,
                .bit_xor => .bit_xor,
                .log_or => .log_or,
                .log_and => .log_and,
                .le => .le,
                .le_eq => .le_eq,
                .gr => .gr,
                .gr_eq => .gr_eq,
                .eq => .eq,
                .n_eq => .n_eq,
                else => unreachable,
            }, .rexpr = try allocator.create(Expression) } };
            res.bin.lexpr.* = temp;
            s = s[1..];

            res.bin.rexpr.* = try Expression.parse(allocator, &s);
        }

        tokens.* = s;

        return res;
    }

    fn deinit(self: Expression, allocator: Allocator) void {
        switch (self) {
            .bin => |a| a.deinit(allocator),
            .un => |a| a.deinit(allocator),
            .fn_call => |a| a.deinit(allocator),
            .ident => {},
            .lit => {},
        }
    }
};

test "destruction of expressions" {
    var leaf = try std.testing.allocator.create(Expression);
    leaf.* = Expression{ .lit = Literal{ .value = 0 } };

    var root: Expression = Expression{ .un = UnaryExpr{ .op = .log_not, .expr = leaf } };
    defer root.deinit(std.testing.allocator);
}

const If = struct {
    condition: Expression,
    true_branch: Block,
    false_branch: Block,

    fn parse(allocator: Allocator, tokens: *[]const Token) anyerror!If {
        var s = tokens.*;
        var res: If = undefined;

        if (s.len < 4) {
            return error.NotEnoughTokens;
        }

        s = s[1..];
        res.condition = try Expression.parse(allocator, &s);

        std.debug.print("should be lbrace {}\n", .{s[0].tp});

        res.true_branch = try Block.parse(allocator, &s);

        if (s[0].tp == .else_kw) {
            s = s[1..];
            res.false_branch = try Block.parse(allocator, &s);
        } else {
            res.false_branch = try Block.empty(allocator);
        }

        tokens.* = s;

        return res;
    }

    fn deinit(self: If, allocator: Allocator) void {
        self.condition.deinit(allocator);
        self.true_branch.deinit(allocator);
        self.false_branch.deinit(allocator);
    }
};

const Return = struct {
    returned_value: Expression,

    fn parse(allocator: Allocator, tokens: *[]const Token) !Return {
        tokens.* = tokens.*[1..];
        const res = Return{ .returned_value = try Expression.parse(allocator, tokens) };
        tokens.* = tokens.*[1..];
        return res;
    }

    fn deinit(self: Return, allocator: Allocator) void {
        self.returned_value.deinit(allocator);
    }
};

const Statement = union(enum) {
    expr: Expression, // this is needed to allow print statements
    ret: Return,
    conditional: If,

    fn parse(allocator: Allocator, tokens: *[]const Token) !Statement {
        return switch (tokens.*[0].tp) {
            .if_kw => Statement{ .conditional = try If.parse(allocator, tokens) },
            .return_kw => Statement{ .ret = try Return.parse(allocator, tokens) },
            else => {
                const res = Statement{ .expr = try Expression.parse(allocator, tokens) };
                tokens.* = tokens.*[1..]; // discard semicolon
                return res;
            },
        };
    }

    fn deinit(self: Statement, allocator: Allocator) void {
        switch (self) {
            .expr => |a| a.deinit(allocator),
            .ret => |a| a.deinit(allocator),
            .conditional => |a| a.deinit(allocator),
        }
    }
};

const Block = struct {
    statements: *std.ArrayList(Statement),

    fn empty(allocator: Allocator) !Block {
        var res: Block = undefined;
        res.statements = try allocator.create(std.ArrayList(Statement));
        res.statements.* = std.ArrayList(Statement).init(allocator);
        return res;
    }

    fn parse(allocator: Allocator, tokens: *[]const Token) !Block {
        var s = tokens.*;

        var res = try Block.empty(allocator);

        if (s.len < 2) {
            return error.NotEnoughTokens;
        }

        if (s[0].tp != .lbrace) {
            return error.IncorrectToken;
        }

        // discard left brace
        s = s[1..];
        while (s[0].tp != .rbrace) {
            try res.statements.append(try Statement.parse(allocator, &s));
        }

        // discard right brace
        s = s[1..];

        tokens.* = s;

        return res;
    }

    fn deinit(self: Block, allocator: Allocator) void {
        for (self.statements.items) |statement| {
            statement.deinit(allocator);
        }
        self.statements.deinit();
        allocator.destroy(self.statements);
    }
};

pub const Function = struct {
    children: Block,
    name_tok: Token,
    params: std.ArrayList(Token),

    fn parse(allocator: Allocator, tokens: *[]const Token) !Function {
        var res: Function = undefined;
        res.params = std.ArrayList(Token).init(allocator);
        var s = tokens.*;
        if (s.len < 6) {
            return error.NotEnoughTokens;
        }
        if (s[0].tp != .fn_kw or s[1].tp != .identifier or s[2].tp != .lparen) {
            return error.IncorrectToken;
        }
        res.name_tok = s[1];
        s = s[3..];
        var paren_count: usize = 1;
        while (paren_count > 0) {
            if (s[0].tp == .identifier) {
                try res.params.append(s[0]);
            }
            paren_count += @intFromBool(s[0].tp == .lparen);
            paren_count -= @intFromBool(s[0].tp == .rparen);
            s = s[1..];
        }
        std.debug.print("parsing function {}\n", .{s[0].tp});
        tokens.* = s;

        res.children = try Block.parse(allocator, tokens);

        return res;
    }

    fn deinit(self: Function, allocator: Allocator) void {
        self.params.deinit();
        self.children.deinit(allocator);
    }
};

pub const AST = struct {
    functions: std.ArrayList(Function),

    pub fn parse(allocator: Allocator, tokens: []const Token) !AST {
        var res = AST{ .functions = std.ArrayList(Function).init(allocator) };

        var tokens_copy = tokens; // cant mutate the parameter

        while (tokens_copy.len > 0) {
            try res.functions.append(try Function.parse(allocator, &tokens_copy));
        }

        return res;
    }

    pub fn deinit(self: AST, allocator: Allocator) void {
        for (self.functions.items) |f| {
            f.deinit(allocator);
        }
        self.functions.deinit();
    }
};
