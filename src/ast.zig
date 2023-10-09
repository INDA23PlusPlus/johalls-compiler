const std = @import("std");
const Allocator = std.mem.Allocator;
const Token = @import("tokenize.zig").Token;

const Literal = struct {
    value: i64,

    fn parse(tok: Token) Literal {
        _ = tok;
    }

    fn deinit(self: UnaryExpr, allocator: Allocator) void {
        _ = allocator;
        _ = self;
    }
};

const Identifier = struct {
    name: []const u8,
};

const UnaryExpr = struct {
    op: enum {
        log_not,
        bin_not,
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
    bin: BinaryExpr,
    un: UnaryExpr,
    fn_call: FunctionCall,

    fn deinit(self: Expression, allocator: Allocator) void {
        switch (self) {
            .bin => |a| a.deinit(allocator),
            .un => |a| a.deinit(allocator),
            .fn_call => |a| a.deinit(allocator),
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
    condition: *Expression,
    true_branch: Block,
    false_branch: Block,

    fn deinit(self: If, allocator: Allocator) void {
        self.condition.deinit(allocator);
        allocator.destroy(self.condition);
    }
};

const Statement = union(enum) {
    empty,
    expr: Expression,

    fn deinit(self: Statement, allocator: Allocator) void {
        switch (self) {
            .empty => {},
            .expr => |e| e.deinit(allocator),
        }
    }
};

const Block = struct {
    statements: *std.ArrayList(Statement),

    fn parse(allocator: Allocator, tokens: *[]const Token) !Block {
        var s = tokens.*;
        var res: Block = undefined;
        res.statements = try allocator.create(std.ArrayList(Statement));
        res.statements.* = std.ArrayList(Statement).init(allocator);

        if (s.len < 2) {
            return error.NotEnoughTokens;
        }

        if (s[0].tp != .lbrace) {
            return error.IncorrectToken;
        }

        s = s[1..];
        var brace_count: usize = 1;

        while (brace_count > 0) {
            brace_count += @intFromBool(s[0].tp == .lbrace);
            brace_count -= @intFromBool(s[0].tp == .rbrace);
            s = s[1..];
        }

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
