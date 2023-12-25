const std = @import("std");
const Allocator = std.mem.Allocator;
const token = @import("token.zig");
const Token = token.Token;
const TokenType = token.TokenType;

const ArenaAllocator = std.heap.ArenaAllocator;

pub const Literal = struct {
    value: i64,

    fn has_side_effects(self: *const @This(), functions_with_side_effects: [][]const u8) bool {
        _ = self;
        _ = functions_with_side_effects;
        return false;
    }

    fn parse(tok: Token) Literal {
        return Literal{ .value = std.fmt.parseInt(i64, tok.str, 10) catch |e| std.debug.panic("{}\n", .{e}) };
    }
};

pub const Identifier = struct {
    name: []const u8,

    fn has_side_effects(self: *const @This(), functions_with_side_effects: [][]const u8) bool {
        _ = self;
        _ = functions_with_side_effects;
        _ = functions_with_side_effects;
        return false;
    }

    fn parse(tok: Token) Identifier {
        return Identifier{ .name = tok.str };
    }
};

pub const UnaryExpr = struct {
    op: TokenType,
    expr: *Expression,

    fn has_side_effects(self: *const @This(), functions_with_side_effects: [][]const u8) bool {
        return self.expr.has_side_effects(functions_with_side_effects);
    }
};

pub const BinaryExpr = struct {
    op: TokenType,
    lexpr: *Expression,
    rexpr: *Expression,

    fn has_side_effects(self: *const @This(), functions_with_side_effects: [][]const u8) bool {
        return self.lexpr.has_side_effects(functions_with_side_effects) or self.rexpr.has_side_effects(functions_with_side_effects);
    }
};

pub const FunctionCall = struct {
    function_name: Identifier,
    arguments: std.ArrayList(Expression),

    fn has_side_effects(self: *const @This(), functions_with_side_effects: [][]const u8) bool {
        for (self.arguments.items) |a| {
            if (a.has_side_effects(functions_with_side_effects)) {
                return true;
            }
        }
        for (functions_with_side_effects) |f| {
            if (std.mem.eql(u8, self.function_name.name, f)) {
                return true;
            }
        }
        return false;
    }
};

pub const ParenthesizedExpression = struct {
    child: *Expression,

    fn has_side_effects(self: *const @This(), functions_with_side_effects: [][]const u8) bool {
        return self.child.has_side_effects(functions_with_side_effects);
    }
};

pub const Expression = union(enum) {
    lit: Literal,
    ident: Identifier,
    bin: BinaryExpr,
    un: UnaryExpr,
    fn_call: FunctionCall,
    paren: ParenthesizedExpression,

    fn has_side_effects(self: *const @This(), functions_with_side_effects: [][]const u8) bool {
        return switch (self.*) {
            inline else => |v| v.has_side_effects(functions_with_side_effects),
        };
    }

    fn parse(allocator: Allocator, tokens: *[]const Token) !Expression {
        var s = tokens.*;
        var res: Expression = undefined;

        if (s[0].is_unary_operator()) {
            res = Expression{ .un = UnaryExpr{ .op = s[0].tp, .expr = try allocator.create(Expression) } };
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
            if (s[0].tp == .lparen) {
                const temp = res;
                res = Expression{ .fn_call = FunctionCall{ .arguments = std.ArrayList(Expression).init(allocator), .function_name = temp.ident } };

                s = s[1..];
                while (s[0].tp != .rparen) {
                    try res.fn_call.arguments.append(try Expression.parse(allocator, &s));
                    if (s[0].tp == .comma) {
                        s = s[1..];
                    }
                }
                s = s[1..];
            }
        } else if (s[0].tp == .lparen) {
            s = s[1..]; // discard left paren
            res = Expression{ .paren = ParenthesizedExpression{ .child = try allocator.create(Expression) } };
            res.paren.child.* = try Expression.parse(allocator, &s);
            s = s[1..]; // discard right paren
        }

        while (s.len > 0 and s[0].is_binary_operator()) {
            const temp = res;
            res = Expression{ .bin = BinaryExpr{ .lexpr = try allocator.create(Expression), .op = s[0].tp, .rexpr = try allocator.create(Expression) } };
            res.bin.lexpr.* = temp;
            s = s[1..]; // discard operator token
            res.bin.rexpr.* = try Expression.parse(allocator, &s);
        }

        tokens.* = s;

        return res;
    }
    fn check(
        self: @This(),
        functions: []const Function,
        variables: *std.ArrayList(Identifier),
        allocator: Allocator,
    ) !void {
        const utils = struct {
            fn var_exists(id: Identifier, vars: *std.ArrayList(Identifier)) !void {
                var found = false;

                for (vars.items) |e| {
                    if (std.mem.eql(u8, e.name, id.name)) {
                        found = true;
                    }
                }

                if (!found) {
                    return error.UnknownIdentifer;
                }
            }
            fn fn_exists(id: Identifier, fns: []const Function) !void {
                var found = false;

                for (fns) |e| {
                    if (std.mem.eql(u8, e.name, id.name)) {
                        found = true;
                    }
                }

                if (!found) {
                    std.debug.print("{s}\n", .{id.name});
                    return error.UnknownIdentifer;
                }
            }
        };
        switch (self) {
            .lit => {},
            .ident => |e| {
                try utils.var_exists(e, variables);
            },
            .bin => |e| {
                try e.lexpr.check(functions, variables, allocator);
                try e.rexpr.check(functions, variables, allocator);
            },
            .un => |e| {
                try e.expr.check(functions, variables, allocator);
            },
            .fn_call => |f| {
                try utils.fn_exists(f.function_name, functions);
                for (f.arguments.items) |e| {
                    try e.check(functions, variables, allocator);
                }
            },
            .paren => |e| {
                try e.child.check(functions, variables, allocator);
            },
        }
    }
};

test "destruction of expressions" {
    const leaf = try std.testing.allocator.create(Expression);
    leaf.* = Expression{ .lit = Literal{ .value = 0 } };

    var root: Expression = Expression{ .un = UnaryExpr{ .op = .log_not, .expr = leaf } };
    defer root.deinit(std.testing.allocator);
}

const If = struct {
    condition: Expression,
    true_branch: Block,
    false_branch: Block,

    fn has_side_effects(self: *const @This(), functions_with_side_effects: [][]const u8) bool {
        return self.condition.has_side_effects(functions_with_side_effects) or self.true_branch.has_side_effects(functions_with_side_effects) or self.false_branch.has_side_effects(functions_with_side_effects);
    }

    fn parse(allocator: Allocator, tokens: *[]const Token) anyerror!If {
        var s = tokens.*;
        var res: If = undefined;

        if (s.len < 4) {
            return error.NotEnoughTokens;
        }

        s = s[1..];
        res.condition = try Expression.parse(allocator, &s);

        if (s[0].tp != .lbrace)
            std.debug.panic("should be lbrace {}\n", .{s[0].tp});

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
    fn check(
        self: @This(),
        functions: []const Function,
        variables: *std.ArrayList(Identifier),
        allocator: Allocator,
    ) !void {
        try self.condition.check(functions, variables, allocator);
        try self.true_branch.check(functions, variables, allocator);
        try self.false_branch.check(functions, variables, allocator);
    }
};

const Return = struct {
    returned_value: Expression,

    fn has_side_effects(self: *const @This(), functions_with_side_effects: [][]const u8) bool {
        return self.returned_value.has_side_effects(functions_with_side_effects);
    }
    fn parse(allocator: Allocator, tokens: *[]const Token) !Return {
        tokens.* = tokens.*[1..];
        const res = Return{ .returned_value = try Expression.parse(allocator, tokens) };
        tokens.* = tokens.*[1..];
        return res;
    }
    fn check(
        self: @This(),
        functions: []const Function,
        variables: *std.ArrayList(Identifier),
        allocator: Allocator,
    ) !void {
        try self.returned_value.check(functions, variables, allocator);
    }
};

const Assignment = struct {
    id: Identifier,
    value: Expression,

    fn has_side_effects(self: *const @This(), functions_with_side_effects: [][]const u8) bool {
        return self.value.has_side_effects(functions_with_side_effects);
    }

    fn parse(allocator: Allocator, tokens: *[]const Token) !Assignment {
        try std.testing.expect(tokens.*[0].tp == .let_kw);
        tokens.* = tokens.*[1..]; // discard "let"

        const id = Identifier.parse(tokens.*[0]);

        try std.testing.expect(tokens.*[0].tp == .identifier);
        tokens.* = tokens.*[1..]; // discard identifier

        try std.testing.expect(tokens.*[0].tp == .assign);
        tokens.* = tokens.*[1..]; // discard "="

        const value = try Expression.parse(allocator, tokens);

        const res = Assignment{ .id = id, .value = value };

        try std.testing.expect(tokens.*[0].tp == .semicolon);
        tokens.* = tokens.*[1..]; // discard ";"

        return res;
    }
    fn check(
        self: @This(),
        functions: []const Function,
        variables: *std.ArrayList(Identifier),
        allocator: Allocator,
    ) !void {
        for (variables.items) |v| {
            if (std.mem.eql(u8, self.id.name, v.name)) {
                return error.VariableRedefinition;
            }
        }
        try self.value.check(functions, variables, allocator);
    }
};

pub const Statement = union(enum) {
    expr: Expression, // this is needed to allow print statements
    ret: Return,
    conditional: If,
    assign: Assignment,

    fn has_side_effects(self: *const @This(), functions_with_side_effects: [][]const u8) bool {
        return switch (self.*) {
            inline else => |v| v.has_side_effects(functions_with_side_effects),
        };
    }

    fn parse(allocator: Allocator, tokens: *[]const Token) !Statement {
        return switch (tokens.*[0].tp) {
            .if_kw => Statement{ .conditional = try If.parse(allocator, tokens) },
            .return_kw => Statement{ .ret = try Return.parse(allocator, tokens) },
            .let_kw => Statement{ .assign = try Assignment.parse(allocator, tokens) },
            else => {
                const res = Statement{ .expr = try Expression.parse(allocator, tokens) };
                tokens.* = tokens.*[1..]; // discard semicolon
                return res;
            },
        };
    }
    fn check(
        self: @This(),
        functions: []const Function,
        variables: *std.ArrayList(Identifier),
        allocator: Allocator,
    ) !void {
        switch (self) {
            .expr => |e| {
                try e.check(functions, variables, allocator);
            },
            .ret => |e| {
                try e.check(functions, variables, allocator);
            },
            .conditional => |e| {
                try e.check(functions, variables, allocator);
            },
            .assign => |e| {
                try e.check(functions, variables, allocator);
            },
        }
    }

    fn always_reaches_return(self: @This()) bool {
        return switch (self) {
            .ret => true,
            .expr => false,
            .conditional => |c| c.true_branch.always_reaches_return() and c.false_branch.always_reaches_return(),
            .assign => false,
        };
    }
};

const Block = struct {
    statements: *std.ArrayList(Statement),

    fn empty(allocator: Allocator) !Block {
        var res: Block = undefined;
        res.statements = try allocator.create(std.ArrayList(Statement));
        res.statements.* = try std.ArrayList(Statement).initCapacity(allocator, 16);
        return res;
    }

    fn has_side_effects(self: *const @This(), functions_with_side_effects: [][]const u8) bool {
        for (self.statements.items) |st| {
            if (st.has_side_effects(functions_with_side_effects)) {
                return true;
            }
        }
        return false;
    }

    fn parse(allocator: Allocator, tokens: *[]const Token) !Block {
        var s = tokens.*;

        var res = try Block.empty(allocator);

        if (s.len < 2) {
            return error.NotEnoughTokens;
        }

        if (s[0].tp != .lbrace) {
            try res.statements.append(try Statement.parse(allocator, &s));
            tokens.* = s;
            return res;
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

    fn check(
        self: @This(),
        functions: []const Function,
        variables: *std.ArrayList(Identifier),
        allocator: Allocator,
    ) anyerror!void {
        var num_defined_variables: usize = 0;
        for (self.statements.items) |st| {
            try st.check(functions, variables, allocator);
            switch (st) {
                .assign => |s| {
                    try variables.append(s.id);
                    num_defined_variables += 1;
                },
                else => {},
            }
        }

        for (0..num_defined_variables) |_| {
            _ = variables.pop();
        }
    }

    fn always_reaches_return(self: @This()) bool {
        for (self.statements.items) |st| {
            if (st.always_reaches_return()) return true;
        }
        return false;
    }
};

pub const Function = struct {
    children: Block,
    name: []const u8,
    params: std.ArrayList(Identifier),

    fn has_side_effects(self: *const @This(), functions_with_side_effects: [][]const u8) bool {
        return self.children.has_side_effects(functions_with_side_effects);
    }

    fn parse(allocator: Allocator, tokens: *[]const Token) !Function {
        var res: Function = undefined;
        res.params = std.ArrayList(Identifier).init(allocator);
        var s = tokens.*;
        if (s.len < 6) {
            return error.NotEnoughTokens;
        }
        if (s[0].tp != .fn_kw or s[1].tp != .identifier or s[2].tp != .lparen) {
            return error.IncorrectToken;
        }
        res.name = s[1].str;
        s = s[3..];
        var paren_count: usize = 1;
        while (paren_count > 0) {
            if (s[0].tp == .identifier) {
                try res.params.append(Identifier.parse(s[0]));
            }
            paren_count += @intFromBool(s[0].tp == .lparen);
            paren_count -= @intFromBool(s[0].tp == .rparen);
            s = s[1..];
        }
        // std.debug.print("parsing function {}\n", .{s[0].tp});
        tokens.* = s;

        res.children = try Block.parse(allocator, tokens);

        return res;
    }

    fn check(self: Function, functions: []const Function, allocator: Allocator) !void {
        var variables = try std.ArrayList(Identifier).initCapacity(allocator, self.params.items.len);
        for (self.params.items) |i| {
            try variables.append(i);
        }
        defer variables.deinit();
        try self.children.check(functions, &variables, allocator);
        if (!self.children.always_reaches_return()) {
            return error.NoReturnStatement;
        }
    }
};

pub const AST = struct {
    functions: std.ArrayList(Function),
    alloc: std.heap.ArenaAllocator,

    pub fn parse(allocator: Allocator, tokens: []const Token) !AST {
        var res = AST{ .functions = std.ArrayList(Function).init(allocator), .alloc = std.heap.ArenaAllocator.init(allocator) };

        var tokens_copy = tokens; // cant mutate the parameter

        while (tokens_copy.len > 0) {
            try res.functions.append(try Function.parse(res.alloc.allocator(), &tokens_copy));
        }
        try res.functions.append(Function{ .children = undefined, .params = std.ArrayList(Identifier).init(allocator), .name = "input" });
        var params = try std.ArrayList(Identifier).initCapacity(res.alloc.allocator(), 1);
        params.appendAssumeCapacity(Identifier{ .name = "x" });
        try res.functions.append(Function{ .children = undefined, .params = params, .name = "print" });

        return res;
    }

    pub fn check(self: *AST) !void {
        var has_main = false;
        for (self.functions.items) |f| {
            if (std.mem.eql(u8, f.name, "input") or std.mem.eql(u8, f.name, "print"))
                continue;
            if (std.mem.eql(u8, f.name, "main"))
                has_main = true;
            try f.check(self.functions.items, self.alloc.allocator());
        }
        if (!has_main) return error.NoMainFunction;
    }

    // TODO: clean this mess up
    pub fn as_c(self: *const @This(), allocator: Allocator) !std.ArrayList(u8) {
        const preamble =
            \\#include <inttypes.h>
            \\#include <stdarg.h>
            \\#include <stdbool.h>
            \\#include <stdint.h>
            \\#include <stdio.h>
            \\#include <stdlib.h>
            \\#include <string.h>
            \\
            \\#define NUMARGS(...) (sizeof((int64_t[]){__VA_ARGS__}) / sizeof(int64_t))
            \\#define HASH(...) (_get_hash(NUMARGS(__VA_ARGS__), __VA_ARGS__))
            \\
            \\#define CACHE_SIZE 65537
            \\
            \\uint64_t _get_hash(int numargs, ...) {
            \\    uint64_t result = 194114084445485833;
            \\    va_list ap;
            \\
            \\    va_start(ap, numargs);
            \\    while (numargs--)
            \\        result = result * 31 ^ (uint64_t) va_arg(ap, int64_t);
            \\    va_end(ap);
            \\    if (result == 0) return 1; // could cause bug with initialization of cache if hash was 0
            \\    return result;
            \\}
            \\
            \\int64_t _dpp_input(void) {
            \\    int64_t result = 0;
            \\    if (scanf("%" SCNd64, &result) < 1) exit(-1);
            \\    return result;
            \\}
            \\
            \\int64_t _dpp_print(int64_t x) {
            \\    (void) printf("%" PRId64 "\n", x);
            \\    return 0;
            \\}
            \\
            \\
        ;
        var functions_with_side_effects = try std.ArrayList([]const u8).initCapacity(allocator, 2);
        defer functions_with_side_effects.deinit();

        var functions_with_side_effects_set = std.StringArrayHashMap(void).init(allocator);
        defer functions_with_side_effects_set.deinit();

        functions_with_side_effects.appendAssumeCapacity("input");
        functions_with_side_effects.appendAssumeCapacity("print");

        _ = try functions_with_side_effects_set.getOrPut("input");
        _ = try functions_with_side_effects_set.getOrPut("print");
        { // yes this is slow, sue me
            var changed = true;
            while (changed) {
                changed = false;
                for (self.functions.items) |f| {
                    if (functions_with_side_effects_set.contains(f.name)) {
                        continue;
                    }
                    if (f.has_side_effects(functions_with_side_effects.items)) {
                        changed = true;
                        try functions_with_side_effects.append(f.name);
                        _ = try functions_with_side_effects_set.getOrPut(f.name);
                    }
                }
            }
        }
        var res = try std.ArrayList(u8).initCapacity(allocator, preamble.len);
        const utils = struct {
            fn print_expr(out: *std.ArrayList(u8), ex: Expression) !void {
                switch (ex) {
                    .bin => |b| {
                        try print_expr(out, b.lexpr.*);
                        try out.append(' ');
                        try out.appendSlice(b.op.get_representation().?);
                        try out.append(' ');
                        try print_expr(out, b.rexpr.*);
                    },
                    .un => |u| {
                        try out.appendSlice(u.op.get_representation().?);
                        try print_expr(out, u.expr.*);
                    },
                    .lit => |l| {
                        var buf: [24]u8 = undefined;
                        try out.appendSlice(try std.fmt.bufPrint(&buf, "{}", .{l.value}));
                    },
                    .ident => |i| {
                        try out.appendSlice("_dpp_");
                        try out.appendSlice(i.name);
                    },
                    .paren => |p| {
                        try out.append('(');
                        try print_expr(out, p.child.*);
                        try out.append(')');
                    },
                    .fn_call => |f| {
                        try out.appendSlice("_dpp_");
                        try out.appendSlice(f.function_name.name);
                        try out.append('(');
                        var first = true;
                        for (f.arguments.items) |arg| {
                            if (!first) {
                                try out.appendSlice(", ");
                            }
                            first = false;
                            try print_expr(out, arg);
                        }
                        try out.append(')');
                    },
                }
            }

            fn print_indentation(out: *std.ArrayList(u8), indentation_level: usize) !void {
                try out.appendNTimes(' ', indentation_level);
            }

            fn print_statement(out: *std.ArrayList(u8), st: Statement, indentation_level: usize) !void {
                switch (st) {
                    .assign => |a| {
                        try print_indentation(out, indentation_level);
                        try out.appendSlice("int64_t ");
                        try out.appendSlice(a.id.name);
                        try out.appendSlice(" = ");
                        try print_expr(out, a.value);
                        try out.appendSlice(";\n");
                    },
                    .conditional => |i| {
                        try print_indentation(out, indentation_level);
                        try out.appendSlice("if (");
                        try print_expr(out, i.condition);
                        try out.appendSlice(") {\n");
                        for (i.true_branch.statements.items) |nx| {
                            try print_statement(out, nx, indentation_level + 4);
                        }
                        try print_indentation(out, indentation_level);
                        try out.append('}');
                        if (i.false_branch.statements.items.len > 0) {
                            try out.appendSlice(" else {\n");
                            for (i.false_branch.statements.items) |nx| {
                                try print_statement(out, nx, indentation_level + 4);
                            }
                            try print_indentation(out, indentation_level);
                            try out.appendSlice("}\n");
                        } else {
                            try out.append('\n');
                        }
                    },
                    .expr => |ex| {
                        try print_indentation(out, indentation_level);
                        try print_expr(out, ex);
                        try out.appendSlice(";\n");
                    },
                    .ret => |ret| {
                        try print_indentation(out, indentation_level);
                        try out.appendSlice("return ");
                        try print_expr(out, ret.returned_value);
                        try out.appendSlice(";\n");
                    },
                }
            }

            fn print_param_list(out: *std.ArrayList(u8), idents: []const Identifier, is_declaration: bool) !void {
                var first = true;
                for (idents) |param| {
                    if (!first) {
                        try out.appendSlice(", ");
                    }
                    first = false;
                    if (is_declaration) {
                        try out.appendSlice("int64_t ");
                    }
                    try out.appendSlice("_dpp_");
                    try out.appendSlice(param.name);
                }
                if (idents.len == 0 and is_declaration) {
                    try out.appendSlice("void");
                }
            }
        };
        try res.appendSlice(preamble);

        for (self.functions.items) |fun| {
            if (std.mem.eql(u8, fun.name, "input") or std.mem.eql(u8, fun.name, "print")) continue;

            const fn_name = fun.name;

            try res.appendSlice("int64_t _dpp_");
            try res.appendSlice(fn_name);

            try res.append('(');
            try utils.print_param_list(&res, fun.params.items, true);
            try res.appendSlice(");\n");
        }
        try res.append('\n');
        for (self.functions.items) |fun| {
            if (std.mem.eql(u8, fun.name, "input") or std.mem.eql(u8, fun.name, "print")) continue;

            const pure_function = !fun.has_side_effects(functions_with_side_effects.items);

            const fn_name = fun.name;
            try res.appendSlice("int64_t ");
            if (pure_function and fun.params.items.len > 0) {
                try res.appendSlice("_dpp_");
                try std.fmt.format(res.writer(), "{s}_impl(", .{fn_name});
                try utils.print_param_list(&res, fun.params.items, true);
                try res.appendSlice(");\n");

                try std.fmt.format(res.writer(), "int64_t _dpp_{s}(", .{fun.name});
                try utils.print_param_list(&res, fun.params.items, true);
                try res.appendSlice(") {\n");

                try res.appendSlice("    struct CacheEntry {\n");
                for (fun.params.items) |param| {
                    try std.fmt.format(res.writer(), "        int64_t {s};\n", .{param.name});
                }
                try res.appendSlice("        uint64_t _hash;\n");
                try res.appendSlice("        int64_t _cached_value;\n");
                try res.appendSlice("    };\n");
                try res.appendSlice("    static struct CacheEntry _cache[CACHE_SIZE];\n");

                try res.appendSlice("    uint64_t hash = HASH(");
                try utils.print_param_list(&res, fun.params.items, false);
                try res.appendSlice(");\n");

                try res.appendSlice("    if (_cache[hash % CACHE_SIZE]._hash == hash) {\n");

                try res.appendSlice("        if (");
                var first = true;
                for (fun.params.items) |param| {
                    if (!first) {
                        try res.appendSlice(" & ");
                    }
                    first = false;

                    try std.fmt.format(res.writer(), "(int) (_cache[hash % CACHE_SIZE].{s} == _dpp_{s})", .{ param.name, param.name });
                }
                try res.appendSlice(") {\n");
                try res.appendSlice("            return _cache[hash % CACHE_SIZE]._cached_value;\n");
                try res.appendSlice("        }\n");

                try res.appendSlice("    }\n");
                try res.appendSlice("    struct CacheEntry entry;\n");
                for (fun.params.items) |param| {
                    try std.fmt.format(res.writer(), "    entry.{s} = _dpp_{s};\n", .{ param.name, param.name });
                }
                try res.appendSlice("    entry._hash = hash;\n");
                try std.fmt.format(res.writer(), "    entry._cached_value = _dpp_{s}_impl(", .{fun.name});
                try utils.print_param_list(&res, fun.params.items, false);
                try res.appendSlice(");\n");
                try res.appendSlice("    memcpy(&_cache[hash % CACHE_SIZE], &entry, sizeof(struct CacheEntry));\n");
                try res.appendSlice("    return entry._cached_value;\n");

                try res.appendSlice("}\n\n");

                try std.fmt.format(res.writer(), "int64_t _dpp_{s}_impl", .{fn_name});
            } else {
                try res.appendSlice("_dpp_");
                try res.appendSlice(fn_name);
            }
            try res.append('(');
            try utils.print_param_list(&res, fun.params.items, true);
            try res.appendSlice(") {\n");

            for (fun.children.statements.items) |statement| {
                try utils.print_statement(&res, statement, 4);
            }
            try res.appendSlice("}\n\n");
        }
        try res.appendSlice("int main(void) { return (int) _dpp_main(); }");

        while (res.getLast() == '\n') _ = res.pop();

        return res;
    }

    pub fn deinit(self: AST) void {
        self.alloc.deinit();
        self.functions.deinit();
    }
};
