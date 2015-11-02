(function () {
    "use strict";

    function unexpected(n, msg) {
        var pos = n.loc ? " at " + n.loc.source + ":" + n.loc.start.line : "";
        var s = "Unexpected parse node type: " + n.type + pos +
            " (" + Object.getOwnPropertyNames(n).toSource() + ")";
        if (msg !== undefined)
            s += msg;
        throw new TypeError(s);
    }

    function expectType(n, t) {
        if (n.type !== t)
            unexpected(n, "(expected " + t + ")");
    }

    // Wrap the expression s in parentheses if needed.
    // xprec is the precedence of the topmost operator in the expression itself.
    // cprec is the precedence of the immediately enclosing expression
    // ("context precedence"). We need parentheses if xprec <= cprec.
    //
    // The precedence numbers agree with jsopcode.tbl. More-positive numbers
    // indicate tighter precedence.
    //
    function wrapExpr(s, cprec, xprec) {
        assertEq(arguments.length, 3);
        assertEq(typeof cprec, 'number');
        assertEq(cprec === cprec, true);
        return (xprec > cprec) ? s : "(" + s + ")";
    }

    // Decompile the statement n, indenting it and spacing it to be pasted into
    // an enclosing statement.
    //
    // Blocks are treated specially so that their braces can be cuddled up with
    // neighboring keywords. The code below that implements this reads like a
    // disgusting hack, but it produces more conventional JS output.
    //
    // If `more` is true, this substatement will be followed by the "while" of
    // a do-while loop or the "else" of an if-statement. So return a specially
    // hacked string that the subsequent keyword can just be added onto.
    //
    function substmt(n, indent, more) {
        if (n.type === "BlockStatement") {
            var body = stmt(n, indent);
            if (more)
                body = body.substring(indent.length, body.length - 1) + " ";
            else
                body = body.substring(indent.length);
            return " " + body;
        }
        return "\n" + stmt(n, indent + "    ") + (more ? indent : "");
    }

    function params(arr, indent, opts) {
        var stuff = arr.map(x => expr(x, '####', 18, false)).join(", ");
        if (opts && opts.arrow && arr.length === 1 && arr[0].type === "Identifier")
            return stuff;
        else
            return "(" + stuff + ")";
    }

    function args(arr, indent) {
        return "(" + arr.map(x => expr(x, indent, 2, false)).join(", ") + ")";
    }

    function functionDeclaration(init, id, n, indent) {
        // Note: If n represents a legacy generator, then n.generator is the
        // string "legacy" rather than true.
        if (n.generator === true)
            init += "*";
        if (init !== "")
            init += " ";

        // name is ordinarily an identifier, but literals are also legal for
        // getters and setters: ({get 1() {}})
        var name = (id === null) ? "" : expr(id, '####', 18, false);

        var body;
        if (n.expression) {
            body = expr(n.body, indent, 2, false);
            if (body.charAt(0) === '{')
                body = " (" + body + ")";
            else
                body = " " + body;
        } else {
            body = substmt(n.body, indent).trimRight();
        }

        return init + name + params(n.params, indent) + body;
    }

    function identifierName(n) {
        assertEq(n.type, "Identifier");
        return n.name;
    }

    var precedence = {
        "||": 5,
        "&&": 6,
        "|": 7,
        "^": 8,
        "&": 9,
        "==": 10,
        "!=": 10,
        "===": 10,
        "!==": 10,
        "<": 11,
        "<=": 11,
        ">": 11,
        ">=": 11,
        "in": 11,
        "instanceof": 11,
        "<<": 12,
        ">>": 12,
        ">>>": 12,
        "+": 13,
        "-": 13,
        "*": 14,
        "/": 14,
        "%": 14,
    };

    function forHead(n, kw, indent) {
        let lhs;
        if (n.left.type == "VariableDeclaration")
            lhs = n.left.kind + " " + declarators(n.left.declarations, indent, true);
        else
            lhs = expr(n.left, indent, 0, true);

        return `for ${n.each ? "each " : ""}(${lhs} ${kw} ${expr(n.right, indent, 0, false)})`;
    }

    function comprehension(n, indent) {
        let body = expr(n.body, indent, 2, false);
        let heads = [];
        for (let block of n.blocks) {
            if (block.type == "ComprehensionIf") {
                heads.push("if (" + expr(block.test, indent, 0, false) + ")")
            } else {
                heads.push(forHead(block, block.of ? "of" : "in", indent));
            }
        }
        if (n.filter)
            heads.push("if (" + expr(n.filter, indent, 0, false) + ")");

        if (n.style === "legacy")
            return body + " " + heads.join(" ");
        else // modern
            return heads.join(" ") + " " + body;
    }

    function isBadIdentifier(n) {
        return n.type === "Identifier" && !n.name.match(/^[_$A-Za-z][_$A-Za-z0-9]*$/);
    }

    function quoteChars(s, delim) {
        var qc = '';
        for (let c of s) {
            var i = c.codePointAt(0);
            if (c === "\\") {
                qc += "\\\\";
            } else if (c === "\n") {
                qc += "\\n";
            } else if (c === "\t") {
                qc += "\\t";
            } else if (c === "\r") {
                qc += "\\r";
            } else if (c === delim) {
                qc += "\\" + delim;
            } else if (i > 0xffff) {
                qc += "\\u{" + i.toString(16) + "}";
            } else if (i > 0xff || i < 0x20) {
                qc += "\\u" + ("0000" + i.toString(16)).slice(-4);
            } else {
                qc += c;
            }
        }
        return qc;
    }

    // Convert an expression object to a string.
    // cprec is the context precedence. If it is high, but n has low
    // precedence, n is automatically wrapped in parentheses.
    // if noIn is true, wrap in-expressions in parentheses.
    function expr(n, indent, cprec, noIn = false) {
        assertEq(noIn, noIn && cprec <= 11);

        switch (n.type) {
        case "ArrayExpression":
        case "ArrayPattern":
            {
                let s = '[';
                let e = n.elements;
                let len = e.length;
                for (var i = 0; i < len; i++) {
                    if (e[i] !== null) {
                        if (i != 0)
                            s += ' ';
                        s += expr(e[i], indent, 2);
                    }
                    if (i != len - 1 || e[i] === null)
                        s += ',';
                }
                return s + ']';
            }

        case "SpreadExpression":
            return "..." + expr(n.expression, indent, 2);

        case "ObjectExpression":
            {
                var s = [];
                var extraIndent = indent + INDENT_LEVEL;
                for (let prop of n.properties) {
                    let code;
                    switch (prop.type) {
                    case "Property":
                        switch (prop.kind) {
                        case "init":
                            {
                                let key = expr(prop.key, extraIndent, 18);
                                if (prop.shorthand)
                                    code = key;
                                else if (prop.method)
                                    code = functionDeclaration("", prop.key, prop.value, indent);
                                else
                                    code = key + ": " + expr(prop.value, indent, 2);
                            }
                            break;
                        case "get":
                        case "set":
                            code = functionDeclaration(prop.kind, prop.key, prop.value, indent);
                            break;
                        default:
                            code = unexpected(prop);
                        }
                        break;

                    case "PrototypeMutation":
                        code = "__proto__: " + expr(prop.value, indent, 2);
                        break;

                    default:
                        code = unexpected(prop);
                    }
                    s.push(code);
                }
                return "{" + s.join(", ") + "}";
            }

        case "ComputedName":
            return "[" + expr(n.name, indent, 2) + "]";

        case "LetExpression":
            return wrapExpr("let (" + declarators(n.head, indent, false) + ") " +
                              expr(n.body, indent, 2),
                            cprec, 3);

        case "GeneratorExpression":
            return "(" + comprehension(n, indent) + ")";

        case "ComprehensionExpression":
            return "[" + comprehension(n, indent) + "]";

        case "YieldExpression":
            // `yield a, b` is a SyntaxError; it must be parenthesized
            // `(yield a), b` or `yield (a, b)`.
            return wrapExpr("yield" +
                            (n.delegate ? "*" : "") +
                            (n.argument ? " " + expr(n.argument, indent, 2) : ""),
                            cprec, 1);

        case "SequenceExpression":
            {
                let s = [];
                let arr = n.expressions;
                for (let i = 0; i < arr.length; i++)
                    s[i] = expr(arr[i], indent, 2, noIn);
                return wrapExpr(s.join(", "), cprec, 2);
            }

        case "ConditionalExpression":
            return wrapExpr(expr(n.test, indent, 4, noIn) +
                              "?" + expr(n.consequent, indent, 0, noIn) +
                              ":" + expr(n.alternate, indent, 3, noIn),
                            cprec, 4);

        case "Identifier":
            return n.name;

        case "Literal":
            // Do not stringify NaN or Infinities as names. Also do not
            // stringify Infinity as "1 / 0", since ({1e999: 0}) is ok
            // meaning ({"Infinity": 0}). ({1 / 0: 0}) is a SyntaxError.
            if (n.value !== n.value) {
                return wrapExpr("0 / 0", cprec, 14);
            } else if (n.value === 1e999) {
                return wrapExpr("1e999", cprec, 19);
            } else if (n.value === -1e999) {
                return wrapExpr("-1e999", cprec, 15);
            } else {
                var s = uneval(n.value);
                if (cprec === 17 && s.match(/\d+/))
                    s = "(" + s + ")";  // grammar quirk: 50.toString() --> (50).toString()
                return s;
            }

        case "CallExpression":
            return wrapExpr(expr(n.callee, indent, 17) +
                             args(n.arguments, indent),
                            cprec, 18);

        case "NewExpression":
            return (n.arguments.length == 0
                    ? wrapExpr("new " + expr(n.callee, indent, 18), cprec, 17)
                    : wrapExpr("new " + expr(n.callee, indent, 18) + args(n.arguments, indent),
                               cprec, 17));

        case "ThisExpression":
            return "this";

        case "MemberExpression":
            return wrapExpr(expr(n.object, indent, 17) +
                             (n.computed
                              ? "[" + expr(n.property, indent, 0) + "]"
                              : isBadIdentifier(n.property)
                              ? "[" + uneval(n.property.name) + "]"
                              : "." + expr(n.property, indent, 18)),
                            cprec, 18);

        case "MetaProperty":
            return expr(n.meta, "####", 18) + "." + expr(n.property, "####", 18);

        case "UnaryExpression":
        case "UpdateExpression":
            {
                var op = n.operator;
                if (op == 'typeof' || op == 'void' || op == 'delete')
                    op += ' ';
                let s = expr(n.argument, indent, 15);
                return wrapExpr(n.prefix ? op + s : s + op, cprec, 15);
            }

        case "LogicalExpression":
        case "BinaryExpression":
            {
                // Note that in the case of an expression like (a+b+c+d+e+...)
                // this is basically a linked list via n.left. Recursing on n.left
                // when the chain has a few thousand nodes gives us an InternalError.
                // So do the slightly more complicated thing and iterate.

                var op = n.operator;
                var prec = precedence[op];
                assertEq(typeof prec, "number");

                // If we're going to parenthesize this whole expression, set
                // noIn to false, so as not to parenthesize subexpressions too.
                let parens = (op == "in" && noIn) || cprec >= prec;
                if (parens)
                    noIn = false;

                let a = [expr(n.right, indent, prec, noIn && prec <= 11), op];
                let x;
                for (x = n.left; x.type === n.type && precedence[x.operator] === prec; x = x.left) {
                    a.push(expr(x.right, indent, prec, noIn && prec <= 11));
                    a.push(x.operator);
                }
                a.push(expr(x, indent, prec - 1, noIn && prec - 1 <= 11));
                let s = a.reverse().join(' ');
                return parens ? '(' + s + ')' : s;
            }

        case "AssignmentExpression":
            return wrapExpr(expr(n.left, indent, 3, noIn) + " " + n.operator + " " +
                              expr(n.right, indent, 2, noIn),
                            cprec, 3);

        case "FunctionExpression":
            return wrapExpr(functionDeclaration("function", n.id, n, indent),
                            cprec, n.expression ? 3 : 19);

        case "ArrowFunctionExpression":
            {
                let par = params(n.params, indent, {arrow: true});
                let body;
                if (n.body.type === "BlockStatement") {
                    body = substmt(n.body, indent).trim();
                } else {
                    body = expr(n.body, indent, 2);
                    if (body.startsWith("{"))
                        body = "(" + body + ")";
                }
                return wrapExpr(par + " => " + body, cprec, 3);
            }

        // These Patterns appear as function parameters, assignment and
        // declarator left-hand sides, and as the left-hand side in a for-in
        // head.
        case "ObjectPattern":
            {
                var s = [];
                for (var i = 0; i < n.properties.length; i++) {
                    var p = n.properties[i];
                    s[i] = expr(p.key, indent + INDENT_LEVEL, 18) +
                            ": " +
                            expr(p.value, indent + INDENT_LEVEL, 2);
                }
                return "{" + s.join(", ") + "}";
            }

        case "TemplateLiteral":
            {
                var s = "";
                var expectString = true;
                for (let e of n.elements) {
                    if (expectString) {
                        if (e.type !== "Literal")
                            unexpected(e);
                        if (typeof e.value !== "string")
                            throw new Error("unexpected " + uneval(e.value) + " in TemplateLiteral");
                        s += quoteChars(e.value);
                    } else {
                        s += "${" + expr(e, indent + INDENT_LEVEL, 1) + "}";
                    }
                    expectString = !expectString;
                }
                return "`" + s + "`";
            }

        case "TaggedTemplate":
            {
                let tag = expr(n.callee, indent, 16);
                let cso = n.arguments[0];
                expectType(cso, "CallSiteObject");
                let s = "";
                for (let i = 0; i < cso.raw.length; i++) {
                    s += cso.raw[i];
                    if (i + 1 < n.arguments.length) {
                        let x = expr(n.arguments[i + 1], indent + INDENT_LEVEL, 1);
                        s += "${" + x + "}";
                    }
                }
                return tag + "`" + s + "`";
            }

        default:
            return unexpected(n);
        }
    }

    function declarators(arr, indent, noIn) {
        var s = [];
        for (var i = 0; i < arr.length; i++) {
            var n = arr[i];

            if (n.type === "VariableDeclarator") {
                var patt = expr(n.id, '####', 3);
                s[i] = n.init === null ? patt : patt + " = " + expr(n.init, indent, 2, noIn);
            } else {
                s[i] = unexpected(n);
            }
        }
        return s.join(", ");
    }

    var stmt = sourceElement;

    var INDENT_LEVEL = "    ";  // four spaces, no arguments

    function sourceElement(n, indent) {
        if (indent === void 0)
            indent = "";

        switch (n.type) {
        case "BlockStatement":
            return (indent + "{\n" +
                    n.body.map(x => stmt(x, indent + INDENT_LEVEL)).join("") +
                    indent + "}\n");

        case "VariableDeclaration":
            return indent + n.kind + " " + declarators(n.declarations, indent, false) + ";\n";

        case "EmptyStatement":
            return indent + ";\n";

        case "ExpressionStatement":
            {
                let s = expr(n.expression, indent, 0);
                if (s.match(/^(?:function |let |{)/))
                    s = "(" + s + ")";
                return indent + s + ";\n";
            }

        case "IfStatement":
            {
                var gotElse = n.alternate !== null;
                var s = indent + "if (" + expr(n.test, indent, 0) + ")" +
                        substmt(n.consequent, indent, gotElse);
                if (gotElse)
                    s += "else" + substmt(n.alternate, indent);
                return s;
            }

        case "WhileStatement":
            return indent + "while (" + expr(n.test, indent, 0) + ")" + substmt(n.body, indent);

        case "ForStatement":
            {
                let s = indent + "for (";
                if (n.init) {
                    if (n.init.type == "VariableDeclaration")
                        s += n.init.kind + " " + declarators(n.init.declarations, indent, true);
                    else
                        s += expr(n.init, indent, 0, true);
                }
                s += ";";
                if (n.test)
                    s += " " + expr(n.test, indent, 0);
                s += ";";
                if (n.update)
                    s += " " + expr(n.update, indent, 0);
                s += ")";
                return s + substmt(n.body, indent);
            }

        case "ForInStatement":
            return indent + forHead(n, "in", indent) + substmt(n.body, indent);

        case "ForOfStatement":
            return indent + forHead(n, "of", indent) + substmt(n.body, indent);

        case "DoWhileStatement":
            {
                var body = substmt(n.body, indent, true);
                return (indent + "do" + body + "while (" + expr(n.test, indent, 0) + ");\n");
            }

        case "ContinueStatement":
            return indent + "continue" + (n.label ? " " + n.label.name : "") + ";\n";

        case "BreakStatement":
            return indent + "break" + (n.label ? " " + n.label.name : "") + ";\n";

        case "ReturnStatement":
            return (indent + "return" +
                    (n.argument ? " " + expr(n.argument, indent, 0) : "") +
                    ";\n");

        case "WithStatement":
            return (indent + "with (" + expr(n.object, indent, 0) + ")" +
                    substmt(n.body, indent));

        case "LabeledStatement":
            return n.label.name + ": " + stmt(n.body, indent);

        case "SwitchStatement":
            {
                let cases = n.cases;
                let s = indent + "switch (" + expr(n.discriminant, indent, 0) + ") {\n";
                let deeper = indent + "    ";
                for (let j = 0; j < n.cases.length; j++) {
                    let scase = cases[j];
                    s += indent;
                    s += (scase.test ? "case " + expr(scase.test, indent, 0) : "default");
                    s += ":\n";
                    let stmts = scase.consequent;
                    for (let i = 0; i < stmts.length; i++)
                        s += stmt(stmts[i], deeper);
                }
                return s + indent + "}\n";
            }

        case "ThrowStatement":
            return indent + "throw " + expr(n.argument, indent, 0) + ";\n";

        case "TryStatement":
            {
                let s = indent + "try" + substmt(n.block, indent, true);
                for (let [i, c] of n.guardedHandlers.entries()) {
                    s += "catch (" + expr(c.param, '####', 0) +
                        " if (" + expr(c.guard, indent, 0) + ")";
                    let more = (n.finalizer !== null || n.handler !== null || i !== n.guardedHandlers.length - 1);
                    s += ")" + substmt(c.body, indent, more);
                }
                let c = n.handler;
                if (c !== null) {
                    s += "catch (" + expr(c.param, "####", 0) + ")" +
                        substmt(c.body, indent, n.finalizer !== null);
                }
                if (n.finalizer)
                    s += "finally" + substmt(n.finalizer, indent, false);
                return s;
            }

        case "DebuggerStatement":
            return indent + "debugger;";

        case "FunctionDeclaration":
            assertEq(n.id.type, "Identifier");
            return (indent +
                    functionDeclaration("function", n.id, n, indent) +
                    (n.expression ? ";\n" : "\n"));

        default:
            return unexpected(n);
        }
    }

    function stringify(n) {
        if (n.type != "Program")
            throw new TypeError("argument must be a Program parse node");
        return n.body.map(x => sourceElement(x, "")).join("");
    }
    Reflect.stringify = stringify;
})();


(function main(args) {
    "use strict";

    // Some smoke tests. These do not cover all cases; the main goal is to be
    // able to parse the SpiderMonkey test suite, which requires a separate
    // test runner.
    function runUnitTests() {
        // These programs are spaced and parenthesized just so, such that
        // Reflect.stringify mirrors Reflect.parse for these strings.
        var tests = [
            // expressions
            "x;\n",
            "null;\n",
            "true;\n",
            "false;\n",
            "-0;\n",
            "x = y;\n",
            "void 0;\n",
            "void y;\n",
            "void f();\n",
            "[];\n",
            "({});\n",
            "({1e999: 0});\n",
            ('({get "a b"() {\n' +
             '    return this;\n' +
             '}});\n'),
            ("({get 1() {\n" +
             "    return this;\n" +
             "}});\n"),
            ("({f() {\n}});\n"),
            ("({[Symbol.iterator]: function* () {\n" +
             "    yield this;\n" +
             "}});\n"),
            ("({[function () {\n" +
             "        return \"name\";\n" +
             "    }()]: \"jeff\"});\n"),
            "let pt = {x, y};\n",
            "[,, 2];\n",
            "[, 1,,];\n",
            "[1,,, 2,,,];\n",
            "[,,,];\n",
            '[0, 1, 2, "x"];\n',
            '[f(x) for (x in y)];\n',
            '[f(x) for (x in y) for ([y, z] in w)];\n',
            '[f(x) for (x in y) for ([y, z] in w) if (z.q())];\n',
            "x.y.z;\n",
            "x[y[z]];\n",
            'x["y z"];\n',
            "(0).toString();\n",
            "f()();\n",
            "f((x, y));\n",
            "f(x = 3);\n",
            "x.y();\n",
            "f(1, 2, 3, null, (g(), h));\n",
            "new (x.y);\n",
            "new (x());\n",
            "(new x).y;\n",
            "new (x().y);\n",
            "a * x + b * y;\n",
            "a * (x + b) * y;\n",
            "a + (b + c);\n",
            "a + b + c;\n",
            Array(1000).join("x + ") + "y;\n",
            //Array(1000).join("x + y - ") + "z;\n",   //CRASH, stack overflow,  unfiled bug
            "x.y = z;\n",
            "get(id).text = f();\n",
            "[,] = x;\n",
            ("({[function () {\n" +
             "        return \"name\";\n" +
             "    }()]: x} = obj);\n"),
            "`${x} > ${y}`;\n",
            "`${x, y}`;\n",
            "f``;\n",
            "x.y``;\n",
            "md.obj.method(x)`${y}`;\n",
            "let f = a => b`${a}`;\n",

            // YieldExpressions are only legal inside generators.
            ("function* gen() {\n" +
             "    yield 1;\n" +
             "}\n"),
            ("function* gen() {\n" +
             "    yield (a, b);\n" +
             "}\n"),
            ("function* gen() {\n" +
             "    (yield a), b;\n" +
             "}\n"),
            ("function* gen() {\n" +
             "    yield this.head;\n" +
             "    yield* this.tail;\n" +
             "}\n"),

            // Reconstituting constant-folded NaNs and Infinities
            "x = 1e999 + y;\n",
            "x = y / -1e999;\n",
            "x = 0 / 0;\n",
            "x = (-1e999).toString();\n",

            // Statements
            ("if (a == b)\n" +
             "    x();\n" +
             "else\n" +
             "    y();\n"),
            ("if (a == b) {\n" +
             "    x();\n" +
             "} else {\n" +
             "    y();\n" +
             "}\n"),
            ("if (a == b)\n" +
             "    if (b == c)\n" +
             "        x();\n" +
             "    else\n" +
             "        y();\n"),
            ("while (a == b)\n" +
             "    c();\n"),
            ("if (a)\n" +
             "    while (b)\n" +
             "        ;\n" +
             "else\n" +
             "    c();\n"),
            ("if (a)\n" +
             "    while (b) {\n" +
             "        ;\n" +
             "    }\n" +
             "else\n" +
             "    c();\n"),
            ("for (;;)\n" +
             "    ;\n"),
            ("for (let i = 0; i < a.length; i++) {\n" +
             "    b[i] = a[i];\n" +
             "}\n"),
            ("for (t = (i in x); t; t = t[i])\n" +  // ExpressionNoIn syntax
             "    ;\n"),
            ("for (let t = (i in x); t; t = t[i])\n" +
             "    ;\n"),
            ("for (t = 1 << (i in x); t < 100; t++)\n" +
             "    ;\n"),
            ("for (var i in arr)\n" +
             "    dump(arr[i]);\n"),
            ('for ([k, v] in items(x))\n' +
             '    dump(k + ": " + v);\n'),
            ("if (x) {\n" +
             "    switch (f(a)) {\n" +
             "    case f(b):\n" +
             "    case \"12\":\n" +
             "        throw exc;\n" +
             "    default:\n" +
             "        fall_through();\n" +
             "    case 99:\n" +
             "        succeed();\n" +
             "    }\n" +
             "}\n"),
            "var x;\n",
            "var x, y;\n",
            "var x = 1, y = x;\n",
            "var x = y = 1;\n",
            "var x = f, g;\n",
            "var x = (f, g);\n",
            "var [x] = a;\n",
            "var [] = x;\n",
            "var [, x] = y;\n",
            "var [[a, b], [c, d]] = x;\n",
            "var {} = x;\n",
            "var {x: x} = x;\n",
            "var {x: a, y: b} = x;\n",
            "var {1: a, 2: b} = x;\n",
            "var {1: [], 2: b} = x;\n",
            'var {"a b": x} = y;\n',
            "const a = 3;\n",
            ('try {\n' +
             '    f();\n' +
             '} finally {\n' +
             '    cleanup();\n' +
             '}\n'),
            ('try {\n' +
             '    f();\n' +
             '} catch (exc if (exc instanceof TypeError)) {\n' +
             '    g();\n' +
             '} catch (x) {\n' +
             '    cope(x);\n' +
             '} finally {\n' +
             '    cleanup();\n' +
             '}\n'),

            // Functions
            ("function f() {\n" +
             "    g();\n" +
             "}\n"),
            "function f(x) x * x;\n",
            "function f(a) a = 1;\n",
            "function f(x) function (y) x + y;\n",
            "function f(x) ({name: x, value: 0});\n",
            "a => a + 1;\n",
            ("() => {\n" +
             "    x++;\n" +
             "};\n"),
            "a => b => [a, b];\n",
            "a = b => c;\n",
            "a => ({});\n",
            "y = a => ({}.x);\n",
            ("function w() {\n" +
             "    return new new.target;\n" +
             "}\n"),

            // strict declarations
            ('"use strict";\n' +
             'x = 1;\n'),
            ('function f() {\n' +
             '    "use strict";\n' +
             '    x = 1;\n' +
             '}\n'),
            ('(function () {\n' +
             '    "use strict";\n' +
             '    x = 1;\n' +
             '});\n'),

            // Statement-vs-ExpressionStatement ambiguities
            "let(t = x);\nx = y, y = t;\n",
            ("(function () {\n" +
             "    go();\n" +
             "}());\n"),
            ("(function () {\n" +
             "}.x);\n"),
            ("(function name() {\n" +
             "}.x);\n"),
            ("(function () {\n" +
             "}.x = 1);\n"),
            ("(function name() {\n" +
             "}.x = 1);\n"),
            ("(function () {\n" +
             "}.x, function () {\n" +
             "}.y);\n"),
            ("(function () {\n" +
             "} + x) * y;\n"),
            ("(function () {\n" +
             "} * x + y);\n"),
            "({a: f()});\n",
            "({a: my_a} = f());\n",

            // misc
            ('options("tracejit");\n' +
             "try {\n" +
             "} catch (e) {\n" +
             "}\n"),
            ("function test() {\n" +
             "    var s1 = evalcx(\"lazy\");\n" +
             "    expect = function () {\n" +
             "        test();\n" +
             "    }(s1);\n" +
             "}\n"),
            ("try {\n" +
             "    var a = new Array(100000);\n" +
             "    var i = a.length;\n" +
             "    new i(eval(\"var obj = new Object(); obj.split = String.prototype.split;\"));\n" +
             "} catch (e) {\n" +
             "}\n"),
            ("test3();\n" +
             "function test3() {\n" +
             "    try {\n" +
             "        eval(\"for(let y in [\\\"\\\", ''])\");\n" +
             "    } catch (ex) {\n" +
             "    }\n" +
             "    new test3;\n" +
             "}\n"),
        ];

        for (var i = 0; i < tests.length; i++) {
            var b = tests[i], a;
            try {
                a = Reflect.stringify(Reflect.parse(b, {loc: false}));
                if (typeof a !== "string") {
                    throw new TypeError("Reflect.stringify returned " +
                                        (a !== null && typeof a === "object"
                                         ? Object.prototype.toString.call(a)
                                         : uneval(a)) +
                                        "; expected string");
                }
            } catch (exc) {
                print("FAIL - Exception thrown.");
                print(exc.name + ": " + exc.message);
                print(exc.stack);
                print("Test was: " + b);
                print();
                continue;
            }
            if (a !== b) {
                print("FAIL - Mismatch.");
                print("got:      " + uneval(a));
                print("expected: " + uneval(b));
                print();
            }
        }
    }

    if (!args) {
        return;
    } else if (args.length === 0) {
        return runUnitTests();
    } else if (args.length === 2 && args[0] === "--check") {
        var program = read(args[1]);
        var after;
        try {
            after = Reflect.stringify(Reflect.parse(program, {loc: false}));
        } catch (exc) {
            print(exc.stack);
            throw exc;
        }

        var dis0 = disassemble("-r", "-S", program);
        var dis1 = disassemble("-r", "-S", after);

        let stripVaryingBits = str => {
            return (str
                    // JSOP_LINENO opcodes provide line number information (for direct
                    // eval).  We need to strip those out, since the line numbers will
                    // never match except by accident.
                    .replace(/^(\d+: +lineno +)\d+$/mg, "$1**")
                    // Unfortunately the disassembly for JSOP_LAMBDA and
                    // JSOP_DEFFUN includes the entire function. Try and strip
                    // that out too.
                    .replace(/(\n\d+: +(?:deffun|lambda|lambda_arrow)\b) +[\s\S]*?(?=\n0\d{4}:)/g, "$1 ***"));
        };

        if (stripVaryingBits(dis0) === stripVaryingBits(dis1)) {
            print(" PASSED! OK");
        } else {
            print("Disassembly output did not match.");
            print("=== PROGRAM BEFORE ===");
            print(program);
            print("\n=== PROGRAM AFTER ===");
            print(after);
            print("\n=== START 1 ===");
            print(stripVaryingBits(dis0));
            print("\n=== START 2 ===");
            print(stripVaryingBits(dis1));
            print("\n=== END ===");
        }
    } else {
        throw new Error("usage: js reflect-stringify.js --check FILE");
    }
})(this.scriptArgs);
