
import { ps, choice, range, action, sequence, join_action, negate, repeat0, optional, repeat1, wsequence, whitespace, ch, butnot, expect, } from "./jsparse.js"
export { parse_sexp }

/* S-expr parser */
function parse_sexp(s) {
	s = s.trimRight()
	var res = program_stx(ps(s))
	if (res.remaining.index === s.length) return res.ast
	throw ("parse error at char " + res.remaining.index + "\nrest to parse: " + res.remaining)
}

var x_stx = function(input) { return x_stx(input) } // forward decl.

var id_special_char = choice("-", "&", "!", "=", ">", "<", "%", "+", "?", "/", "*", "$", "_", "'", ".", "@", "|", "~", "^")
var id_char = choice(range("a", "z"), range("A", "Z"), range("0", "9"), id_special_char)

var id_stx = action(
	join_action(butnot(repeat1(id_char), "."), ""), 
	function (str) {
		if (str.length == 1) return str
		switch (str[0]) {
			//case ".": return str.length == 1 ? str : ["js-getter", ["wat-string", str.substring(1)]]
			case ".": return ["js-getter", ["wat-string", str.substring(1)]]
			case "@": return ["js-invoker", ["wat-string", str.substring(1)]]
			case "&": return ["js-global", ["wat-string", str.substring(1)]]
		}
		return str
	}
)
var escape_char = choice("\"", "\\", "n", "r", "t", "0")
var escape_sequence = action(
	sequence("\\", escape_char),
	function(ast) {
		switch (ast[1]) {
			case "n": return "\n"
			case "r": return "\r"
			case "t": return "\t"
			case "0": return "\0"
			default: return ast[1]
		}
	}
);

var line_terminator = choice(ch("\r"), ch("\n"))
var string_char = choice(escape_sequence, line_terminator, negate("\""))
var string_stx = action(
	sequence("\"", join_action(repeat0(string_char), ""), "\""),
	function(ast) { return ["wat-string", ast[1]] }
);

var digits = join_action(repeat1(range("0", "9")), "")
var number_stx = action(
	sequence(optional(choice("+", "-")), digits, optional(join_action(sequence(".", digits), ""))),
	function(ast) {
		var sign = ast[0] ? ast[0] : ""
		var integral_digits = ast[1]
		var fractional_digits = ast[2] || ""
		return Number(sign + integral_digits + fractional_digits)
	}
);

function make_constant_stx(string, constant) { return action(string, function(ast) { return constant; }) }
var nil_stx = make_constant_stx("()", [])
var t_stx = make_constant_stx("#t", true)
var f_stx = make_constant_stx("#f", false)
var null_stx = make_constant_stx("#null", null)
var inert_stx = make_constant_stx("#inert", "#inert")
var ignore_stx = make_constant_stx("#ignore", "#ignore")
var undefined_stx = make_constant_stx("#undefined", undefined)
var qualified_stx = action(sequence(id_stx, ":", id_stx), function(ast) { return ["eval", ["quote", ast[2]], ast[0]] })
var dot_stx = action(wsequence(".", x_stx), function(ast) { return ast[1] })
var compound_stx = action(
	wsequence( "(", repeat1(x_stx), optional(dot_stx), ")" ),
	function(ast) { return !ast[2] ? ast[1] : ast[1].concat( [".", ast[2]] ) }
);
var quote_stx = action(sequence("'", x_stx), function(ast) { return ["quote", ast[1]] })
var cmt_stx = expect(sequence(";", repeat0(negate(line_terminator)), optional(line_terminator)))
var x_stx = whitespace(choice(qualified_stx, inert_stx, ignore_stx, nil_stx, t_stx, f_stx, null_stx, undefined_stx, number_stx, quote_stx, compound_stx, id_stx, string_stx, cmt_stx))
var program_stx = repeat0(x_stx)
