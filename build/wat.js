(function(f){if(typeof exports==="object"&&typeof module!=="undefined"){module.exports=f()}else if(typeof define==="function"&&define.amd){define([],f)}else{var g;if(typeof window!=="undefined"){g=window}else if(typeof global!=="undefined"){g=global}else if(typeof self!=="undefined"){g=self}else{g=this}g.wat = f()}})(function(){var define,module,exports;return (function(){function r(e,n,t){function o(i,f){if(!n[i]){if(!e[i]){var c="function"==typeof require&&require;if(!f&&c)return c(i,!0);if(u)return u(i,!0);var a=new Error("Cannot find module '"+i+"'");throw a.code="MODULE_NOT_FOUND",a}var p=n[i]={exports:{}};e[i][0].call(p.exports,function(r){var n=e[i][1][r];return o(n||r)},p,p.exports,r,e,n,t)}return n[i].exports}for(var u="function"==typeof require&&require,i=0;i<t.length;i++)o(t[i]);return o}return r})()({1:[function(require,module,exports){
module.exports.main = [["vm-def","$define!","vm-def"],["$define!","array->list","vm-array-to-list"],["$define!","begin","vm-begin"],["$define!","cons","vm-cons"],["$define!","cons?","vm-cons?"],["$define!","dnew","vm-dnew"],["$define!","dref","vm-dref"],["$define!","error","vm-error"],["$define!","eval","vm-eval"],["$define!","if","vm-if"],["$define!","js-getter","vm-js-getter"],["$define!","js-global","vm-js-global"],["$define!","js-invoker","vm-js-invoker"],["$define!","list*","vm-list*"],["$define!","list->array","vm-list-to-array"],["$define!","make-environment","vm-make-environment"],["$define!","new","vm-js-new"],["$define!","nil?","vm-nil?"],["$define!","reverse-list","vm-reverse-list"],["$define!","setter","vm-setter"],["$define!","string->symbol","vm-string-to-symbol"],["$define!","symbol-name","vm-symbol-name"],["$define!","symbol?","vm-symbol?"],["$define!","throw","vm-throw"],["$define!","unwrap","vm-unwrap"],["$define!","wrap","vm-wrap"],["$define!","$vau","vm-vau"],["$define!","quote",["$vau",["x"],"#ignore","x"]],["$define!","list",["wrap",["$vau","elts","#ignore","elts"]]],["$define!","the-environment",["$vau",[],"e","e"]],["$define!","get-current-environment",["wrap",["$vau",[],"e","e"]]],["$define!","make-macro-expander",["wrap",["$vau",["expander"],"#ignore",["$vau","operands","env",["eval",["eval",["cons","expander","operands"],["make-environment"]],"env"]]]]],["$define!","$vau",["make-macro-expander",["$vau",["params","env-param",".","body"],"#ignore",["list","vm-vau","params","env-param",["list*","begin","body"]]]]],["$define!","macro",["make-macro-expander",["$vau",["params",".","body"],"#ignore",["list","make-macro-expander",["list*","$vau","params","#ignore","body"]]]]],["$define!","define-macro",["macro",[["name",".","params"],".","body"],["list","$define!","name",["list*","macro","params","body"]]]],["define-macro",["$lambda","params",".","body"],["list","wrap",["list*","$vau","params","#ignore","body"]]],["define-macro",["define-operative",["name",".","params"],"envparam",".","body"],["list","$define!","name",["list*","$vau","params","envparam","body"]]],["define-macro",["loop",".","body"],["list","vm-loop",["list*","begin","body"]]],["define-operative",["catch","protected","handler"],"env",["eval",["list","vm-catch","protected",["eval","handler","env"]],"env"]],["define-operative",["push-prompt","prompt",".","body"],"env",["eval",["list","vm-push-prompt",["eval","prompt","env"],["list*","begin","body"]],"env"]],["define-macro",["take-subcont","prompt","k",".","body"],["list","vm-take-subcont","prompt",["list*","$lambda",["list","k"],"body"]]],["define-macro",["push-subcont","k",".","body"],["list","vm-push-subcont","k",["list*","$lambda",[],"body"]]],["define-macro",["push-prompt-subcont","p","k",".","body"],["list","vm-push-prompt-subcont","p","k",["list*","$lambda",[],"body"]]],["$define!","compose",["$lambda",["f","g"],["$lambda",["arg"],["f",["g","arg"]]]]],["$define!","car",["$lambda",[["x",".","#ignore"]],"x"]],["$define!","cdr",["$lambda",[["#ignore",".","x"]],"x"]],["$define!","caar",["compose","car","car"]],["$define!","cadr",["compose","car","cdr"]],["$define!","cdar",["compose","cdr","car"]],["$define!","cddr",["compose","cdr","cdr"]],["$define!","map-list",["$lambda",["f","lst"],["if",["nil?","lst"],[],["cons",["f",["car","lst"]],["map-list","f",["cdr","lst"]]]]]],["$define!","list-for-each",["$lambda",["f","lst"],["if",["nil?","lst"],[],["begin",["f",["car","lst"]],["list-for-each","f",["cdr","lst"]]]]]],["$define!","list-keep",["$lambda",["p","lst"],["if",["nil?","lst"],[],["if",["p",["car","lst"]],["cons",["car","lst"],["list-keep","p",["cdr","lst"]]],["list-keep","p",["cdr","lst"]]]]]],["$define!","fold-list",["$lambda",["f","init","lst"],["if",["nil?","lst"],"init",["fold-list","f",["f","init",["car","lst"]],["cdr","lst"]]]]],["define-macro",["let","bindings",".","body"],["if",["symbol?","bindings"],["list*","let-loop","bindings","body"],["list*",["list*","$lambda",["map-list","car","bindings"],"body"],["map-list","cadr","bindings"]]]],["define-macro",["let-loop","name","bindings",".","body"],["list","letrec",["list",["list","name",["list*","$lambda",["map-list","car","bindings"],"body"]]],["list*","name",["map-list","cadr","bindings"]]]],["define-macro",["let*","bindings",".","body"],["if",["nil?","bindings"],["list*","let",[],"body"],["list","let",["list",["car","bindings"]],["list*","let*",["cdr","bindings"],"body"]]]],["define-macro",["letrec","bindings",".","body"],["list*","let",[],["list","$define!",["map-list","car","bindings"],["list*","list",["map-list","cadr","bindings"]]],"body"]],["define-macro",["lambda","params",".","body"],["letrec",[["typed-params->names-and-checks",["$lambda",["ps"],["if",["cons?","ps"],["let*",[[["p",".","rest-ps"],"ps"],[["names",".","checks"],["typed-params->names-and-checks","rest-ps"]]],["if",["cons?","p"],["let*",[[["name","type"],"p"],["check",["list","the","type","name"]]],["cons",["cons","name","names"],["cons","check","checks"]]],["cons",["cons","p","names"],"checks"]]],["cons","ps",[]]]]]],["let",[[["untyped-names",".","type-checks"],["typed-params->names-and-checks","params"]]],["list*","$lambda","untyped-names",["list*","begin","type-checks"],"body"]]]],["define-macro",["define","lhs",".","rhs"],["if",["cons?","lhs"],["list","$define!",["car","lhs"],["list*","lambda",["cdr","lhs"],"rhs"]],["list","$define!","lhs",["car","rhs"]]]],["define",["apply","appv","arg",".","opt"],["if",["instanceof","appv",["js-global",["wat-string","Function"]]],[["js-invoker",["wat-string","apply"]],"appv",null,["list->array","arg"]],["eval",["cons",["unwrap","appv"],"arg"],["if",["nil?","opt"],["make-environment"],["car","opt"]]]]],["define-operative",["cond",".","clauses"],"env",["if",["nil?","clauses"],null,["let",[[[["test",".","body"],".","clauses"],"clauses"]],["if",["eval","test","env"],["apply",["wrap","begin"],"body","env"],["apply",["wrap","cond"],"clauses","env"]]]]],["define","else",true],["define-operative",["and",".","x"],"e",["cond",[["nil?","x"],true],[["nil?",["cdr","x"]],["eval",["car","x"],"e"]],[["eval",["car","x"],"e"],["apply",["wrap","and"],["cdr","x"],"e"]],["else",false]]],["define-operative",["or",".","x"],"e",["cond",[["nil?","x"],false],[["nil?",["cdr","x"]],["eval",["car","x"],"e"]],[["eval",["car","x"],"e"],true],["else",["apply",["wrap","or"],["cdr","x"],"e"]]]],["define",["call-with-escape","fun"],["let",[["fresh",["list",null]]],["catch",["fun",["$lambda","opt-arg",["throw",["list","fresh","opt-arg"]]]],["$lambda",["exc"],["if",["and",["cons?","exc"],["===","fresh",["car","exc"]]],["let",[["opt-arg",["cadr","exc"]]],["if",["cons?","opt-arg"],["car","opt-arg"],null]],["throw","exc"]]]]]],["define-macro",["label","name",".","body"],["list","call-with-escape",["list*","$lambda",["list","name"],"body"]]],["define-operative",["while","test",".","body"],"env",["let",[["body",["list*","begin","body"]]],["label","return",["loop",["if",["eval","test","env"],["eval","body","env"],["return"]]]]]],["define-macro",["when","test",".","body"],["list","if","test",["list*","begin","body"],null]],["define-macro",["unless","test",".","body"],["list*","when",["list","not","test"],"body"]],["define-macro",["set",["getter",".","args"],"new-val"],["list*",["list","setter","getter"],"new-val","args"]],["define-operative",["dlet","bindings",".","body"],"env",["eval",["let","process-bindings",[["bs","bindings"]],["if",["nil?","bs"],["list*","begin","body"],["let*",[[[["name","expr"],".","rest-bs"],"bs"],["value",["eval","expr","env"]]],["list","vm-dlet","name","value",["process-bindings","rest-bs"]]]]],"env"]],["define-operative",["define-prototype","name","super-name","prop-names"],"env",["eval",["list","$define!","name",["make-prototype","name","super-name","prop-names","env"]],"env"]],["define",["make-prototype","name","super-name","prop-names","env"],["let",[["p",["apply","vm-js-make-prototype",["list*",["symbol-name","name"],["map-list","symbol-name","prop-names"]]]],["super",["eval","super-name","env"]]],["set",[["js-getter",["wat-string","prototype"]],"p"],[["js-invoker",["wat-string","create"]],["js-global",["wat-string","Object"]],[["js-getter",["wat-string","prototype"]],"super"]]],["set",[["js-getter",["wat-string","constructor"]],[["js-getter",["wat-string","prototype"]],"p"]],"super"],"p"]],["define-macro",["define-generic",["name",".","#ignore"]],["list","$define!","name",["lambda","args",["apply",[["js-getter","name"],["car","args"]],"args"]]]],["define-macro",["define-method",["name",["self","ctor"],".","args"],".","body"],["list","put-method","ctor",["symbol-name","name"],["list*","lambda",["list*","self","args"],"body"]]],["define",["put-method","ctor","name","fun"],["set",[["js-getter","name"],[["js-getter",["wat-string","prototype"]],"ctor"]],"fun"]],["define-operative",["provide","symbols",".","body"],"env",["eval",["list","$define!","symbols",["list","let",[],["list*","begin","body"],["list*","list","symbols"]]],"env"]],["define-operative",["module","exports",".","body"],"env",["let",[["menv",["make-environment","env"]]],["eval",["list*","provide","exports","body"],"menv"],["make-environment","menv"]]],["define-macro",["define-module","name","exports",".","body"],["list","$define!","name",["list*","module","exports","body"]]],["define-operative",["import","module","imports"],"env",["let*",[["m",["eval","module","env"]],["values",["map-list",["$lambda",["import"],["eval","import","m"]],"imports"]]],["eval",["list","$define!","imports",["list*","list","values"]],"env"]]],["define",["relational-op","name"],["let",[["binop",["vm-js-binop","name"]]],["letrec",[["op",["lambda",["arg1","arg2",".","rest"],["if",["binop","arg1","arg2"],["if",["nil?","rest"],true,["apply","op",["list*","arg2","rest"]]],false]]]],"op"]]],["define","==",["relational-op",["wat-string","=="]]],["define","===",["relational-op",["wat-string","==="]]],["define","<",["relational-op",["wat-string","<"]]],["define",">",["relational-op",["wat-string",">"]]],["define","<=",["relational-op",["wat-string","<="]]],["define",">=",["relational-op",["wat-string",">="]]],["define",["!=",".","args"],["not",["apply","==","args"]]],["define",["!==",".","args"],["not",["apply","===","args"]]],["define","*",["let",[["vm*",["vm-js-binop",["wat-string","*"]]]],["lambda","args",["fold-list","vm*",1,"args"]]]],["define","+",["let",[["vm+",["vm-js-binop",["wat-string","+"]]]],["lambda","args",["if",["nil?","args"],0,["fold-list","vm+",["car","args"],["cdr","args"]]]]]],["define",["negative-op","binop","unit"],["lambda",["arg1",".","rest"],["if",["nil?","rest"],["binop","unit","arg1"],["fold-list","binop","arg1","rest"]]]],["define","-",["negative-op",["vm-js-binop",["wat-string","-"]],0]],["define","/",["negative-op",["vm-js-binop",["wat-string","/"]],1]],["define","%",["vm-js-binop",["wat-string","%"]]],["define","not",["vm-js-unop",["wat-string","!"]]],["define","typeof",["vm-js-unop",["wat-string","typeof"]]],["define","in",["vm-js-binop",["wat-string","in"]]],["define","instanceof",["vm-js-binop",["wat-string","instanceof"]]],["define","bitand",["vm-js-binop",["wat-string","&"]]],["define","bitor",["vm-js-binop",["wat-string","|"]]],["define","bitxor",["vm-js-binop",["wat-string","^"]]],["define","bitnot",["vm-js-unop",["wat-string","~"]]],["define","bitshiftl",["vm-js-binop",["wat-string","<<"]]],["define","bitshiftr",["vm-js-binop",["wat-string",">>"]]],["define","bitshiftr0",["vm-js-binop",["wat-string",">>>"]]],["define-operative",["object",".","pairs"],"env",["let",[["obj",["vm-js-make-object"]]],["map-list",["$lambda",[["name","value"]],["set",[["js-getter",["eval","name","env"]],"obj"],["eval","value","env"]]],"pairs"],"obj"]],["define",["elt","object","key"],[["js-getter","key"],"object"]],["set",["setter","elt"],["lambda",["new-val","object","key"],["set",[["js-getter","key"],"object"],"new-val"]]],["define",["array",".","args"],["list->array","args"]],["define",["js-callback","fun"],["vm-js-function",["$lambda","args",["push-prompt","vm-root-prompt",["apply","fun","args"]]]]],["define-macro",["js-lambda","params",".","body"],["list","js-callback",["list*","lambda","params","body"]]],["define-macro",["type?","obj","type"],["list","vm-type?","obj","type",["symbol-name","type"]]],["define-macro",["the","type","obj"],["list","if",["list","type?","obj","type"],"obj",["list","error",["list","+","obj",["wat-string"," is not a: "],"type"]]]],["define","Array",["js-global",["wat-string","Array"]]],["define","Boolean",["js-global",["wat-string","Boolean"]]],["define","Date",["js-global",["wat-string","Date"]]],["define","Function",["js-global",["wat-string","Function"]]],["define","Number",["js-global",["wat-string","Number"]]],["define","Object",["js-global",["wat-string","Object"]]],["define","RegExp",["js-global",["wat-string","RegExp"]]],["define","String",["js-global",["wat-string","String"]]],["define",["log","x",".","xs"],["apply",["js-invoker",["wat-string","log"]],["list*",["js-global",["wat-string","console"]],"x","xs"]],"x"],["define-prototype","Cell","Object",["value"]],["define",["cell","value"],["new","Cell","value"]],["define",["ref",["c","Cell"]],[["js-getter",["wat-string","value"]],"c"]],["set",["setter","ref"],["lambda",["new-val",["c","Cell"]],["set",[["js-getter",["wat-string","value"]],"c"],"new-val"]]],["define-macro",["++","place"],["list","set","place",["list","+","place",1]]],["define-macro",["--","place"],["list","set","place",["list","-","place",1]]],["define",["map-array","fun",["arr","Array"]],["list->array",["map-list","fun",["array->list","arr"]]]],["define",["array-keep","pred",["arr","Array"]],["list->array",["list-keep","pred",["array->list","arr"]]]],["define-operative",["time","expr"],"env",["let",[["n",[["js-invoker",["wat-string","getTime"]],["new","Date"]]],["result",["eval","expr","env"]]],["log",["+",["wat-string","time "],"expr",["wat-string",": "],["-",[["js-invoker",["wat-string","getTime"]],["new","Date"]],"n"],["wat-string","ms"]]],"result"]],["define-operative",["assert-true","expr"],"env",["unless",["===",true,["eval","expr","env"]],["error",["+",["wat-string","Should be true: "],"expr"]]]],["define-operative",["assert-false","expr"],"env",["unless",["===",false,["eval","expr","env"]],["error",["+",["wat-string","Should be false: "],"expr"]]]],["define-operative",["assert-===","expected","expr2"],"env",["let",[["res",["eval","expr2","env"]],["exp",["eval","expected","env"]]],["unless",["===","exp","res"],["error",["+","expr2",["wat-string"," should be "],"exp",["wat-string"," but is "],"res"]]]]],["define-operative",["assert-==","expected","expr2"],"env",["let",[["res",["eval","expr2","env"]],["exp",["eval","expected","env"]]],["unless",["==","exp","res"],["error",["+","expr2",["wat-string"," should be "],"exp",["wat-string"," but is "],"res"]]]]],["define-operative",["assert-throws","expr"],"env",["label","return",["catch",["eval","expr","env"],["lambda",["exc"],["return"]]],["error",["+",["wat-string","Should throw: "],"expr"]]]],["define-prototype","Option","Object",[]],["define-prototype","Some","Option",["value"]],["define-prototype","None","Option",[]],["define",["some","value"],["new","Some","value"]],["define","none",["new","None"]],["define-operative",["if-option",["option-name","option-expr"],"then","else"],"env",["let",[["option",["the","Option",["eval","option-expr","env"]]]],["if",["type?","option","Some"],["eval",["list",["list","lambda",["list","option-name"],"then"],[["js-getter",["wat-string","value"]],"option"]],"env"],["eval","else","env"]]]],["define",["print-stacktrace","err"],["define",["print-frame","k"],["log",[["js-invoker",["wat-string","toString"]],[["js-getter",["wat-string","fun"]],"k"]],[["js-getter",["wat-string","dbg"]],"k"],[["js-getter",["wat-string","e"]],"k"]],["when",["type?",[["js-getter",["wat-string","next"]],"k"],["js-global",["wat-string","StackFrame"]]],["print-frame",[["js-getter",["wat-string","next"]],"k"]]]],["take-subcont","vm-root-prompt","k",["log","err"],["log","k"],["push-prompt","vm-root-prompt",["push-subcont","k"]]]],["define",["user-break","err"],["throw","err"]]]

},{}],2:[function(require,module,exports){
module.exports = {
	ps, choice, range, action, sequence, join_action, negate, repeat0, optional, repeat1, wsequence, whitespace, ch, butnot, expect,
} /* browserify adaptation for wat */

// Copyright (C) 2007 Chris Double.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
//
// 1. Redistributions of source code must retain the above copyright notice,
//    this list of conditions and the following disclaimer.
//
// 2. Redistributions in binary form must reproduce the above copyright notice,
//    this list of conditions and the following disclaimer in the documentation
//    and/or other materials provided with the distribution.
//
// THIS SOFTWARE IS PROVIDED ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES,
// INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
// FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
// DEVELOPERS AND CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
// SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
// PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
// OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
// WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
// OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
// ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//

function foldl(f, initial, seq) {
	for (var i = 0; i < seq.length; ++i) initial = f(initial, seq[i])
	return initial
}

var memoize = true

function ParseState(input, index) {
	this.input = input
	this.index = index || 0
	this.length = input.length - this.index
	this.cache = { }
	return this
}

ParseState.prototype.from = function(index) {
	var r = new ParseState(this.input, this.index + index)
	r.cache = this.cache
	r.length = this.length - index
	return r
}

ParseState.prototype.substring = function(start, end) {
	return this.input.substring(start + this.index, (end || this.length) + this.index)
}

ParseState.prototype.trimLeft = function() {
	var s = this.substring(0)
	var m = s.match(/^\s+/)
	return m ? this.from(m[0].length) : this
}

ParseState.prototype.at = function(index) {
	return this.input.charAt(this.index + index)
}

ParseState.prototype.toString = function() {
	return '"' + this.substring(0) + '"'
}

ParseState.prototype.getCached = function(pid) {
	if (!memoize) return false

	var p = this.cache[pid]
	return p ? p[this.index] : false
}

ParseState.prototype.putCached = function(pid, cached) {
	if (!memoize) return false

	var p = this.cache[pid];
	(p ? p : this.cache[pid] = {})[this.index] = cached
}

function ps(str) {
	return new ParseState(str)
}

// 'r' is the remaining string to be parsed.
// 'matched' is the portion of the string that was successfully matched by the parser.
// 'ast' is the AST returned by the successfull parse.
function make_result(r, matched, ast) {
	return { remaining: r, matched: matched, ast: ast }
}

var parser_id = 0

// 'token' is a parser combinator that given a string, returns a parser
// that parses that string value. The AST contains the string that was parsed.
function token(s) {
	var pid = parser_id++
	
	return function(state) {
		var savedState = state
		var cached = savedState.getCached(pid)
		if (cached) return cached

		var r = state.length >= s.length && state.substring(0, s.length) == s
		cached = !r ? false : make_result(state.from(s.length), s, s)
		savedState.putCached(pid, cached)
		return cached
	}
}

// Like 'token' but for a single character. Returns a parser that given a string
// containing a single character, parses that character value.
function ch(c) {
	var pid = parser_id++
	
	return function(state) {
		var savedState = state
		var cached = savedState.getCached(pid)
		if (cached) return cached

		var r = state.length >= 1 && state.at(0) == c
		cached = !r ? false : make_result(state.from(1), c, c)
		savedState.putCached(pid, cached)
		return cached
	}
}

// 'range' is a parser combinator that returns a single character parser
// (similar to 'ch'). It parses single characters that are in the inclusive
// range of the 'lower' and 'upper' bounds ("a" to "z" for example).
function range(lower, upper) {
	var pid = parser_id++
	
	return function(state) {
		var savedState = state
		var cached = savedState.getCached(pid)
		if (cached)	return cached

		if (state.length < 1)
			cached = false
		else {
			var ch = state.at(0)
			cached = ch >= lower && ch <= upper ? make_result(state.from(1), ch, ch) : false
		}
		savedState.putCached(pid, cached)
		return cached
	}
}

// Helper function to convert string literals to token parsers
// and perform other implicit parser conversions.
function toParser(p) {
	return typeof(p) == "string" ? token(p) : p
}

// Parser combinator that returns a parser that
// skips whitespace before applying parser.
function whitespace(p) {
	var p = toParser(p)
	var pid = parser_id++
	
	return function(state) {
		var savedState = state
		var cached = savedState.getCached(pid)
		if (cached) return cached

		cached = p(state.trimLeft())
		savedState.putCached(pid, cached)
		return cached
	}
}

// Parser combinator that passes the AST generated from the parser 'p' to the function 'f'.
// The result of 'f' is used as the AST in the result.
function action(p, f) {
	var p = toParser(p)
	var pid = parser_id++
	
	return function(state) {
		var savedState = state
		var cached = savedState.getCached(pid)
		if (cached) return cached

		var x = p(state)
		if (!x)
			cached = false
		else {
			x.ast = f(x.ast)
			cached = x
		}
		savedState.putCached(pid, cached)
		return cached
	}
}

// Given a parser that produces an array as an ast, returns a
// parser that produces an ast with the array joined by a separator.
function join_action(p, sep) {
	return action(p, function(ast) { return ast.join(sep) })
}

// Given an ast of the form [ Expression, [ a, b, ...] ], convert to
// [ [ [ Expression [ a ] ] b ] ... ]
// This is used for handling left recursive entries in the grammar. e.g.
// MemberExpression:
//   PrimaryExpression
//   FunctionExpression
//   MemberExpression [ Expression ]
//   MemberExpression . Identifier
//   new MemberExpression Arguments
function left_factor(ast) {
	return foldl(
		function(v, action) { return [v, action] }, ast[0], ast[1]
	)
}

// Return a parser that left factors the ast result of the original
// parser.
function left_factor_action(p) {
	return action(p, left_factor)
}

// 'negate' will negate a single character parser. So given 'ch("a")' it will successfully
// parse any character except for 'a'. Or 'negate(range("a", "z"))' will successfully parse
// anything except the lowercase characters a-z.
function negate(p) {
	var p = toParser(p)
	var pid = parser_id++
	
	return function(state) {
		var savedState = state
		var cached = savedState.getCached(pid)
		if (cached) return cached

        if(state.length < 1)
        	cached = false
        else {	
            var r = p(state)
            cached =  r ? false : make_result(state.from(1), state.at(0), state.at(0))
        }
		savedState.putCached(pid, cached)
		return cached
	}
}

// 'end_p' is a parser that is successful if the input string is empty (ie. end of parse).
function end_p(state) {
	return state.length == 0 ? make_result(state, undefined, undefined) : false
}

// 'nothing_p' is a parser that always fails.
function nothing_p(state) {
	return false
}

// 'sequence' is a parser combinator that processes a number of parsers in sequence.
// It can take any number of arguments, each one being a parser. The parser that 'sequence'
// returns succeeds if all the parsers in the sequence succeeds. It fails if any of them fail.
function sequence() {
	var parsers = []
	for (var i = 0; i < arguments.length; ++i) 	parsers.push(toParser(arguments[i]))
	var pid = parser_id++
	
	return function(state) {
		var savedState = state
		var cached = savedState.getCached(pid)
		if (cached) return cached

		var ast = []
		var matched = ""
		for (var i = 0; i < parsers.length; ++i) {
			var parser = parsers[i]
			var result = parser(state)
			if (!result) break
			state = result.remaining
			if (result.ast !== nothing) ast.push(result.ast)
			matched += result.matched
		}
		cached = i != parsers.length ? false : make_result(state, matched, ast)
		savedState.putCached(pid, cached)
		return cached
	}
}

// Like sequence, but ignores whitespace between individual parsers.
function wsequence() {
	var parsers = []
	for (var i = 0; i < arguments.length; ++i) {
		parsers.push(whitespace(toParser(arguments[i])))
	}
	return sequence.apply(null, parsers)
}

// 'choice' is a parser combinator that provides a choice between other parsers.
// It takes any number of parsers as arguments
// and returns a parser that will try each of the given parsers in order.
// The first one that succeeds results in a successfull parse.
// It fails if all parsers fail.
function choice() {
	var parsers = []
	for (var i = 0; i < arguments.length; ++i) {
		parsers.push(toParser(arguments[i]))
	}
	var pid = parser_id++
	
	return function(state) {
		var savedState = state
		var cached = savedState.getCached(pid)
		if (cached) return cached
		
		for (var i = 0; i < parsers.length; ++i) {
			var parser = parsers[i]
			var result = parser(state)
			if (result) break
		}
		cached = i == parsers.length ? false : result
		savedState.putCached(pid, cached)
		return cached
	}
}

// 'butnot' is a parser combinator that takes two parsers, 'p1' and 'p2'.
// It returns a parser that succeeds if 'p1' matches and 'p2' does not,
// or 'p1' matches and the matched text is longer that p2's.
// Useful for things like: butnot(IdentifierName, ReservedWord)
function butnot(p1, p2) {
	var p1 = toParser(p1)
	var p2 = toParser(p2)
	var pid = parser_id++

	// match a but not b.
	// if both match and b's matched text is shorterthan a's,
	// a failed match is made
	return function(state) {
		var savedState = state
		var cached = savedState.getCached(pid)
		if (cached) return cached

		var br = p2(state)
		if (!br) {
			cached = p1(state)
		}
		else {
			var ar = p1(state)
			cached = !ar || ar.matched.length <= br.matched.length ? false : ar
		}
		savedState.putCached(pid, cached)
		return cached
	}
}

// 'difference' is a parser combinator that takes two parsers, 'p1' and 'p2'.
// It returns a parser that succeeds if 'p1' matches and 'p2' does not. If
// both match then if p2's matched text is shorter than p1's it is successfull.
function difference(p1, p2) {
	var p1 = toParser(p1)
	var p2 = toParser(p2)
	var pid = parser_id++

	// match a but not b. if both match and b's matched text is shorter
	// than a's, a successfull match is made
	return function(state) {
		var savedState = state
		var cached = savedState.getCached(pid)
		if (cached) return cached

		var br = p2(state)
		if (!br)
			cached = p1(state)
		else {
			var ar = p1(state)
			cached = ar.matched.length >= br.matched.length ? br : ar	
		}
		savedState.putCached(pid, cached)
		return cached
	}
}


// 'xor' is a parser combinator that takes two parsers, 'p1' and 'p2'.
// It returns a parser that succeeds if 'p1' or 'p2' match but fails if
// they both match.
function xor(p1, p2) {
	var p1 = toParser(p1)
	var p2 = toParser(p2)
	var pid = parser_id++

	// match a or b but not both
	return function(state) {
		var savedState = state
		var cached = savedState.getCached(pid)
		if (cached) return cached

		var ar = p1(state)
		var br = p2(state)
		cached = ar && br ? false :	(ar || br)
		savedState.putCached(pid, cached)
		return cached
	}
}


var nothing = {}
function nothing_action(ast) { return nothing }

// A parser combinator that takes one parser.
// It returns a parser that looks for zero or more matches of the original parser.
function repeat0(p) {
	var p = toParser(p)
	var pid = parser_id++

	return function(state) {
		var savedState = state
		var cached = savedState.getCached(pid)
		if (cached) return cached

		var ast = []
		var matched = ""
		var result
		while (result = p(state)) {
			if (result.ast !== nothing) ast.push(result.ast)
			matched += result.matched
			if (result.remaining.index == state.index) break
			state = result.remaining
		}
		cached = make_result(state, matched, ast)
		savedState.putCached(pid, cached)
		return cached
	}
}

// A parser combinator that takes one parser.
// It returns a parser that looks for one or more matches of the original parser.
function repeat1(p) {
	var p = toParser(p)
	var pid = parser_id++

	return function(state) {
		var savedState = state
		var cached = savedState.getCached(pid)
		if (cached) return cached

		var ast = []
		var matched = ""
		var result = p(state)
		if (!result)
			cached = false
		else {
			for (; result; result = p(state)) {
				if (result.ast !== nothing) ast.push(result.ast)
				matched += result.matched
				if (result.remaining.index == state.index) break
				state = result.remaining
			}
			cached = make_result(state, matched, ast)
		}
		savedState.putCached(pid, cached)
		return cached
	}
}

// A parser combinator that takes one parser.
// It returns a parser that matches zero or one matches of the original parser.
function optional(p) {
	var p = toParser(p)
	var pid = parser_id++
	
	return function(state) {
		var savedState = state
		var cached = savedState.getCached(pid)
		if (cached) return cached

		cached = p(state) || make_result(state, "", false)
		savedState.putCached(pid, cached)
		return cached
	}
}

// A parser combinator that ensures that the given parser succeeds but
// ignores its result. This can be useful for parsing literals that you
// don't want to appear in the ast. eg:
// sequence(expect("("), Number, expect(")")) => ast: Number
function expect(p) {
	return action(p, function(ast) { return nothing })
}

function chain(p, s, f) {
	var p = toParser(p)

	return action(
		sequence(p, repeat0(action(sequence(s, p), f))),
		function(ast) { return [ast[0]].concat(ast[1]) }
	)
}

// A parser combinator to do left chaining and evaluation. Like 'chain', it expects a parser
// for an item and for a seperator. The seperator parser's AST result should be a function
// of the form: function(lhs,rhs) { return x }
// Where 'x' is the result of applying some operation to the lhs and rhs AST's from the item
// parser.
function chainl(p, s) {
	var p = toParser(p)
	return action(
		sequence(p, repeat0(sequence(s, p))),
		function(ast) {
			return foldl(function(v, action) { return action[0](v, action[1]) }, ast[0], ast[1])
		}
	)
}

// A parser combinator that returns a parser that matches lists of things. The parser to
// match the list item and the parser to match the seperator need to
// be provided. The AST is the array of matched items.
function list(p, s) {
	return chain(p, s, function(ast) { return ast[1] })
}

// Like list, but ignores whitespace between individual parsers.
function wlist() {
	var parsers = []
	for (var i = 0; i < arguments.length; ++i) {
		parsers.push(whitespace(arguments[i]))
	}
	return list.apply(null, parsers)
}

// A parser that always returns a zero length match
function epsilon_p(state) {
	return make_result(state, "", nothing)
}

// Allows attaching of a function anywhere in the grammer.
// If the function returns true then parse succeeds otherwise it fails.
// Can be used for testing if a symbol is in the symbol table, etc.
function semantic(f) {
	var pid = parser_id++
	
	return function(state) {
		var savedState = state
		var cached = savedState.getCached(pid)
		if (cached)	return cached
		
		cached = f() ? make_result(state, "", nothing) : false
		savedState.putCached(pid, cached)
		return cached
	}
}

// The and predicate asserts that a certain conditional
// syntax is satisfied before evaluating another production.
// Eg: sequence(and("0"), oct_p)
// (if a leading zero, then parse octal)
// It succeeds if 'p' succeeds and fails if 'p' fails.
// It never consume any input however,
// and doesn't put anything in the resulting AST.
function and(p) {
	var p = toParser(p)
	var pid = parser_id++
	
	return function(state) {
		var savedState = state
		var cached = savedState.getCached(pid)
		if (cached)	return cached
		
		cached = p(state) ? make_result(state, "", nothing) : false
		savedState.putCached(pid, cached)
		return cached
	}
}

// The opposite of 'and'.
// It fails if 'p' succeeds and succeeds if 'p' fails.
// It never consumes any input.
// This combined with 'and' can be used for 'lookahead' and disambiguation of cases.
//
// Compare:
// sequence("a",choice("+","++"),"b")
//   parses a+b
//   but not a++b because the + matches the first part and peg's don't
//   backtrack to other choice options if they succeed but later things fail.
//
// sequence("a",choice(sequence("+", not("+")),"++"),"b")
//    parses a+b
//    parses a++b
//
function not(p) {
	var p = toParser(p)
	var pid = parser_id++
	
	return function(state) {
		var savedState = state
		var cached = savedState.getCached(pid)
		if (cached)	return cached
		
		cached = p(state) ? false : make_result(state, "", nothing)
		savedState.putCached(pid, cached)
		return cached
	}
}

},{}],3:[function(require,module,exports){
module.exports.parse_sexp = parse_sexp;

var { ps, choice, range, action, sequence, join, join_action, negate, repeat0, optional, repeat1, wsequence, whitespace, ch, butnot, expect, } = require("./jsparse.js")

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
var ign_stx = make_constant_stx("#ignore", "#ignore")
var t_stx = make_constant_stx("#t", true)
var f_stx = make_constant_stx("#f", false)
var null_stx = make_constant_stx("#null", null)
var undef_stx = make_constant_stx("#undefined", undefined)
var qualified_stx = action(sequence(id_stx, ":", id_stx), function(ast) { return ["eval", ["quote", ast[2]], ast[0]] })
var dot_stx = action(wsequence(".", x_stx), function(ast) { return ast[1] });
var compound_stx = action(
	wsequence( "(", repeat1(x_stx), optional(dot_stx), ")" ),
	function(ast) {
		return !ast[2] ? ast[1] : ast[1].concat( [".", ast[2]] );
	}
);
var quote_stx = action(sequence("'", x_stx), function(ast) { return ["quote", ast[1]] })
var cmt_stx = expect(sequence(";", repeat0(negate(line_terminator)), optional(line_terminator)))
var x_stx = whitespace(choice(qualified_stx, ign_stx, nil_stx, t_stx, f_stx, null_stx, undef_stx, number_stx, quote_stx, compound_stx, id_stx, string_stx, cmt_stx))
var program_stx = repeat0(x_stx)

},{"./jsparse.js":2}],4:[function(require,module,exports){
(function (global){(function (){
// Qua VM by Manuel Simoni (msimoni@gmail.com)

/* Abbreviations:
	c: cons
	x: expression
	xs: expressions
	op: operator
	o: operands
	o0, o1, ..: operand 0 1 .. 
	cmb: combiner
	opv: operative combiner
	apv: applicative combiner
	apv0: applicative combiner with 0 arguments
	apv1, handler: applicative combiner with 1 argument
	p: parameter
	ps: parameters
	pt: parameters tree
	arg: argument
	args: arguments
	e: environment
	eo: environment operand 
	ep: environment parameter
	xe: extended environment
	f: function
	s: supplier
	k, next, continuation: stackframe
	exc: exception
	id: identifier
	num: number
	str: string
	stx: syntax
	sym: symbol
	cmt: comment
	dbg: debugging information
*/

module.exports = function Qua() {
	
	var trace = false
	
	/* Continuations */
	function StackFrame(f, next, dbg, e) { this.f = f; this.next = next; this.dbg = dbg; this.e = e }
	StackFrame.prototype.toString = function() { return "[StackFrame " + this.f + " " + this.dbg + " " + this.e + "]" }

	function Resumption(k, s) { this.k = k; this.s = s }
	Resumption.prototype.toString = function() { return "[Resumption " + this.s + " " + this.k + "]" }
	function isResumption(m) { return m /*instanceof Resumption*/ }
	function resumeFrame(m) { return m.k.f(new Resumption(m.k.next, m.s)) }

	function Suspension(prompt, handler) { this.prompt = prompt; this.handler = handler; this.k = null	}
	Suspension.prototype.toString = function() { return "[Suspension " + this.prompt + " " + this.handler + " " + this.k +  "]" }
	function isSuspension(x) { return x instanceof Suspension }
	function suspendFrame(suspension, f, dbg, e) {
		suspension.k = new StackFrame(f, suspension.k, dbg, e)
		return suspension
	}
	
	
	/* Forms */
	function Nil() { }
	Nil.prototype.wat_match = function(e, rhs) { if (rhs !== nil) return "too many arguments" /*+ ", nil expected, but got: " + to_string(rhs)*/ }
	Nil.prototype.toString = function() { return "()" }
 	var nil = new Nil()
 	
	function Ign() { }
	Ign.prototype.wat_match = function(e, rhs) { return null }
	Ign.prototype.toString = function() { return "#ignore" }
	var ign = new Ign()
	
	
	/* Evaluation Core */
	function evaluate(m, e, x) {
		if (trace) print("eval:", x)
		return x && x.wat_eval ? x.wat_eval(m, e) : x
	}
	
	function Sym(name) { this.name = name }
	Sym.prototype.wat_eval = function(m, e) { return lookup(e, this.name) }
	Sym.prototype.wat_match = function(e, rhs) { e.bindings[this.name] = rhs; if (trace) print("bind:", this.name, rhs, e); }
	Sym.prototype.toString = function() { return this.name }
	function sym(name) { return new Sym(name) }
	function sym_name(sym) { return sym.name }

	function Cons(car, cdr) { this.car = car; this.cdr = cdr }
	Cons.prototype.wat_eval = function(m, e) {
		var op = isResumption(m) ? resumeFrame(m) : evaluate(null, e, this.car)
		return isSuspension(op) ? suspendFrame(op, m=> this.wat_eval(m, e)) : combine(null, e, op, this.cdr)
	}
	Cons.prototype.wat_match = function(e, rhs) {
		if (!this.car.wat_match) return "cannot match against: " + this.car
		if (!this.cdr.wat_match) return "cannot match against: " + this.cdr 
		this.car.wat_match(e, car(rhs)); return this.cdr.wat_match(e, cdr(rhs));
	}
	Cons.prototype.toString = function() {
		return "(" + cons_to_string(this) + ")"
		function cons_to_string(c) {
			if (c.cdr === nil) return to_string(c.car)
			if (c.cdr instanceof Cons) return to_string(c.car) + " " + cons_to_string(c.cdr)
			return to_string(c.car) + " . " + to_string(c.cdr)
		}
	}
	function cons(car, cdr) { return new Cons(car, cdr) }
	function car(cons) { // tc
		return cons instanceof Cons ? cons.car : error("not a cons: " + to_string(cons))
	}
	function cdr(cons) { // tc
		return cons instanceof Cons ? cons.cdr : error("not a cons: " + to_string(cons))
	}
	function elt(o, i) { for (; i>0; i-=1) o=cdr(o); return car(o); }
	function len(o) { for (var i=0; o instanceof Cons; o=cdr(o)) i+=1; return i; }
	
	
	/* Environment */
	function Env(parent) { this.bindings = Object.create(!parent ? null : parent.bindings); this.parent = parent }
	Env.prototype.toString = function() {
		if (this == the_environment)
			var s = '[The-Env]'
		else {	
		 	var s=''; for (let n in Object.getOwnPropertyNames(this.bindings)) s+= (!s ? "" : " ") + n + "=" + this.bindings[n];
		}
		return "[Env" + (!s ? "" : ", " + s) + (!this.parent ? "" : " " + this.parent.toString()) + "]"
	}
	function env(parent) { return new Env(parent) }
	function lookup(e, name) {
		if (!(name in e.bindings)) error("unbound: " + name)
		if (trace) print("lookup:", name)
		return e.bindings[name]
	}
	
	
	/* Bind */
	function bind(e, lhs, rhs, exp) {
		if (!lhs.wat_match) return error("cannot match against: " + lhs)
		try {
			var msg = lhs.wat_match(e, rhs);
		}
		catch (exc) { // only error in car() or cdr()
			var msg = "insufficient arguments" // + " because " + exc.getMessage()
		}
		return !msg ? ign : error(msg + " in bind: " + lhs + (!exp ? "" : " of: " + exp) + " with: " + rhs)
	}
	
	
	/* Operative & Applicative Combiners */
	function combine(m, e, cmb, o) {
		if (trace) print("combine:", cons(cmb, o))
		if (cmb && cmb.wat_combine)	return cmb.wat_combine(m, e, o)
		// TODO per default le Function non wrapped dovrebbero essere operative e non applicative
		if (cmb instanceof Function)
			return jswrap(cmb).wat_combine(m, e, o) // Function x default applicative
			//	 return Apv.prototype.wat_combine.call(new JSFun(cmb), m, e, o) 
			//   return new JSFun(cmb).wat_combine(m, e, o) // Function x default operative
			//   return jsfun(cmb).wat_combine(m, e, o)
			//   return cmb.apply(null, list_to_array(o))
		return error("not a combiner: " + to_string(cmb) + " in: " + cons(cmb, o))
	}
	
	function Opv(p, ep, x, e) { this.p = p; this.ep = ep; this.x = x; this.e = e }
	Opv.prototype.wat_combine = function(m, e, o) {
		var xe = env(this.e); bind(xe, this.p, o, this); bind(xe, this.ep, e, this); return evaluate(null, xe, this.x)
	}
	Opv.prototype.toString = function() { return "[Opv " + to_string(this.p) + " " + to_string(this.ep) + " " + to_string(this.x) + "]" }

	function Apv(cmb) { this.cmb = cmb }
	Apv.prototype.wat_combine = function(m, e, o) {
		var args = isResumption(m) ? resumeFrame(m) : evalArgs(null, e, o, nil)
		return isSuspension(args) ? suspendFrame(args, m=> this.wat_combine(m, e, o)) : this.cmb.wat_combine(null, e, args)
	}
	function evalArgs(m, e, todo, done) {
		if (todo === nil) return reverse_list(done) 
		var arg = isResumption(m) ? resumeFrame(m) : evaluate(null, e, car(todo))
		return isSuspension(arg) ? suspendFrame(arg, m=> evalArgs(m, e, todo, done)) : evalArgs(null, e, cdr(todo), cons(arg, done))
	}
	Apv.prototype.toString = function() { return "[Apv " + to_string(this.cmb) + "]" }
	function wrap(cmb) { return cmb && cmb.wat_combine ? new Apv(cmb) : error("cannot wrap: " + cmb) } // type check
	function unwrap(apv) { return apv instanceof Apv ? apv.cmb : error("cannot unwrap: " + apv) } // type check
	
	
	/* Built-in Combiners */
	function Vau() { }; function Def() { }; function Eval() { }
	Vau.prototype.wat_combine = function(m, e, o) {
		// o = (pt ep expr)
		if (len(o) > 3) return error("too many operands in: " + cons(this, o));
		var pt = elt(o, 0)
		var ep = elt(o, 1)
		var msg = pcheck(pt, ep); if (msg) return error(msg + " of: " + cons(this, o))
		return new Opv(pt, ep, elt(o, 2), e)
	}
	Vau.prototype.toString = function() { return "vm-vau" }
	Def.prototype.wat_combine = function(m, e, o) { // error handling
		// o = (pt arg)
		if (len(o) > 2) return error("too many operands in: " + cons(this, o));
		var pt = elt(o, 0) // almeno un parametro, singolo o nel parameters tree insieme ad #ignore
		if (!(pt instanceof Sym)) {
			if (!(pt instanceof Cons)) return error("not a symbol: " + pt + " in: " + cons(this, o)) 
			var msg = pcheck(pt); if (msg) return error(msg + " of: " + cons(this, o))
		}
		var arg = elt(o, 1)
		var val = isResumption(m) ? resumeFrame(m) : evaluate(null, e, arg)
		return isSuspension(val) ? suspendFrame(val, m=> this.wat_combine(m, e, o)) : bind(e, pt, val, cons(this, o))
	}
	Def.prototype.toString = function() { return "vm-def" }
	function pcheck(pt, ep) {
		var symbols = new Set()
		if (pt != nil && pt != ign) {	var msg = pcheck(pt); if (msg) return msg }
		if (!ep) return symbols.size > 0 ? null : "no one symbol in: " + pt
		if (ep == ign) return null
		if (!(ep instanceof Sym)) return "not a #ignore or symbol: " + ep
		return !symbols.has(ep.name) ? null : "not a unique symbol: " + ep
		function pcheck(p) {
			if (p == ign) return null
			if (p instanceof Sym) return !symbols.has(p.name) ? (symbols.add(p.name), null) : "not a unique symbol: " + p + (p == ptree ? "" : " in: " + ptree)
			if (!(p instanceof Cons)) return "not a #ignore or symbol: " + p + (p == pt ? "" : " in: " + pt) 
			var msg = pcheck(p.car); if (msg) return msg
			return p.cdr == nil ? null : pcheck(p.cdr)
		}
	}
	Eval.prototype.wat_combine = function(m, e, o) { // error handling
		// o = (x eo)
		var x = elt(o, 0)
		var eo = elt(o, 1)
		return evaluate(m, eo, x)
	}
	Eval.prototype.toString = function() { return "vm-eval" }
	
	
	/* First-order Control */
	function Begin() { }; function If() { }; function Loop() { }; function Catch() { }; function Finally() { }
	Begin.prototype.wat_combine = function(m, e, o) {
		//o = (.. xs)
		return o === nil ? null : begin(m, e, o)
		function begin(m, e, xs) {
			var res = isResumption(m) ? resumeFrame(m) : evaluate(null, e, car(xs))
			return isSuspension(res) ? suspendFrame(res, m=> begin(m, e, xs)) : (kdr=> kdr === nil ? res : begin(null, e, kdr))(cdr(xs))
		}
	}
	Begin.prototype.toString = function() { return "vm-begin" }
	If.prototype.wat_combine = function(m, e, o) {
		// o = (test then else) 
		if (len(o) > 3) return error("too many operands in: " + cons(this, o));
		var test = isResumption(m) ? resumeFrame(m) : evaluate(null, e, elt(o, 0))
		return isSuspension(test) ? suspendFrame(test, m=> this.wat_combine(m, e, o)) : evaluate(null, e, test ? elt(o, 1) : elt(o, 2))
	}
	If.prototype.toString = function() { return "vm-if" }
	Loop.prototype.wat_combine = function self(m, e, o) {
		// o = (x)
		var first = true // only resume once
		while (true) {
			var res = first && isResumption(m) ? resumeFrame(m) : evaluate(null, e, elt(o, 0))
			first = false
			if (isSuspension(res)) return suspendFrame(res, m=> self(m, e, o), elt(o, 0), e)
		}
	}
	Loop.prototype.toString = function() { return "vm-loop" }
	Catch.prototype.wat_combine = function self(m, e, o) {
		//o = (x handler)
		var x = elt(o, 0)
		var handler = elt(o, 1)
		try {
			var res = isResumption(m) ? resumeFrame(m) : evaluate(null, e, x)
		}
		catch (exc) {
			// unwrap handler to prevent eval if exc is sym or cons
			var res = combine(null, e, unwrap(handler), list(exc))
		}
		if (isSuspension(res)) suspendFrame(res, m=> self(m, e, o), x, e)
		return res
	}
	Catch.prototype.toString = function() { return "vm-catch" }
	Finally.prototype.wat_combine = function self(m, e, o) {
		// o = (prot cleanup)
		var prot = elt(o, 0)
		var cleanup = elt(o, 1)
		try {
			var res = isResumption(m) ? resumeFrame(m) : evaluate(null, e, prot)
			if (isSuspension(res)) suspendFrame(res, m=> self(m, e, o), prot, e)
		}
		finally {
			return isSuspension(res) ? res : doCleanup(null, e, cleanup, res)
		}
		function doCleanup(m, e, cleanup, res) {
			var fres = isResumption(m) ? resumeFrame(m) : evaluate(null, e, cleanup)
			if (isSuspension(fres)) suspendFrame(fres, m=> doCleanup(m, e, cleanup, res), cleanup, e)
			return fres
		}
	}
	Finally.prototype.toString = function() { return "vm-finally" }
	
	
	/* Delimited Control */
	function PushPrompt() { }; function TakeSubcont() {	}; function PushSubcont() { }; function PushPromptSubcont() { }
	PushPrompt.prototype.wat_combine = function self(m, e, o) {
		// o = (prompt exp)
		if (len(o) > 2) return error("too many operands in: " + cons(this, o));
		var prompt = elt(o, 0)
		var x = elt(o, 1)
		var res = isResumption(m) ? resumeFrame(m) : evaluate(null, e, x)	
		if (!isSuspension(res)) return res
		if (res.prompt !== prompt) return suspendFrame(res, m=> self(m, e, o), x, e)
		var continuation = res.k
		var handler = res.handler
		return combine(null, e, handler, cons(continuation, nil))
	}
	PushPrompt.prototype.toString = function() { return "vm-push-prompt" }
	TakeSubcont.prototype.wat_combine = function(m, e, o) {
		// o = (prompt handler)
		if (len(o) > 2) return error("too many operands in: " + cons(this, o));
		var prompt = elt(o, 0)
		var handler = elt(o, 1)
		if (!(handler instanceof Apv)) return error("not a one applicative combiner: " + handler)
		var cap = new Suspension(prompt, handler)
		return suspendFrame(cap, m=> combine(null, e, m.s, nil), this, e)
	}
	TakeSubcont.prototype.toString = function() { return "vm-take-subcont" }
	PushSubcont.prototype.wat_combine = function self(m, e, o) {
		// o = (k apv0)
		if (len(o) > 2) return error("too many operands in: " + cons(this, o));
		var k = elt(o, 0)
		if (!(k instanceof StackFrame)) return error("not a stackframe: " + k);
		var apv0 = elt(o, 1)
		if (!(apv0 instanceof Apv)) return error("not a zero args applicative combiner: " + apv0);
		var res = isResumption(m) ? resumeFrame(m) : resumeFrame(new Resumption(k, apv0))
		if (isSuspension(res)) suspendFrame(res, m=> self(m, e, o), apv0, e)
		return res
	}
	PushSubcont.prototype.toString = function() { return "vm-push-subcont" }
	PushPromptSubcont.prototype.wat_combine = function self(m, e, o) {
		// o = (prompt k apv0)
		if (len(o) > 2) return error("too many operands in: " + cons(this, o));
		var prompt = elt(o, 0)
		var k = elt(o, 1)
		if (!(o1 instanceof StackFrame)) return error("not a stackframe: " + o1); 
		var apv0 = elt(o, 2)
		if (!(o2 instanceof Apv)) return error("not a zero args applicative combiner: " + o2); 
		var res = isResumption(m) ? resumeFrame(m) : resumeFrame(new Resumption(k, apv0))
		if (!isSuspension(res)) return res
		if (res.prompt !== prompt) return suspendFrame(res, m=> self(m, e, o), apv0, e)
		var continuation = res.k
		var handler = res.handler
		return combine(null, e, handler, cons(continuation, nil))
	}
	PushPromptSubcont.prototype.toString = function() { return "vm-push-prompt-subcont" }
	
	
	/* Dynamic Variables */
	function DV(val) { this.val = val }; function DNew() { }; function DRef() { }; function DLet() { }
	DNew.prototype.wat_combine = function(m, e, o) { return new DV(elt(o, 0)) }
	DNew.prototype.toString = function() { return "vm-dnew" }
	DRef.prototype.wat_combine = function(m, e, o) { return elt(o, 0).val }
	DRef.prototype.toString = function() { return "vm-dref" }
	DLet.prototype.wat_combine = function self(m, e, o) {
		var dv = elt(o, 0)
		var val = elt(o, 1)
		var x = elt(o, 2)
		var oldVal = dv.val
		try {
			dv.val = val
			var res = isResumption(m) ? resumeFrame(m) : evaluate(null, e, x)
			if (isSuspension(res)) suspendFrame(res, m=> self(m, e, o), x, e)
			return res
		}
		finally {
			dv.val = oldVal
		}
	}
	DLet.prototype.toString = function() { return "vm-dlet" }
	
	
	/* Error handling */
	var ROOT_PROMPT = {}
	ROOT_PROMPT.toString = function() { return "ROOT_PROMPT" }
	function push_root_prompt(x) { return list(new PushPrompt(), ROOT_PROMPT, x) }
	function error(err) {
		//console.log(err)
		var user_break = the_environment.bindings["user-break"]
		if (!user_break) throw err
		return combine(null, the_environment, user_break, list(err))
	}
	
	
	/* Utilities */
	function list() {
		return array_to_list(true, arguments)
	}
	function list_star() {
		return array_to_list(false, arguments)
	}
	function array_to_list(b, args) {
		var len = args.length-1
		var c = b || len < 0 ? nil : args[len]
		for (var i=len-(b?0:1); i>=0; i-=1) c = cons(args[i], c)
		return c
	}
	function list_to_array(c) {
		for (var res = []; c !== nil; c = cdr(c)) res.push(car(c)); return res
	}
	function reverse_list(list) {
		for (var res = nil; list !== nil; list = cdr(list)) res = cons(car(list), res); return res
	}
	function print() {
		console.log(to_string(... arguments))
		return arguments[arguments.length - 1]
	}
	function eq(a, b) {
		if (a instanceof Cons && b instanceof Cons) return eq(a.car, b.car) && eq(a.cdr, b.cdr)
		if (a instanceof Sym && b instanceof Sym) return a.name === b.name
		return a === b 
	}
	function assert(a, b) {
		try {
			var v = evaluate(null, env(the_environment), a)
			if (arguments.length == 1)
				print(a, "should be throw but is", v)
			else if (!eq(v, b))
				print(a, "should be", b, "but is", v);
		}
		catch (t) {
			if (arguments.length > 1) print(a, "throw", t);
		}
	}
	
	
	/* Bytecode parser */
	function parse_bytecode(obj) {
		switch (Object.prototype.toString.call(obj)) {
			case "[object String]": return obj === "#ignore" ? ign : sym(obj)
			case "[object Array]": return parse_bytecode_array(obj)
			default: return obj
		}
		function parse_bytecode_array(arr) {
			if (arr.length == 0) return nil
			if (arr.length == 2 && arr[0] === "wat-string") return arr[1]
			var head = cons(parse_bytecode(arr[0]), nil), c = head
			for (var i=1; i<arr.length; i+=1) {
				if (arr[i] !== ".") { c = c.cdr = cons(parse_bytecode(arr[i]), nil); continue }
				if (i != arr.length-2) throw error(". not is the penultimate element in " + arr)
				c.cdr = parse_bytecode(arr[i+1])
				return head
			}
			return head
		}
	}
	
	
	/* JSNI */
	function JSFun(fun) { this.fun = fun }
	JSFun.prototype.wat_combine = function(m, e, o) { return this.fun.apply(null, list_to_array(o))	}
	JSFun.prototype.toString = function() { return "[JSFun " + this.fun + "]" }
	function jsfun(fun) { return Object.prototype.toString.call(fun) === "[object Function]" ? new JSFun(fun) : error("no fun") }
	function jswrap(fun) { return wrap(jsfun(fun)) }
	
	var js_types = ["Array", "Boolean", "Date", "Function", "Number", "Object", "RegExp", "String"]
	function is_type(obj, type_obj, type_name) {
		if (!type_obj) return error("type is undefined")
		if (js_types.indexOf(type_name) === -1) return obj instanceof type_obj
		return toString.call(obj) === "[object " + type_name + "]"
	}
	function js_unop(op) { return jswrap(new Function("a", "return (" + op + " a)")) }
	function js_binop(op) { return jswrap(new Function("a", "b", "return (a " + op + " b)")) }
	function js_invoker(method_name) {
		if (!method_name) return error("method name is null/undefined")
		return jswrap(
			function(rcv) {
				if (!rcv) return error("receiver is null/undefined")
				if (arguments.length < 1) return error("invoker called with wrong args: " + arguments)
				var method = rcv[method_name]
				if (!method) return error("method not found: " + method_name + " in: " + to_string(rcv))
				return method.apply(rcv, Array.prototype.slice.call(arguments, 1))
			}
		)
	}
	function js_getter(prop_name) {
		var getter = jswrap(
			function(rcv) {
				if (arguments.length !== 1) return error(prop_name + " getter called with wrong args")
				if ((rcv !== undefined) && (rcv !== null)) return rcv[prop_name]
				return error("can't get " + prop_name + " of " + rcv)
			}
		)
		getter.wat_setter = js_setter(prop_name)
		return getter
	}
	function js_setter(prop_name) {
		return jswrap(
			function(val, rcv) {
				if (arguments.length !== 2) return error("setter called with wrong args: " + arguments)
				if (rcv !== undefined && rcv !== null) return rcv[prop_name] = val
				return error("can't set " + prop_name + " of " + rcv)
			}
		)
	}
	function make_prototype(name) {
		var prop_names = Array.prototype.slice.call(arguments, 1)
		var param_names = prop_names.join(",")
		var param_inits = prop_names.map(prop_name=> "this." + prop_name + "=" + prop_name + ";").join("")
		return eval("(function " + name + "(" + param_names + "){" + param_inits + "})")
	}
	function jsnew(ctor) {
		var factoryFunction = ctor.bind.apply(ctor, arguments)
		return new factoryFunction()
	}
	function js_function(cmb) {
		return function() {
			var args = cons(this, array_to_list(Array.prototype.slice.call(arguments)))
			return combine(null, null, cmb, args)
		}
	}
	var JS_GLOBAL = jswrap(name=> global[name])
	JS_GLOBAL.wat_setter = jswrap((new_val, name)=> global[name] = new_val)
	
	
	/* Setter - you are not expected to understand this - immediately */
	var SETTER = jswrap(obj=> obj.wat_setter)
	SETTER.wat_setter = jswrap((new_setter, obj)=> obj.wat_setter = new_setter)
	
	
	/* Stringification */
	function to_string() {
		var s=''; for (let arg of arguments) s += (!s?'':' ') + to_string(arg); return s
		/* TODO sostituito dal seguente
		function to_string(obj) {
			if (toString.call(obj) === "[object String]") return obj //JSON.stringify(obj)
			if (obj !== null && obj !== undefined) return obj.toString()
			if (obj !== null) return "#null"
			if (obj !== undefined) return "#undefined"
			return Object.prototype.toString.call(obj)
		}
		*/
		function to_string(obj) {
			if (obj === null) return "#null"
			if (obj === undefined) return "#undefined"
			if (toString.call(obj) === "[object String]") return obj //JSON.stringify(obj) // con o senza apici?
			return obj.toString() // Object.prototype.toString.call(obj)
		}
	}
	
	
	/* Bootstrap */
	var builtin_bytecode =
		["vm-begin",
			// Basics
			["vm-def", "vm-vau", new Vau()],
			["vm-def", "vm-eval", wrap(new Eval())],
			["vm-def", "vm-make-environment", jswrap(parent=> env(parent))],
			["vm-def", "vm-wrap", jswrap(wrap)],
			["vm-def", "vm-unwrap", jswrap(unwrap)],
			// Values
			["vm-def", "vm-cons", jswrap(cons)],
			["vm-def", "vm-cons?", jswrap(obj=> obj instanceof Cons)],
			["vm-def", "vm-nil?", jswrap(obj=> obj === nil)],
			["vm-def", "vm-string-to-symbol", jswrap(sym)],
			["vm-def", "vm-symbol?", jswrap(obj=> obj instanceof Sym)],
			["vm-def", "vm-symbol-name", jswrap(sym_name)],
			// First-order Control
			["vm-def", "vm-if", new If()],
			["vm-def", "vm-loop", new Loop()],
			["vm-def", "vm-throw", jswrap(function(err) { throw err })],
			["vm-def", "vm-catch", new Catch()],
			["vm-def", "vm-finally", new Finally()],
			// Delimited Control
			["vm-def", "vm-push-prompt", new PushPrompt()],
			["vm-def", "vm-take-subcont", wrap(new TakeSubcont())],
			["vm-def", "vm-push-subcont", wrap(new PushSubcont())],
			["vm-def", "vm-push-prompt-subcont", wrap(new PushPromptSubcont())],
			// Dynamically-scoped Variables
			["vm-def", "vm-dnew", wrap(new DNew())],
			["vm-def", "vm-dlet", new DLet()],
			["vm-def", "vm-dref", wrap(new DRef())],
			// Errors
			["vm-def", "vm-root-prompt", ROOT_PROMPT],
			["vm-def", "vm-error", jswrap(error)],
			// JS Interface
			["vm-def", "vm-js-wrap", jswrap(jswrap)],
			["vm-def", "vm-js-unop", jswrap(js_unop)],
			["vm-def", "vm-js-binop", jswrap(js_binop)],
			["vm-def", "vm-js-getter", jswrap(js_getter)],
			["vm-def", "vm-js-setter", jswrap(js_setter)],
			["vm-def", "vm-js-invoker", jswrap(js_invoker)],
			["vm-def", "vm-js-function", jswrap(js_function)],
			["vm-def", "vm-js-global", JS_GLOBAL],
			["vm-def", "vm-js-make-object", jswrap(function() { return {} })],
			["vm-def", "vm-js-make-prototype", jswrap(make_prototype)],
			["vm-def", "vm-js-new", jswrap(jsnew)],
			["vm-def", "vm-type?", jswrap(is_type)],
			// Setters
			["vm-def", "vm-setter", SETTER],
			// Utilities
			["vm-def", "vm-list", jswrap(list)],
			["vm-def", "vm-list*", jswrap(list_star)],
			["vm-def", "vm-list-to-array", jswrap(list_to_array)],
			["vm-def", "vm-array-to-list", jswrap(array_to_list)],
			["vm-def", "vm-reverse-list", jswrap(reverse_list)],
			["vm-def", "==", jswrap((a,b)=> a == b)],
			["vm-def", "eq", jswrap((a,b)=> eq(a,b))],
			["vm-def", "to-string", jswrap(to_string)],
			["vm-def", "assert", jsfun(assert)],
			["vm-def", "trace", jsfun(b=>trace=b)],
		]
	var the_environment = env()
	bind(the_environment, sym("vm-def"), new Def())
	bind(the_environment, sym("vm-begin"), new Begin())
	evaluate(null, the_environment, parse_bytecode(builtin_bytecode))
	
	
	/* API */
	this.exec = function(bytecode) {
		var wrapped = push_root_prompt(cons(new Begin(), parse_bytecode(bytecode)))
		var res = evaluate(null, the_environment, wrapped)
		if (isSuspension(res)) throw "prompt not found: " + res.prompt
		return res
	}
	this.call = function(fun_name) {
		return this.exec(list(sym(fun_name), parse_bytecode(Array.prototype.slice.call(arguments, 1))))
	}
	this.get = function(var_name) {
		return this.exec(sym(var_name))
	}
}

}).call(this)}).call(this,typeof global !== "undefined" ? global : typeof self !== "undefined" ? self : typeof window !== "undefined" ? window : {})
},{}],5:[function(require,module,exports){
var VM = require("./vm.js")
var boot_bytecode = require("./build/boot.js").main
var parser = require("./parser.js")
//var boot_bytecode = parser.parse_sexp(require('fs').readFileSync('./boot.wat', 'utf8'))

var vm = new VM()
vm.exec(boot_bytecode)

module.exports.vm = function() {
    return {
        eval: function(sexp) {
			var e = parser.parse_sexp(sexp)
			//console.log(vm.e)
			return vm.exec(e)
        }
    };
};

},{"./build/boot.js":1,"./parser.js":3,"./vm.js":4}]},{},[5])(5)
});
