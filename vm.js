// Qua VM by Manuel Simoni (msimoni@gmail.com)
module.exports = function Qua() {
	
	/* Continuations */
	function StackFrame(fun, next, dbg, e) { this.fun = fun; this.next = next; this.dbg = dbg; this.e = e }

	function Resumption(k, f) { this.k = k; this.f = f }
	function isResumption(m) { return m instanceof Resumption }
	function resumeFrame(m) { return m.k.fun(new Resumption(m.k.next, m.f)) }

	function Suspension(prompt, handler) { this.prompt = prompt; this.handler = handler; this.k = null	}
	function isSuspension(x) { return x instanceof Suspension }
	function suspendFrame(suspension, fun, dbg, e) {
		suspension.k = new StackFrame(fun, suspension.k, dbg, e)
		return suspension
	}

	function monadic(m, a, b) {
		if (!isResumption(m))
			var res = a()
		else
			var res = resumeFrame(m)
		if (!isSuspension(res))
			return b(res)
		return suspendFrame(res, function(m) { return monadic(m, a, b) })
	}
	
	/* Evaluation Core */
	function evaluate(m, e, x) {
		 if (!x || !x.wat_eval) return x
		 return x.wat_eval(m, e)
	}
	function Sym(name) { this.name = name }
	function sym(name) { return new Sym(name) }
	Sym.prototype.wat_eval = function(m, e) { return lookup(e, this.name) }
	function Cons(car, cdr) { this.car = car; this.cdr = cdr }
	Cons.prototype.wat_eval = function(m, e) {
		var that = this
		return monadic(
			null,
			function() { return evaluate(null, e, car(that)) },
			function(op) { return combine(null, e, op, cdr(that)) }
		)
	}
	
	/* Operative & Applicative Combiners */
	function combine(m, e, cmb, o) {
		if (cmb && cmb.wat_combine)
			return cmb.wat_combine(m, e, o)
		if (cmb instanceof Function)
			return jswrap(cmb).wat_combine(m, e, o)
		return error("not a combiner: " + to_string(cmb))
	}
	function Opv(p, ep, x, e) { this.p = p; this.ep = ep; this.x = x; this.e = e }
	Opv.prototype.wat_combine = function(m, e, o) {
		var that = this
		var xe = env(that.e)
		return monadic(
			null,
			function() { return bind(xe, that.p, o) },
			function() {
				return monadic(
					null,
					function() { return bind(xe, that.ep, e) },
					function() { return evaluate(null, xe, that.x) }
				)
			}
		)
	}
	function Apv(cmb) { this.cmb = cmb }
	Apv.prototype.wat_combine = function(m, e, o) {
		var that = this
		return monadic(
			null,
			function() { return evalArgs(null, e, o, NIL) },
			function(args) { return that.cmb.wat_combine(null, e, args) }
		)
		function evalArgs(m, e, todo, done) {
			if (todo === NIL) return reverse_list(done) 
			return monadic(
				null,
				function() { return evaluate(null, e, car(todo)) },
				function(arg) { return evalArgs(null, e, cdr(todo), cons(arg, done)) }
			)
		}
	}
	// TODO capire perché JSFun non ha wat_combine che dovrebbe avere
	function wrap(cmb) { return cmb && (cmb.wat_combine || cmb instanceof JSFun) ? new Apv(cmb) : error("cannot wrap: " + cmb) } // type check
	function unwrap(apv) { return apv instanceof Apv ? apv.cmb : error("cannot unwrap: " + apv) } // type check
	
	/* Built-in Combiners */
	function Vau() { }; function Def() { }; function Eval() { }
	Vau.prototype.wat_combine = function(m, e, o) {
		// o = (ptree envp expr)
		var ptree = elt(o, 0)
		var err = pcheck(ptree); if (err) return err
		return new Opv(ptree, elt(o, 1), elt(o, 2), e)
	}
	Def.prototype.wat_combine = function(m, e, o) { // error handling
		var lhs = elt(o, 0);
		var err = pcheck(lhs); if (err) return err
		var rhs = elt(o, 1)
		return monadic(
			null,
			function() { return evaluate(null, e, rhs) },
			function(val) { return bind(e, lhs, val) }
		)
	}
	function pcheck(p) {
		return pcheck(p)
		function pcheck(x) {
			if (x === NIL || x == IGN || x instanceof Sym) return
			if (x instanceof Cons) {
				 var err = pcheck(car(x))
				 if (err) return err
				 return pcheck(cdr(x))
			}
			return error("not a symbol: " + to_string(x) + " in: " + p)
		}
	}
	Eval.prototype.wat_combine = function(m, e, o) { // error handling
		var x = elt(o, 0)
		var e = elt(o, 1)
		return evaluate(m, e, x)
	}
	
	/* First-order Control */
	function Begin() { }; function If() { }; function Loop() { }; function Catch() { }; function Finally() { }
	Begin.prototype.wat_combine = function(m, e, o) {
		return o === NIL ? null : begin(m, e, o)
		function begin(m, e, xs) {
			return monadic(
				null,
				function() { return evaluate(null, e, car(xs)) },
				function(res) {
					var kdr = cdr(xs)
					return kdr === NIL ? res : begin(null, e, kdr)
				}
			)
		}
	}
	If.prototype.wat_combine = function(m, e, o) {
		return monadic(
			null,
			function() { return evaluate(null, e, elt(o, 0)) },
			function(test) { return evaluate(null, e, test ? elt(o, 1) : elt(o, 2)) }
		)
	}
	Loop.prototype.wat_combine = function self(m, e, o) {
		var first = true // only resume once
		while (true) {
			var res = first && isResumption(m) ? resumeFrame(m) : evaluate(null, e, elt(o, 0))
			first = false
			if (isSuspension(res)) return suspendFrame(res, function(m) { return self(m, e, o) }, elt(o, 0), e)
		}
	}
	Catch.prototype.wat_combine = function self(m, e, o) {
		var x = elt(o, 0)
		var handler = elt(o, 1)
		try {
			var res = isResumption(m) ? resumeFrame(m) : evaluate(null, e, x)
		}
		catch (exc) {
			// unwrap handler to prevent eval if exc is sym or cons
			var res = combine(null, e, unwrap(handler), list(exc))
		}
		if (isSuspension(res)) suspendFrame(res, function(m) { return self(m, e, o) }, x, e)
		return res
	}
	Finally.prototype.wat_combine = function self(m, e, o) {
		var prot = elt(o, 0)
		var cleanup = elt(o, 1)
		try {
			var res = isResumption(m) ? resumeFrame(m) : evaluate(null, e, prot)
			if (isSuspension(res)) suspendFrame(res, function(m) { return self(m, e, o) }, prot, e)
		}
		finally {
			return isSuspension(res) ? res : doCleanup(null, e, cleanup, res)
		}
		function doCleanup(m, e, cleanup, res) {
			var fres = isResumption(m) ? resumeFrame(m) : evaluate(null, e, cleanup)
			if (isSuspension(fres)) suspendFrame(fres, function(m) { return doCleanup(m, e, cleanup, res) }, cleanup, e)
			return fres
		}
	}
	
	/* Delimited Control */
	function PushPrompt() { }; function TakeSubcont() {	}; function PushSubcont() { }; function PushPromptSubcont() { }
	PushPrompt.prototype.wat_combine = function self(m, e, o) {
		var prompt = elt(o, 0)
		var x = elt(o, 1)
		var res = isResumption(m) ? resumeFrame(m) : evaluate(null, e, x)	
		if (!isSuspension(res)) return res
		if (res.prompt !== prompt) return suspendFrame(res, function(m) { return self(m, e, o) }, x, e)
		var continuation = res.k
		var handler = res.handler
		return combine(null, e, handler, cons(continuation, NIL))
	}
	TakeSubcont.prototype.wat_combine = function(m, e, o) {
		var prompt = elt(o, 0)
		var handler = elt(o, 1)
		var cap = new Suspension(prompt, handler)
		return suspendFrame(cap, function(m) { return combine(null, e, m.f, NIL) }, this, e)
	}
	PushSubcont.prototype.wat_combine = function self(m, e, o) {
		var thek = elt(o, 0)
		var thef = elt(o, 1)
		var res = isResumption(m) ? resumeFrame(m) : resumeFrame(new Resumption(thek, thef))
		if (isSuspension(res)) suspendFrame(res, function(m) { return self(m, e, o) }, thef, e)
		return res
	}
	PushPromptSubcont.prototype.wat_combine = function self(m, e, o) {
		var prompt = elt(o, 0)
		var thek = elt(o, 1)
		var thef = elt(o, 2)
		var res = isResumption(m) ? resumeFrame(m) : resumeFrame(new Resumption(thek, thef))
		if (!isSuspension(res)) return res
		if (res.prompt !== prompt) return suspendFrame(res, function(m) { return self(m, e, o) }, thef, e)
		var continuation = res.k
		var handler = res.handler
		return combine(null, e, handler, cons(continuation, NIL))
	}
	
	/* Dynamic Variables */
	function DV(val) { this.val = val }; function DNew() { }; function DRef() { }; function DLet() { }
	DNew.prototype.wat_combine = function(m, e, o) { return new DV(elt(o, 0)) }
	DRef.prototype.wat_combine = function(m, e, o) { return elt(o, 0).val }
	DLet.prototype.wat_combine = function self(m, e, o) {
		var dv = elt(o, 0)
		var val = elt(o, 1)
		var x = elt(o, 2)
		var oldVal = dv.val
		try {
			dv.val = val
			var res = isResumption(m) ? resumeFrame(m) : evaluate(null, e, x)
			if (isSuspension(res)) suspendFrame(res, function(m) { return self(m, e, o) }, x, e)
			return res
		}
		finally {
			dv.val = oldVal
		}
	}
	
	/* Forms */
	function Nil() { }; var NIL = new Nil()
	function Ign() { }; var IGN = new Ign()
	function cons(car, cdr) { return new Cons(car, cdr) }
	function car(cons) { // tc
		return cons instanceof Cons ? cons.car : error("not a cons: " + to_string(cons))
	}
	function cdr(cons) { // tc
		return cons instanceof Cons ? cons.cdr : error("not a cons: " + to_string(cons))
	}
	function elt(cons, i) { return i === 0 ? car(cons) : elt(cdr(cons), i - 1) }
	function sym_name(sym) { return sym.name }
	
	/* Environment */
	function Env(parent) { this.bindings = Object.create(!parent ? null : parent.bindings); this.parent = parent }
	function env(parent) { return new Env(parent) }
	function lookup(e, name) { return name in e.bindings ? e.bindings[name] : error("unbound: " + name) }
	
	function bind(e, lhs, rhs) {
		if (lhs.wat_match)
			return lhs.wat_match(e, rhs)
		return error("cannot match against: " + lhs)
	}
	Sym.prototype.wat_match = function(e, rhs) { return e.bindings[this.name] = rhs }
	Cons.prototype.wat_match = function(e, rhs) {
		var that = this
		return monadic(
			null,
			function() { return car(that).wat_match(e, car(rhs)) },
			function() { return cdr(that).wat_match(e, cdr(rhs)) }
		)
	}
	Nil.prototype.wat_match = function(e, rhs) {
		if (rhs !== NIL) return error("NIL expected, but got: " + to_string(rhs))
	}
	Ign.prototype.wat_match = function(e, rhs) { }
	
	/* Setter - you are not expected to understand this - immediately */
	var SETTER = jswrap(function setter(obj) { return obj.wat_setter })
	SETTER.wat_setter = jswrap(function(new_setter, obj) { obj.wat_setter = new_setter })
	
	/* Error handling */
	var ROOT_PROMPT = {}
	function push_root_prompt(x) { return list(new PushPrompt(), ROOT_PROMPT, x) }
	function error(err) {
		//console.log(err)
		var user_break = the_environment.bindings["user-break"]
		if (user_break === undefined) 
			throw err
		return combine(null, the_environment, user_break, list(err))
	}
	
	/* Utilities */
	function list() {
		return array_to_list(Array.prototype.slice.call(arguments))
	}
	function list_star() {
		var len = arguments.length
		var c = len >= 1 ? arguments[len - 1] : NIL
		for (var i = len - 1; i > 0; i--) c = cons(arguments[i - 1], c)
		return c
	}
	function array_to_list(array, end) {
		var c = end ? end : NIL
		for (var i = array.length; i > 0; i--) c = cons(array[i - 1], c)
		return c
	}
	function list_to_array(c) {
		for (var res = []; c !== NIL; c = cdr(c)) res.push(car(c)); return res
	}
	function reverse_list(list) {
		for (var res = NIL; list !== NIL; list = cdr(list)) res = cons(car(list), res); return res
	}
	
	/* Bytecode parser */
	function parse_bytecode(obj) {
		switch (Object.prototype.toString.call(obj)) {
			case "[object String]": return obj === "#ignore" ? IGN : sym(obj)
			case "[object Array]": return parse_bytecode_array(obj)
			default: return obj
		}
		function parse_bytecode_array(arr) {
			if ((arr.length == 2) && arr[0] === "wat-string") return arr[1]
			var i = arr.indexOf(".")
			if (i === -1) return array_to_list(arr.map(parse_bytecode))
			var front = arr.slice(0, i)
			return array_to_list(front.map(parse_bytecode), parse_bytecode(arr[i + 1]))
		}
	}
	
	/* JSNI */
	var js_types = ["Array", "Boolean", "Date", "Function", "Number", "Object", "RegExp", "String"]
	function is_type(obj, type_obj, type_name) {
		if (!type_obj) return error("type is undefined")
		if (js_types.indexOf(type_name) === -1) return obj instanceof type_obj
		return toString.call(obj) === "[object " + type_name + "]"
	}
	function JSFun(jsfun) {
		if (Object.prototype.toString.call(jsfun) !== "[object Function]") return error("no fun")
		this.jsfun = jsfun
	}
	JSFun.prototype.wat_combine = function(m, e, o) { 
		return this.jsfun.apply(null, list_to_array(o))
	}
	function jswrap(jsfun) { return wrap(new JSFun(jsfun)) }
	function js_unop(op) { return jswrap(new Function("a", "return (" + op + " a)")) }
	function js_binop(op) { return jswrap(new Function("a", "b", "return (a " + op + " b)")) }
	function js_invoker(method_name) {
		return jswrap(
			function() {
				if (arguments.length < 1)
					return error("invoker called with wrong args: " + arguments)
				if (!method_name)
					return error("method name is null/undefined")
				var rcv = arguments[0]
				if (!rcv) 
					return error("receiver is null/undefined")
				var method = rcv[method_name]
				if (!method)
					return error("method not found: " + method_name + " in: " + to_string(rcv))
				return method.apply(rcv, Array.prototype.slice.call(arguments, 1))
			}
		)
	}
	function js_getter(prop_name) {
		var getter = jswrap(
			function() {
				if (arguments.length !== 1) return error(prop_name + " getter called with wrong args")
				var rcv = arguments[0]
				if ((rcv !== undefined) && (rcv !== null)) return rcv[prop_name]
				return error("can't get " + prop_name + " of " + rcv)
			}
		)
		getter.wat_setter = js_setter(prop_name)
		return getter
	}
	function js_setter(prop_name) {
		return jswrap(
			function() {
				if (arguments.length !== 2) return error("setter called with wrong args: " + arguments)
				var rcv = arguments[1]
				if (rcv !== undefined && rcv !== null) return rcv[prop_name] = arguments[0]
				return error("can't set " + prop_name + " of " + rcv)
			}
		)
	}
	function make_prototype(name) {
		var prop_names = Array.prototype.slice.call(arguments, 1)
		var param_names = prop_names.join(",")
		var param_inits = prop_names.map(
				function(prop_name) { return "this." + prop_name + "=" + prop_name + ";" }
			).join("")
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
	var JS_GLOBAL = jswrap(function(name) { return eval(name) })
	JS_GLOBAL.wat_setter = jswrap(function(new_val, name) { global[name] = new_val })
	
	/* Stringification */
	function to_string(obj) {
		if (toString.call(obj) === "[object String]") return JSON.stringify(obj)
		if (obj !== null && obj !== undefined) return obj.toString()
		return Object.prototype.toString.call(obj)
	}
	Nil.prototype.toString = function() { return "()" }
	Ign.prototype.toString = function() { return "#ignore" }
	Sym.prototype.toString = function() { return this.name }
	Cons.prototype.toString = function() { return "(" + cons_to_string(this) + ")"
		function cons_to_string(c) {
			if (cdr(c) === NIL) return to_string(car(c))
			if (cdr(c) instanceof Cons) return to_string(car(c)) + " " + cons_to_string(cdr(c))
			return to_string(car(c)) + " . " + to_string(cdr(c))
		}
	}
	Apv.prototype.toString = function() { return "[Apv " + to_string(this.cmb) + "]" }
	Opv.prototype.toString = function() { return "[Opv " + to_string(this.p) + " " + to_string(this.ep) + " " + to_string(this.x) + "]" }
	Vau.prototype.toString = function() { return "vm-vau" }
	Def.prototype.toString = function() { return "vm-def" }
	Eval.prototype.toString = function() { return "vm-eval" }
	Begin.prototype.toString = function() { return "vm-begin" }
	If.prototype.toString = function() { return "vm-if" }
	Loop.prototype.toString = function() { return "vm-loop" }
	Catch.prototype.toString = function() { return "vm-catch" }
	Finally.prototype.toString = function() { return "vm-finally" }
	DLet.prototype.toString = function() { return "vm-dlet" }
	DNew.prototype.toString = function() { return "vm-dnew" }
	DRef.prototype.toString = function() { return "vm-dref" }
	PushPrompt.prototype.toString = function() { return "vm-push-prompt" }
	TakeSubcont.prototype.toString = function() { return "vm-take-subcont" }
	PushSubcont.prototype.toString = function() { return "vm-push-subcont" }
	PushPromptSubcont.prototype.toString = function() { return "vm-push-prompt-subcont" }
	JSFun.prototype.toString = function() { return "[JSFun " + this.jsfun.toString() + "]" }
	
	/* Bootstrap */
	var the_environment = env()
	bind(the_environment, sym("vm-def"), new Def())
	bind(the_environment, sym("vm-begin"), new Begin())
	var builtin_bytecode =
		["vm-begin",
			// Basics
			["vm-def", "vm-vau", new Vau()],
			["vm-def", "vm-eval", wrap(new Eval())],
			["vm-def", "vm-make-environment", jswrap(function(parent) { return env(parent) })],
			["vm-def", "vm-wrap", jswrap(wrap)],
			["vm-def", "vm-unwrap", jswrap(unwrap)],
			// Values
			["vm-def", "vm-cons", jswrap(cons)],
			["vm-def", "vm-cons?", jswrap(function(obj) { return obj instanceof Cons })],
			["vm-def", "vm-nil?", jswrap(function(obj) { return obj === NIL })],
			["vm-def", "vm-string-to-symbol", jswrap(sym)],
			["vm-def", "vm-symbol?", jswrap(function(obj) { return obj instanceof Sym })],
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
			// Setters
			["vm-def", "vm-setter", SETTER],
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
			// Utilities
			["vm-def", "vm-list-to-array", jswrap(list_to_array)],
			["vm-def", "vm-array-to-list", jswrap(array_to_list)],
			["vm-def", "vm-reverse-list", jswrap(reverse_list)],
			["vm-def", "vm-list*", jswrap(list_star)]
		]
	evaluate(null, the_environment, parse_bytecode(builtin_bytecode))
	
	/* API */
	this.exec = function(bytecode) {
		var wrapped = push_root_prompt(parse_bytecode([new Begin()].concat(bytecode)))
		var res = evaluate(null, the_environment, wrapped)
		if (isSuspension(res)) throw "prompt not found: " + res.prompt
		return res
	}
	this.call = function(fun_name) {
		return this.exec(parse_bytecode([fun_name].concat(Array.prototype.slice.call(arguments, 1))))
	}
	this.get = function(var_name) {
		return this.exec(parse_bytecode(var_name))
	}
}
