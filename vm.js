// Qua VM by Manuel Simoni (msimoni@gmail.com)
module.exports = function Qua() {
	
	var trace = false
	
	/* Continuations */
	function StackFrame(fun, next, dbg, e) { this.fun = fun; this.next = next; this.dbg = dbg; this.e = e }
	StackFrame.prototype.toString = function() { return "[StackFrame " + this.fun + " " + this.dbg + " " + this.e + "]" }

	function Resumption(k, f) { this.k = k; this.f = f }
	Resumption.prototype.toString = function() { return "[Resumption " + this.f + " " + this.k + "]" }
	function isResumption(m) { return m instanceof Resumption }
	function resumeFrame(m) { return m.k.fun(new Resumption(m.k.next, m.f)) }

	function Suspension(prompt, handler) { this.prompt = prompt; this.handler = handler; this.k = null	}
	Suspension.prototype.toString = function() { return "[Suspension " + this.prompt + " " + this.handler + " " + this.k +  "]" }
	function isSuspension(x) { return x instanceof Suspension }
	function suspendFrame(suspension, fun, dbg, e) {
		suspension.k = new StackFrame(fun, suspension.k, dbg, e)
		return suspension
	}
	
	
	/* Forms */
	function Nil() { }
	Nil.prototype.wat_match = function(e, rhs) { if (rhs !== NIL) return "too many arguments" /*+ ", NIL expected, but got: " + to_string(rhs)*/ }
	Nil.prototype.toString = function() { return "()" }
 	var NIL = new Nil()
 	
	function Ign() { }
	Ign.prototype.wat_match = function(e, rhs) { }
	Ign.prototype.toString = function() { return "#ignore" }
	var IGN = new Ign()
	
	
	/* Evaluation Core */
	function evaluate(m, e, x) {
		if (trace) print("eval:", x)
		if (!x || !x.wat_eval) return x
		return x.wat_eval(m, e)
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
			if (c.cdr === NIL) return to_string(c.car)
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
			var s = 'The-Environment'
		else {	
		 	var s=''; for (let n in Object.getOwnPropertyNames(this.bindings)) s+= (!s?'':' ') + n + "=" + this.bindings[n];
		}
		return "[Env" + (!s?'':', '+s) + (!this.parent?'':' '+this.parent.toString()) + "]"
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
		catch (e) { // error in car() or cdr()
			var msg = "insufficient arguments" 
		}
		return !msg ? IGN : error(msg + " in bind: " + lhs + (!exp? '' : " of: " + exp) + " with: " + rhs)
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
		var args = isResumption(m) ? resumeFrame(m) : evalArgs(null, e, o, NIL)
		return isSuspension(args) ? suspendFrame(args, m=> this.wat_combine(m, e, o)) : this.cmb.wat_combine(null, e, args)
	}
	Apv.prototype.toString = function() { return "[Apv " + to_string(this.cmb) + "]" }
	function evalArgs(m, e, todo, done) {
		if (todo === NIL) return reverse_list(done) 
		var arg = isResumption(m) ? resumeFrame(m) : evaluate(null, e, car(todo))
		return isSuspension(arg) ? suspendFrame(arg, m=> evalArgs(m, e, todo, done)) : evalArgs(null, e, cdr(todo), cons(arg, done))
	}
	function wrap(cmb) { return cmb && cmb.wat_combine ? new Apv(cmb) : error("cannot wrap: " + cmb) } // type check
	function unwrap(apv) { return apv instanceof Apv ? apv.cmb : error("cannot unwrap: " + apv) } // type check
	
	
	/* Built-in Combiners */
	function Vau() { }; function Def() { }; function Eval() { }
	Vau.prototype.wat_combine = function(m, e, o) {
		// o = (ptree envp expr)
		if (len(o) > 3) return error("too many operands in: " + cons(this, o));
		var ptree = elt(o, 0)
		var envp = elt(o, 1)
		var msg = pcheck(ptree, envp); if (msg) return error(msg + " of: " + cons(this, o))
		return new Opv(ptree, envp, elt(o, 2), e)
	}
	Vau.prototype.toString = function() { return "vm-vau" }
	Def.prototype.wat_combine = function(m, e, o) { // error handling
		if (len(o) > 2) return error("too many operands in: " + cons(this, o));
		var ptree = elt(o, 0) // almeno un parametro, singolo o nel parameters tree insieme ad #ignore
		if (!(ptree instanceof Sym)) {
			if (!(ptree instanceof Cons)) return error("not a symbol: " + ptree + " in: " + cons(this, o)) 
			var msg = pcheck(ptree); if (msg) return error(msg + " of: " + cons(this, o))
		}
		var rhs = elt(o, 1)
		var val = isResumption(m) ? resumeFrame(m) : evaluate(null, e, rhs)
		return isSuspension(val) ? suspendFrame(val, m=> this.wat_combine(m, e, o)) : bind(e, ptree, val, cons(this, o))
	}
	Def.prototype.toString = function() { return "vm-def" }
	function pcheck(ptree, envp) {
		var symbols = new Set()
		if (ptree != NIL && ptree != IGN) {	var msg = pcheck(ptree); if (msg) return msg }
		if (!envp) return symbols.size > 0 ? null : "no one symbol in: " + ptree
		if (envp == IGN) return null
		if (!(envp instanceof Sym)) return "not a #ignore or symbol: " + envp
		return !symbols.has(envp.name) ? null : "not a unique symbol: " + envp
		function pcheck(p) {
			if (p == IGN) return null
			if (p instanceof Sym) return !symbols.has(p.name) ? (symbols.add(p.name), null) : "not a unique symbol: " + p + (p == ptree ? "" : " in: " + ptree)
			if (!(p instanceof Cons)) return "not a #ignore or symbol: " + p + (p == ptree ? "" : " in: " + ptree) 
			var msg = pcheck(p.car); if (msg) return msg
			return p.cdr == NIL ? null : pcheck(p.cdr)
		}
	}
	Eval.prototype.wat_combine = function(m, e, o) { // error handling
		var x = elt(o, 0)
		var e = elt(o, 1)
		return evaluate(m, e, x)
	}
	Eval.prototype.toString = function() { return "vm-eval" }
	
	
	/* First-order Control */
	function Begin() { }; function If() { }; function Loop() { }; function Catch() { }; function Finally() { }
	Begin.prototype.wat_combine = function(m, e, o) {
		return o === NIL ? null : begin(m, e, o)
		function begin(m, e, xs) {
			var res = isResumption(m) ? resumeFrame(m) : evaluate(null, e, car(xs))
			return isSuspension(res) ? suspendFrame(res, m=> begin(m, e, xs)) : (kdr=> kdr === NIL ? res : begin(null, e, kdr))(cdr(xs))
		}
	}
	Begin.prototype.toString = function() { return "vm-begin" }
	If.prototype.wat_combine = function(m, e, o) {
		if (len(o) > 3) return error("too many operands in: " + cons(this, o));
		var test = isResumption(m) ? resumeFrame(m) : evaluate(null, e, elt(o, 0))
		return isSuspension(test) ? suspendFrame(test, m=> this.wat_combine(m, e, o)) : evaluate(null, e, test ? elt(o, 1) : elt(o, 2))
	}
	If.prototype.toString = function() { return "vm-if" }
	Loop.prototype.wat_combine = function self(m, e, o) {
		var first = true // only resume once
		while (true) {
			var res = first && isResumption(m) ? resumeFrame(m) : evaluate(null, e, elt(o, 0))
			first = false
			if (isSuspension(res)) return suspendFrame(res, m=> self(m, e, o), elt(o, 0), e)
		}
	}
	Loop.prototype.toString = function() { return "vm-loop" }
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
		if (isSuspension(res)) suspendFrame(res, m=> self(m, e, o), x, e)
		return res
	}
	Catch.prototype.toString = function() { return "vm-catch" }
	Finally.prototype.wat_combine = function self(m, e, o) {
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
		var prompt = elt(o, 0)
		var x = elt(o, 1)
		var res = isResumption(m) ? resumeFrame(m) : evaluate(null, e, x)	
		if (!isSuspension(res)) return res
		if (res.prompt !== prompt) return suspendFrame(res, m=> self(m, e, o), x, e)
		var continuation = res.k
		var handler = res.handler
		return combine(null, e, handler, cons(continuation, NIL))
	}
	PushPrompt.prototype.toString = function() { return "vm-push-prompt" }
	TakeSubcont.prototype.wat_combine = function(m, e, o) {
		var prompt = elt(o, 0)
		var handler = elt(o, 1)
		var cap = new Suspension(prompt, handler)
		return suspendFrame(cap, m=> combine(null, e, m.f, NIL), this, e)
	}
	TakeSubcont.prototype.toString = function() { return "vm-take-subcont" }
	PushSubcont.prototype.wat_combine = function self(m, e, o) {
		var thek = elt(o, 0)
		var thef = elt(o, 1)
		var res = isResumption(m) ? resumeFrame(m) : resumeFrame(new Resumption(thek, thef))
		if (isSuspension(res)) suspendFrame(res, m=> self(m, e, o), thef, e)
		return res
	}
	PushSubcont.prototype.toString = function() { return "vm-push-subcont" }
	PushPromptSubcont.prototype.wat_combine = function self(m, e, o) {
		var prompt = elt(o, 0)
		var thek = elt(o, 1)
		var thef = elt(o, 2)
		var res = isResumption(m) ? resumeFrame(m) : resumeFrame(new Resumption(thek, thef))
		if (!isSuspension(res)) return res
		if (res.prompt !== prompt) return suspendFrame(res, m=> self(m, e, o), thef, e)
		var continuation = res.k
		var handler = res.handler
		return combine(null, e, handler, cons(continuation, NIL))
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
	function push_root_prompt(x) { return list(new PushPrompt(), ROOT_PROMPT, x) }
	function error(err) {
		//console.log(err)
		var user_break = the_environment.bindings["user-break"]
		if (user_break === undefined) throw err
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
		var c = b || len < 0 ? NIL : args[len]
		for (var i=len-(b?0:1); i>=0; i-=1) c = cons(args[i], c)
		return c
	}
	function list_to_array(c) {
		for (var res = []; c !== NIL; c = cdr(c)) res.push(car(c)); return res
	}
	function reverse_list(list) {
		for (var res = NIL; list !== NIL; list = cdr(list)) res = cons(car(list), res); return res
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
			if (arguments.length == 1) print(a, "should be throw but is", v)
			else if (!eq(v, b)) print(a, "should be", b, "but is", v);
		}
		catch (t) {
			if (arguments.length > 1) print(a, "throw", t);
		}
	}
	
	
	/* Bytecode parser */
	function parse_bytecode(obj) {
		switch (Object.prototype.toString.call(obj)) {
			case "[object String]": return obj === "#ignore" ? IGN : sym(obj)
			case "[object Array]": return parse_bytecode_array(obj)
			default: return obj
		}
		function parse_bytecode_array(arr) {
			if (arr.length == 0) return NIL
			if (arr.length == 2 && arr[0] === "wat-string") return arr[1]
			var head = cons(parse_bytecode(arr[0]), NIL), c = head
			for (var i=1; i<arr.length; i+=1) {
				if (arr[i] !== ".") { c = c.cdr = cons(parse_bytecode(arr[i]), NIL); continue }
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
			["vm-def", "vm-nil?", jswrap(obj=> obj === NIL)],
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
