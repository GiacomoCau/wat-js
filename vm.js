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
	k, next: stackframe
	exc: exception
	id: identifier
	num: number
	str: string
	stx: syntax
	sym: symbol
	cmt: comment
	dbg: debugging information
*/

export function Qua() {
	
	var trace = false
	var stack = false
	var thenv = false
	
	// Continuations
	function StackFrame(f, next, dbg, e) { this.f = f; this.next = next; this.dbg = dbg; this.e = e }
	StackFrame.prototype.toString = function() { return "[StackFrame " + this.f + " " + this.dbg + " " + this.e + "]" }

	function Resumption(k, s) { this.k = k; this.s = s }
	Resumption.prototype.toString = function() { return "[Resumption " + this.s + " " + this.k + "]" }
	function resumeFrame(r) { return r.k.f(new Resumption(r.k.next, r.s)) }

	function Suspension(prompt, handler) { this.prompt = prompt; this.handler = handler; this.k = null	}
	Suspension.prototype.toString = function() { return "[Suspension " + this.prompt + " " + this.handler + " " + this.k +  "]" }
	function isSuspension(x) { return x instanceof Suspension }
	function suspendFrame(suspension, f, dbg, e) {
		return suspension.k = new StackFrame(f, suspension.k, dbg, e), suspension
	}
	
	
	// Forms
	function Inert() {}
	Inert.prototype.toString = function() { return "#inert" }
	var inert = new Inert()
	
	function Nil() { }
	Nil.prototype.bind = function(e, rhs) { if (rhs !== nil) return "too many arguments" /*+ ", nil expected, but got: " + toString(rhs)*/ }
	Nil.prototype.toString = function() { return "()" }
 	var nil = new Nil()
 	
	function Ignore() { }
	Ignore.prototype.bind = function(e, rhs) { return null }
	Ignore.prototype.toString = function() { return "#ignore" }
	var ignore = new Ignore()
	
	
	// Evaluation Core
	function evaluate(r, e, x) {
		if (trace) print("evaluate:", x)
		return x && x.eval ? x.eval(r, e) : x
	}
	
	function Sym(name) { this.name = name }
	Sym.prototype.eval = function(r, e) { return lookup(e, this.name) }
	Sym.prototype.bind = function(e, rhs) { if (trace) print("    bind:", this.name+"="+rhs, e); e.bindings[this.name] = rhs; }
	Sym.prototype.toString = function() { return this.name }
	function sym(name) { return new Sym(name) }

	function Cons(car, cdr) { this.car = car; this.cdr = cdr }
	Cons.prototype.eval = function(r, e) {
		var op = r ? resumeFrame(r) : evaluate(null, e, this.car)
		return isSuspension(op) ? suspendFrame(op, r=> this.eval(r, e)) : combine(null, e, op, this.cdr)
	}
	Cons.prototype.bind = function(e, rhs) {
		if (!this.car.bind) return "cannot match against: " + this.car
		if (!this.cdr.bind) return "cannot match against: " + this.cdr 
		this.car.bind(e, car(rhs)); return this.cdr.bind(e, cdr(rhs));
	}
	Cons.prototype.toString = function() {
		return "(" + consToString(this) + ")"
		function consToString(c) {
			if (c.cdr === nil) return toString(c.car)
			if (c.cdr instanceof Cons) return toString(c.car) + " " + consToString(c.cdr)
			return toString(c.car) + " . " + toString(c.cdr)
		}
	}
	function cons(car, cdr) { return new Cons(car, cdr) }
	function car(cons) { // tc
		return cons instanceof Cons ? cons.car : error("not a cons: " + toString(cons))
	}
	function cdr(cons) { // tc
		return cons instanceof Cons ? cons.cdr : error("not a cons: " + toString(cons))
	}
	function elt(o, i) { for (; i>0; i-=1) o=cdr(o); return car(o); }
	function len(o) { for (var i=0; o instanceof Cons; o=cdr(o)) i+=1; return i; }
	
	
	// Environment
	function Env(parent) { this.bindings = Object.create(!parent ? null : parent.bindings); this.parent = parent }
	Env.prototype.toString = function() {
		var isThenv = this == theEnvironment;
		if (!isThenv || thenv) {	
		 	var s=''; for (let n in this.bindings) if (Object.hasOwn(this.bindings, n)) s= n + "=" + this.bindings[n] + (!s ? "" : ", " + s);
		}
		return "[" + (!isThenv ? "" : "The-") + "Env" + (!s ? "" : " " + s) + (!this.parent ? "" : " " + parent) + "]"
	}
	function env(parent) { return new Env(parent) }
	function lookup(e, name) {
		if (!(name in e.bindings)) error("unbound: " + name)
		if (trace) print("  lookup:", name)
		return e.bindings[name]
	}
	
	
	// Bind
	function bind(e, lhs, rhs, exp) {
		if (!lhs.bind) return error("cannot match against: " + lhs)
		try {
			var msg = lhs.bind(e, rhs);
		}
		catch (exc) { // only error in car() or cdr()
			var msg = "too few arguments" // + " because " + exc.getMessage()
		}
		return !msg ? inert : error(msg + " in bind: " + lhs + (!exp ? "" : " of: " + exp) + " with: " + rhs)
	}
	
	
	// Operative & Applicative Combiners
	function combine(r, e, cmb, o) {
		if (trace) print(" combine:", cmb, o)
		if (cmb && cmb.combine)	return cmb.combine(r, e, o)
		// TODO per default le Function non wrapped dovrebbero essere operative e non applicative
		if (cmb instanceof Function)
			return jsWrap(cmb).combine(r, e, o) // Function x default applicative
			//	 return Apv.prototype.combine.call(new JsFun(cmb), r, e, o) 
			//   return new JsFun(cmb).combine(r, e, o) // Function x default operative
			//   return jsFun(cmb).combine(r, e, o)
			//   return cmb.apply(null, listToArray(o))
		return error("not a combiner: " + toString(cmb) + " in: " + cons(cmb, o))
	}
	
	function Opv(p, ep, x, e) { this.p = p; this.ep = ep; this.x = x; this.e = e }
	Opv.prototype.combine = function(r, e, o) {
		var xe = env(this.e); bind(xe, this.p, o, this); bind(xe, this.ep, e, this); return evaluate(null, xe, this.x)
	}
	Opv.prototype.toString = function() { return "[Opv " + toString(this.p) + " " + toString(this.ep) + " " + toString(this.x) + "]" }

	function Apv(cmb) { this.cmb = cmb }
	Apv.prototype.combine = function(r, e, o) {
		var args = r ? resumeFrame(r) : evalArgs(null, e, o, nil)
		return isSuspension(args) ? suspendFrame(args, r=> this.combine(r, e, o)) : this.cmb.combine(null, e, args)
	}
	function evalArgs(r, e, todo, done) {
		if (todo === nil) return reverseList(done) 
		var arg = r ? resumeFrame(r) : evaluate(null, e, car(todo))
		return isSuspension(arg) ? suspendFrame(arg, r=> evalArgs(r, e, todo, done)) : evalArgs(null, e, cdr(todo), cons(arg, done))
	}
	Apv.prototype.toString = function() { return "[Apv " + toString(this.cmb) + "]" }
	function wrap(cmb) { return cmb && cmb.combine ? new Apv(cmb) : error("cannot wrap: " + cmb) } // type check
	function unwrap(apv) { return apv instanceof Apv ? apv.cmb : error("cannot unwrap: " + apv) } // type check
	
	
	// Built-in Combiners
	function Vau() { }; function Def() { }; function Eval() { }
	Vau.prototype.combine = function(r, e, o) {
		// o = (pt ep expr)
		if (len(o) > 3) return error("too many operands in: " + cons(this, o));
		var pt = elt(o, 0)
		var ep = elt(o, 1)
		var msg = checkPt(pt, ep); if (msg) return error(msg + " of: " + cons(this, o))
		return new Opv(pt, ep, elt(o, 2), e)
	}
	Vau.prototype.toString = function() { return "%vau" }
	Def.prototype.combine = function(r, e, o) { // error handling
		// o = (pt arg)
		if (len(o) > 2) return error("too many operands in: " + cons(this, o));
		var pt = elt(o, 0) // almeno un parametro, singolo o nel parameters tree insieme ad #ignore
		if (!(pt instanceof Sym)) {
			if (!(pt instanceof Cons)) return error("not a symbol: " + pt + " in: " + cons(this, o)) 
			var msg = checkPt(pt); if (msg) return error(msg + " of: " + cons(this, o))
		}
		var arg = elt(o, 1)
		var val = r ? resumeFrame(r) : evaluate(null, e, arg)
		return isSuspension(val) ? suspendFrame(val, r=> this.combine(r, e, o)) : bind(e, pt, val, cons(this, o))
	}
	Def.prototype.toString = function() { return "%def" }
	Eval.prototype.combine = function(r, e, o) { // error handling
		// o = (x eo)
		var x = elt(o, 0)
		var eo = elt(o, 1)
		return evaluate(r, eo, x)
	}
	Eval.prototype.toString = function() { return "%eval" }
	
	
	// First-order Control
	function Begin() { }; function If() { }; function Loop() { }; function Catch() { }; function Finally() { }
	Begin.prototype.combine = function(r, e, o) {
		// o = (.. xs)
		return o === nil ? null : begin(r, e, o)
		function begin(r, e, xs) {
			var res = r ? resumeFrame(r) : evaluate(null, e, car(xs))
			return isSuspension(res) ? suspendFrame(res, r=> begin(r, e, xs)) : (kdr=> kdr === nil ? res : begin(null, e, kdr))(cdr(xs))
		}
	}
	Begin.prototype.toString = function() { return "%begin" }
	If.prototype.combine = function(r, e, o) {
		// o = (test then else) 
		if (len(o) > 3) return error("too many operands in: " + cons(this, o));
		var test = r ? resumeFrame(r) : evaluate(null, e, elt(o, 0))
		return isSuspension(test) ? suspendFrame(test, r=> this.combine(r, e, o)) : evaluate(null, e, test ? elt(o, 1) : elt(o, 2))
	}
	If.prototype.toString = function() { return "%if" }
	Loop.prototype.combine = function self(r, e, o) {
		// o = (x)
		var first = true // only resume once
		while (true) {
			var res = first && r ? resumeFrame(r) : evaluate(null, e, elt(o, 0))
			first = false
			if (isSuspension(res)) return suspendFrame(res, r=> self(r, e, o), elt(o, 0), e)
		}
	}
	Loop.prototype.toString = function() { return "%loop" }
	Catch.prototype.combine = function self(r, e, o) {
		// o = (x handler)
		var x = elt(o, 0)
		var handler = elt(o, 1)
		try {
			var res = r ? resumeFrame(r) : evaluate(null, e, x)
		}
		catch (exc) {
			// unwrap handler to prevent eval if exc is sym or cons
			var res = combine(null, e, unwrap(handler), list(exc))
		}
		if (isSuspension(res)) suspendFrame(res, r=> self(r, e, o), x, e)
		return res
	}
	Catch.prototype.toString = function() { return "%catch" }
	Finally.prototype.combine = function self(r, e, o) {
		// o = (prot cleanup)
		var prot = elt(o, 0)
		var cleanup = elt(o, 1)
		try {
			var res = r ? resumeFrame(r) : evaluate(null, e, prot)
			if (isSuspension(res)) suspendFrame(res, r=> self(r, e, o), prot, e)
		}
		finally {
			return isSuspension(res) ? res : doCleanup(null, e, cleanup, res)
		}
		function doCleanup(r, e, cleanup, res) {
			var fres = r ? resumeFrame(r) : evaluate(null, e, cleanup)
			if (isSuspension(fres)) suspendFrame(fres, r=> doCleanup(r, e, cleanup, res), cleanup, e)
			return fres
		}
	}
	Finally.prototype.toString = function() { return "%finally" }
	
	
	// Delimited Control
	function PushPrompt() { }; function TakeSubcont() {	}; function PushSubcont() { }; function PushPromptSubcont() { }
	PushPrompt.prototype.combine = function self(r, e, o) {
		// o = (prompt exp)
		if (len(o) > 2) return error("too many operands in: " + cons(this, o));
		var prompt = elt(o, 0)
		var x = elt(o, 1)
		var res = r ? resumeFrame(r) : evaluate(null, e, x)	
		if (!isSuspension(res)) return res
		if (res.prompt !== prompt) return suspendFrame(res, r=> self(r, e, o), x, e)
		return combine(null, e, res.handler, cons(res.k, nil))
	}
	PushPrompt.prototype.toString = function() { return "%push-prompt" }
	TakeSubcont.prototype.combine = function(r, e, o) {
		// o = (prompt handler)
		if (len(o) > 2) return error("too many operands in: " + cons(this, o));
		var prompt = elt(o, 0)
		var handler = elt(o, 1)
		if (!(handler instanceof Apv)) return error("not a one applicative combiner: " + handler)
		var cap = new Suspension(prompt, handler)
		return suspendFrame(cap, r=> combine(null, e, r.s, nil), this, e)
	}
	TakeSubcont.prototype.toString = function() { return "%take-subcont" }
	PushSubcont.prototype.combine = function self(r, e, o) {
		// o = (k apv0)
		if (len(o) > 2) return error("too many operands in: " + cons(this, o));
		var k = elt(o, 0)
		if (!(k instanceof StackFrame)) return error("not a stackframe: " + k);
		var apv0 = elt(o, 1)
		if (!(apv0 instanceof Apv)) return error("not a zero args applicative combiner: " + apv0);
		var res = r ? resumeFrame(r) : resumeFrame(new Resumption(k, apv0))
		if (isSuspension(res)) suspendFrame(res, r=> self(r, e, o), apv0, e)
		return res
	}
	PushSubcont.prototype.toString = function() { return "%push-subcont" }
	PushPromptSubcont.prototype.combine = function self(r, e, o) {
		// o = (prompt k apv0)
		if (len(o) > 2) return error("too many operands in: " + cons(this, o));
		var prompt = elt(o, 0)
		var k = elt(o, 1)
		if (!(o1 instanceof StackFrame)) return error("not a stackframe: " + o1); 
		var apv0 = elt(o, 2)
		if (!(o2 instanceof Apv)) return error("not a zero args applicative combiner: " + o2); 
		var res = r ? resumeFrame(r) : resumeFrame(new Resumption(k, apv0))
		if (!isSuspension(res)) return res
		if (res.prompt !== prompt) return suspendFrame(res, r=> self(r, e, o), apv0, e)
		return combine(null, e, res.handler, cons(res.k, nil))
	}
	PushPromptSubcont.prototype.toString = function() { return "%push-prompt-subcont" }
	
	
	// Dynamic Variables
	function DV(val) { this.val = val }; function DNew() { }; function DRef() { }; function DLet() { }
	DNew.prototype.combine = function(r, e, o) { return new DV(elt(o, 0)) }
	DNew.prototype.toString = function() { return "%dnew" }
	DRef.prototype.combine = function(r, e, o) { return elt(o, 0).val }
	DRef.prototype.toString = function() { return "%dref" }
	DLet.prototype.combine = function self(r, e, o) {
		var dv = elt(o, 0)
		var val = elt(o, 1)
		var x = elt(o, 2)
		var oldVal = dv.val
		try {
			dv.val = val
			var res = r ? resumeFrame(r) : evaluate(null, e, x)
			if (isSuspension(res)) suspendFrame(res, r=> self(r, e, o), x, e)
			return res
		}
		finally {
			dv.val = oldVal
		}
	}
	DLet.prototype.toString = function() { return "%dlet" }
	
	
	// Error handling
	var rootPrompt = {}
	rootPrompt.toString = function() { return "rootPrompt" }
	function pushRootPrompt(x) { return list(new PushPrompt(), rootPrompt, x) }
	function error(err) {
		var userBreak = theEnvironment.bindings["user-break"]
		if (!userBreak) throw err
		return combine(null, theEnvironment, userBreak, list(err))
	}
	function checkPt(pt, ep) {
		var symbols = new Set()
		if (pt != nil && pt != ignore) { var msg = checkPt(pt); if (msg) return msg }
		if (ep === undefined) return symbols.size > 0 ? null : "no one symbol in: " + pt
		if (ep === ignore) return null
		if (!(ep instanceof Sym)) return "not a #ignore or symbol: " + ep
		return !symbols.has(ep.name) ? null : "not a unique symbol: " + ep
		function checkPt(p) {
			if (p === ignore) return null
			if (p instanceof Sym) return !symbols.has(p.name) ? (symbols.add(p.name), null) : "not a unique symbol: " + p + (p == pt ? "" : " in: " + pt)
			if (!(p instanceof Cons)) return "not a #ignore or symbol: " + p + (p == pt ? "" : " in: " + pt) 
			var msg = checkPt(p.car); if (msg) return msg
			return p.cdr == nil ? null : checkPt(p.cdr)
		}
	}
	/*
	function args(apv) {
		switch(apv.cmb) {
			case Opv opv-> opv.p == nil ? 0 : opv.p instanceof Cons c && c.cdr == nil && (c.car == ignore || c.car instanceof Sym) ? 1 : Integer.MAX_VALUE
			case JsFun jsFun-> jsFun.jfun instanceof Supplier ? 0 : jFun.jfun instanceof Function ? 1 : Integer.MAX_VALUE
			default-> Integer.MAX_VALUE
		};
	}
	*/
	function checkO(op, o, min, max) {
		var len = len(o)
		if (!(max instanceof Number)) {
			if (len == min) return checkO(3, arguments), le
			return error("not " + expt + " operands for combine: " + op + " with: " + o)
		}
		else {
			if (len >= min && (max == -1 || len <= max)) return checkO(4, arguments), len 
			return error((len < min ? "less then " + min : max == -1 ? "" : " or more then " + max) + " operands for combine: " + op + " with: " + o)
		}
		function checkO(i, args) {
			if (args.length == 0) return 0
			for (var ii=i, oo=o; i<args.length && o instanceof Cons; i+=1, o=o.cdr) {
				var o0 = o.car; var cl=args[i]; if (cl || cl.prototype.isPrototypeOf(o0)) continue;
				return error("not a " + toString(args[i]) + ": " + o0 + " for combine: " + op + " with: " + oo)
			}
			return i-ii;
		}
	}
	
	
	// Utilities
	function list() {
		return arrayToList(true, arguments)
	}
	function listStar() {
		return arrayToList(false, arguments)
	}
	function arrayToList(b, args) {
		var len = args.length-1
		var c = b || len < 0 ? nil : args[len]
		for (var i=len-(b?0:1); i>=0; i-=1) c = cons(args[i], c)
		return c
	}
	function listToArray(c) {
		for (var res = []; c !== nil; c = cdr(c)) res.push(car(c)); return res
	}
	function reverseList(list) {
		for (var res = nil; list !== nil; list = cdr(list)) res = cons(car(list), res); return res
	}
	function print() {
		console.log(toString(... arguments))
		return arguments[arguments.length - 1]
	}
	function eq(a, b) {
		if (a instanceof Cons && b instanceof Cons) return eq(a.car, b.car) && eq(a.cdr, b.cdr)
		if (a instanceof Sym && b instanceof Sym) return a.name === b.name
		return a === b 
	}
	function assert(a, b) {
		try {
			var v = evaluate(null, env(theEnvironment), a)
			if (arguments.length == 1)
				print(a, "should throw but is", v)
			else if (!eq(v, b))
				print(a, "should be", b, "but is", v);
		}
		catch (t) {
			if (arguments.length > 1) print(a, "throw", t);
		}
	}
	
	
	// Bytecode parser
	function parseBytecode(obj) {
		switch (Object.prototype.toString.call(obj)) {
			case "[object String]": return obj === "#inert" ? inert : obj === "#ignore" ? ignore : sym(obj)
			case "[object Array]": return parseBytecodeArray(obj)
			default: return obj
		}
		function parseBytecodeArray(arr) {
			if (arr.length == 0) return nil
			if (arr.length == 2 && arr[0] === "wat-string") return arr[1]
			var head = cons(parseBytecode(arr[0]), nil), c = head
			for (var i=1; i<arr.length; i+=1) {
				if (arr[i] !== ".") { c = c.cdr = cons(parseBytecode(arr[i]), nil); continue }
				if (i != arr.length-2) throw error(". not is the penultimate element in " + arr)
				c.cdr = parseBytecode(arr[i+1])
				return head
			}
			return head
		}
	}
	
	
	// JSNI
	function JsFun(fun) { this.fun = fun }
	JsFun.prototype.combine = function(r, e, o) { return this.fun.apply(null, listToArray(o))	}
	JsFun.prototype.toString = function() { return "[JsFun " + this.fun + "]" }
	function jsFun(fun) { return Object.prototype.toString.call(fun) === "[object Function]" ? new JsFun(fun) : error("no fun") }
	function jsWrap(fun) { return wrap(jsFun(fun)) }
	
	var jsTypes = ["Array", "Boolean", "Date", "Function", "Number", "Object", "RegExp", "String"]
	function isType(obj, typeObj, typeName) {
		if (!typeObj) return error("type is undefined")
		if (jsTypes.indexOf(typeName) === -1) return obj instanceof typeObj
		return toString.call(obj) === "[object " + typeName + "]"
	}
	function jsUnop(op) { return jsWrap(new Function("a", "return (" + op + " a)")) }
	function jsBinop(op) { return jsWrap(new Function("a", "b", "return (a " + op + " b)")) }
	function jsInvoker(methodName) {
		if (!methodName) return error("method name is null/undefined")
		return jsWrap(
			function(rcv) {
				if (!rcv) return error("receiver is null/undefined")
				if (arguments.length < 1) return error("invoker called with wrong args: " + arguments)
				var method = rcv[methodName]
				if (!method) return error("method not found: " + methodName + " in: " + toString(rcv))
				return method.apply(rcv, Array.prototype.slice.call(arguments, 1))
			}
		)
	}
	function jsGetter(propName) {
		var getter = jsWrap(
			function(rcv) {
				if (arguments.length !== 1) return error(propName + " getter called with wrong args")
				if (rcv !== undefined && rcv !== null) return rcv[propName]
				return error("can't get " + propName + " of " + rcv)
			}
		)
		getter.setter = jsSetter(propName)
		return getter
	}
	function jsSetter(propName) {
		return jsWrap(
			function(val, rcv) {
				if (arguments.length !== 2) return error("setter called with wrong args: " + arguments)
				if (rcv !== undefined && rcv !== null) return rcv[propName] = val
				return error("can't set " + propName + " of " + rcv)
			}
		)
	}
	function makePrototype(name) {
		var propNames = Array.prototype.slice.call(arguments, 1)
		var paramNames = propNames.join(",")
		var paramInits = propNames.map(propName=> "this." + propName + "=" + propName + ";").join("")
		return eval("(function " + name + "(" + paramNames + "){" + paramInits + "})")
	}
	function jsNew(ctor) {
		var factoryFunction = ctor.bind.apply(ctor, arguments)
		return new factoryFunction()
	}
	function jsFunction(cmb) {
		return function() {
			var args = cons(this, arrayToList(Array.prototype.slice.call(arguments)))
			return combine(null, null, cmb, args)
		}
	}
	var jsGlobal = jsWrap(name=> globalThis[name])
	jsGlobal.setter = jsWrap((newVal, name)=> globalThis[name] = newVal)
	
	
	// Setter - you are not expected to understand this - immediately
	var SETTER = jsWrap(obj=> obj.setter)
	SETTER.setter = jsWrap((newSetter, obj)=> obj.setter = newSetter)
	
	
	// Stringification
	function toString() {
		var s=''; for (let arg of arguments) s += (!s?'':' ') + toString(arg); return s
		function toString(obj) {
			if (obj === null) return "#null"
			if (obj === undefined) return "#undefined"
			if (Object.prototype.toString.call(obj) === "[object String]") return obj //JSON.stringify(obj) // con o senza apici?
			return obj.toString() // Object.prototype.toString.call(obj)
		}
	}
	
	
	// Bootstrap
	var theEnvironment = env()
	bind(theEnvironment, sym("%def"), new Def())
	bind(theEnvironment, sym("%begin"), new Begin())
	evaluate(null, theEnvironment,
		parseBytecode(
			["%begin",
				// Basics
				["%def", "%vau", new Vau()],
				["%def", "%eval", wrap(new Eval())],
				["%def", "%make-environment", jsWrap(parent=> env(parent))],
				["%def", "%wrap", jsWrap(wrap)],
				["%def", "%unwrap", jsWrap(unwrap)],
				// Values
				["%def", "%cons", jsWrap(cons)],
				["%def", "%cons?", jsWrap(obj=> obj instanceof Cons)],
				["%def", "%nil?", jsWrap(obj=> obj === nil)],
				["%def", "%string-to-symbol", jsWrap(sym)],
				["%def", "%symbol?", jsWrap(obj=> obj instanceof Sym)],
				["%def", "%symbol-name", jsWrap(sym=> sym.name)],
				// First-order Control
				["%def", "%if", new If()],
				["%def", "%loop", new Loop()],
				["%def", "%throw", jsWrap(err=> { throw err })],
				["%def", "%catch", new Catch()],
				["%def", "%finally", new Finally()],
				// Delimited Control
				["%def", "%push-prompt", new PushPrompt()],
				["%def", "%take-subcont", wrap(new TakeSubcont())],
				["%def", "%push-subcont", wrap(new PushSubcont())],
				["%def", "%push-prompt-subcont", wrap(new PushPromptSubcont())],
				// Dynamically-scoped Variables
				["%def", "%dnew", wrap(new DNew())],
				["%def", "%dlet", new DLet()],
				["%def", "%dref", wrap(new DRef())],
				// Errors
				["%def", "%root-prompt", rootPrompt],
				["%def", "%error", jsWrap(error)],
				// JS Interface
				["%def", "%js-wrap", jsWrap(jsWrap)],
				["%def", "%js-unop", jsWrap(jsUnop)],
				["%def", "%js-binop", jsWrap(jsBinop)],
				["%def", "%js-getter", jsWrap(jsGetter)],
				["%def", "%js-setter", jsWrap(jsSetter)],
				["%def", "%js-invoker", jsWrap(jsInvoker)],
				["%def", "%js-function", jsWrap(jsFunction)],
				["%def", "%js-global", jsGlobal],
				["%def", "%js-make-object", jsWrap(function() { return {} })],
				["%def", "%js-make-prototype", jsWrap(makePrototype)],
				["%def", "%js-new", jsWrap(jsNew)],
				["%def", "%type?", jsWrap(isType)],
				// Setters
				["%def", "%setter", SETTER],
				// Utilities
				["%def", "%list", jsWrap(list)],
				["%def", "%list*", jsWrap(listStar)],
				["%def", "%list-to-array", jsWrap(listToArray)],
				["%def", "%array-to-list", jsWrap(arrayToList)],
				["%def", "%reverse-list", jsWrap(reverseList)],
				["%def", "!", jsWrap(a=> !a)],
				["%def", "==", jsWrap((a,b)=> a == b)],
				["%def", "eq", jsWrap((a,b)=> eq(a,b))],
				["%def", "to-string", jsWrap(toString)],
				["%def", "assert", jsFun(assert)],
				["%def", "trace", jsFun(b=> b === undefined ? trace : trace=b)],
				["%def", "stack", jsFun(b=> b === undefined ? stack : stack=b)],
				["%def", "thenv", jsFun(b=> b === undefined ? thenv : thenv=b)],
			]
		)
	)
	
	
	// API
	this.exec = function(bytecode) {
		var wrapped = pushRootPrompt(cons(new Begin(), parseBytecode(bytecode)))
		var res = evaluate(null, theEnvironment, wrapped)
		if (isSuspension(res)) throw "prompt not found: " + res.prompt
		return res
	}
	this.call = function(funName) {
		return this.exec(list(sym(funName), parseBytecode(Array.prototype.slice.call(arguments, 1))))
	}
	this.get = function(varName) {
		return this.exec(sym(varName))
	}
}
