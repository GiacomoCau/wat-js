
// ok with node 13.14 --harmony-top-level-await or greater
import { Vm, readFile } from './wat.js' // con package.json: type=module 
var vm = Vm()
console.log(
	//vm.parse_bytecode(
		//["params","env-param",".","body"] // ok
		//["params","env-param",".","()"] // ok
	vm.eval(
		readFile("./test.wat")
		//`( ($lambda ((a . b) . c) b) '((1 . 2) . 3) )` 
		//`($define ((a . b) . c) '((1 . 2) . 3) ) b` 
		//'(log "logging" 1 2 3)'
		//&x
		//"(* 2 2)(+ 1 2)"
		//require("fs").readFileSync("test.wat", "utf8")
		//((lambda (x y) x) 1 2)
		//"(cons ((lambda (x y) x) 1 2) ((lambda (x y) y) 1 2))"
		//"(list ((lambda (x y) x) 1 2) ((lambda (x y) y) 1 2))"
		//"(begin 1 2)" // 2
		//"1 2" // 2
		//"1 2"
		//";\n1"
		//"a"
		///"print-stacktrace"
		//"@toString"
		//".dbg"
		//"(@log &console \"funzica\" \"?\")"
		//"(log \"funzica\" \"?\")"
		//"(take-subcont %root-prompt k (%type? k &StackFrame))"
		//"(nil? (cdr '(a)))"
		//`(let ( ((a) (1)) ) a)`
		//"(cadr '((1 . 2) (3 . 4)))"
		//"(if #t #undefined #t)"
		//"'(sequence #undefined)"
		//"(cond (#t #t))"
		//"(trace #t)(define x 1)"
		/*
		`
		(define x 1)
		(define exports (list x))
		;(log (eval (list* module (list* list x) exports) (the-environment))) ; va in errore not a symbol: [Apv [Opv elts #ignore elts]] in: ([Apv [Opv elts #ignore elts]] 1)
		;(log (eval (list* module (list list x) exports) (get-current-environment))) ; va in errore idem come sopra
		;(log (eval '(module (x) exports) (the-environment))) ; torna un Env con x=1, forse è ok!
		;(log (eval (list* module (list 'x) exports) (the-environment))) ; torna un Env con x=1, idem come sopra
		;(log (eval (list* module '(x) exports) (the-environment))) ; idem come sopra
		(log (eval (list* module '(list list x) exports) (get-current-environment))) ; idem come sopra
		;(log (eval (list* module (list* list x) exports) (get-current-environment))) ; idem come sopra

		 `
		//*/
		//`(provide (n) ($define! n 0))` // ?
		//"(%vau (a . 12) #ignore 0)"
		//"($vau (a . 12) #ignore 0)"
		//"($lambda (a b . 12) 1)"
		//"(lambda (a b . 12) 1)"
		//"((lambda (a b . c) c) 1 2 3 4) "
		//"(%def (a b 12) 1)"
		//"(@toString ((%vau (a . b) #ignore (list* a a a b)) 1 2 3))"
		//"((lambda #ignore 1) 2 3 4)"
		//"(&setTimeout (.log &console) 0 \"funzica\")"
		//"(%wrap 1)"
		//"(apply (wrap 2) '(1) #ignore)"
		//"(js-global \"console\")"
		//"(js-global \"Math\")"
		//"(js-global \"process\")"
		//"'(a b . ())" // ok
		//"'(a b . c)" // ok
		//"(@toString '(a b . ()))"
		//"(@toString '(a b . c))"
		//"(@toString '(a b . (c)))"
		//"($vau a a a)"
		//"($vau (b . a) a a)"
		/*
		`	
			($define! log (wrap ($vau (msg) #ignore (@log &console msg))))
			(log "verifica define formals")
				(assert ($define! 1 2)         throw)
				(assert ($define! "" 2)        throw)
				(assert ($define! () 2)        throw)
				(assert ($define! #ignore 2)   throw)
				(assert ($define! (1) 2)       throw)
				(assert ($define! ("") 2)      throw)
				(assert ($define! (() a) 2)    throw)
				(assert ($define! (#ignore) 2) throw)
				(assert ($define! (a a) 1)     throw)			
				(assert ($define! (a . a) 1)   throw)			
			(log "\nverifica define arguments (master)")
				(assert ($define! a 1 2)       throw)
				(assert ($define! (a) 1)       throw)
				(assert ($define! (a) '(1 2))  throw)
				(assert ($define! ((a)) '(1))  throw)
			(log "\nverifica define arguments (define)")						
				(assert ($define! (a) 1 2)     throw)
				(assert ($define! (a b) 1)     throw)
				(assert ($define! ((a)) '(1))  throw)
			(log "\nverifica vau formals")
				(assert ($vau 1 #ignore a)      throw)
				(assert ($vau (1) #ignore a)    throw)
				(assert ($vau (()) #ignore a)   throw)
				(assert ($vau (a 1) #ignore a)  throw)
				(assert ($vau (a ()) #ignore a) throw)
				(assert ($vau (a a) #ignore a)  throw)
				(assert ($vau (a . a) #ignore a) throw)
			(log "\nverifica vau eformal")
				(assert ($vau a 1 a)  throw)
				(assert ($vau a () a) throw)
				(assert ($vau a a a) throw)
			(log "\nverifica vau arguments")
				(assert (($vau (a b) e a) 1) throw)
				(assert (($vau (a) e a) 1 2) throw)
			(log "\nverifica define formals return")
				(assert ($define! a 1)          throw)
				(assert ($define! (a) '(1))     throw)
				(assert ($define! (a . b) '(1)) throw)		
		`
		//*/
		//"(to-string (($vau (x 1) #ignore 1 2 3 4 5 x) 6))"
		//"(assert (%def a))"
		//"\"ok\""
	)
);
console.log("finito")