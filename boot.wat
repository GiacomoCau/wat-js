;; -*- mode: Scheme -*-

;; ``72. An adequate bootstrap is a contradiction in terms.''

;; Rename ur-def
(vm-def $define! vm-def)

;; Rename bindings that will be used as provided by VM
($define! array->list vm-array-to-list)
($define! begin vm-begin)
($define! cons vm-cons)
($define! cons? vm-cons?)
($define! dnew vm-dnew)
($define! dref vm-dref)
($define! error vm-error)
($define! eval vm-eval)
($define! if vm-if)
($define! js-getter vm-js-getter)
($define! js-global vm-js-global)
($define! js-invoker vm-js-invoker)
($define! list* vm-list*)
($define! list->array vm-list-to-array)
($define! make-environment vm-make-environment)
($define! new vm-js-new)
($define! nil? vm-nil?)
($define! reverse-list vm-reverse-list)
($define! setter vm-setter)
($define! string->symbol vm-string-to-symbol)
($define! symbol-name vm-symbol-name)
($define! symbol? vm-symbol?)
($define! throw vm-throw)
($define! unwrap vm-unwrap)
($define! wrap vm-wrap)

;; Important utilities
($define! $vau vm-vau)
($define! quote ($vau (x) #ignore x))
($define! list (wrap ($vau elts #ignore elts)))
($define! the-environment ($vau () e e))
($define! get-current-environment (wrap ($vau () e e)))

;;;; Macro and vau

; derivazione Shutt!
;($define! $vau
; ((wrap
;     ($vau ($vau) #ignore
;       ($vau (formals eformal . body) env
;         (eval (list $vau formals eformal (cons begin body)) env) )))
;   $vau ))

($define! make-macro-expander
  (wrap
    ($vau (expander) #ignore
      ($vau operands env
        (eval (eval (cons expander operands) (make-environment)) env) ))))

; derivazione Simoni!
($define! $vau
  (make-macro-expander
    ($vau (params env-param . body) #ignore
      (list vm-vau params env-param (list* begin body)) )))

($define! macro
  (make-macro-expander
    ($vau (params . body) #ignore
      (list make-macro-expander (list* $vau params #ignore body)) )))

($define! define-macro
  (macro ((name . params) . body)
    (list $define! name (list* macro params body)) ))

(define-macro ($lambda params . body)
  (list wrap (list* $vau params #ignore body)) )

(define-macro (define-operative (name . params) envparam . body)
  (list $define! name (list* $vau params envparam body)) )

;;;; Wrap incomplete VM forms

(define-macro (loop . body)
  (list vm-loop (list* begin body)))

(define-operative (catch protected handler) env
  (eval (list vm-catch protected (eval handler env)) env) )

(define-operative (push-prompt prompt . body) env
  (eval (list vm-push-prompt (eval prompt env) (list* begin body)) env) )

(define-macro (take-subcont prompt k . body)
  (list vm-take-subcont prompt (list* $lambda (list k) body)) )

(define-macro (push-subcont k . body)
  (list vm-push-subcont k (list* $lambda () body)) )

(define-macro (push-prompt-subcont p k . body)
  (list vm-push-prompt-subcont p k (list* $lambda () body)) )

;;;; List utilities

($define! compose ($lambda (f g) ($lambda (arg) (f (g arg)))))

($define! car ($lambda ((x . #ignore)) x))
($define! cdr ($lambda ((#ignore . x)) x))
($define! caar (compose car car))
($define! cadr (compose car cdr))
($define! cdar (compose cdr car))
($define! cddr (compose cdr cdr))

;;;; Important macros and functions

($define! map-list
  ($lambda (f lst)
    (if (nil? lst)
        ()
        (cons (f (car lst)) (map-list f (cdr lst))) )))

($define! list-for-each
  ($lambda (f lst)
    (if (nil? lst)
        ()
        (begin (f (car lst)) (list-for-each f (cdr lst))) )))

($define! list-keep
  ($lambda (p lst)
    (if (nil? lst)
        ()
        (if (p (car lst))
            (cons (car lst) (list-keep p (cdr lst)))
            (list-keep p (cdr lst)) ))))

($define! fold-list
  ($lambda (f init lst)
    (if (nil? lst)
        init
        (fold-list f (f init (car lst)) (cdr lst)) )))

(define-macro (let x . rest)
  (if (symbol? x)
      (list* let-loop x rest)
      (list* (list* $lambda (map-list car x) rest)
             (map-list cadr x) )))

(define-macro (let* bindings . body)
  (if (nil? bindings)
      (list* let () body)
      (list let (list (car bindings))
            (list* let* (cdr bindings) body) )))

(define-macro (letrec bindings . body)
  (list* let ()
         (list $define!
               (map-list car bindings)
               (list* list (map-list cadr bindings)))
         body))

(define-macro (let-loop name bindings . body)
  (list letrec (list (list name (list* $lambda (map-list car bindings) body)))
        (list* name (map-list cadr bindings) )))

(define-macro (lambda params . body)
  (letrec ((typed-params->names-and-checks
            ($lambda (ps)
              (if (cons? ps)
                  (let* (((p . rest-ps) ps)
                         ((names . checks) (typed-params->names-and-checks rest-ps)))
                    (if (cons? p)
                        (let* (((name type) p)
                               (check (list the type name)))
                          (cons (cons name names) (cons check checks)))
                        (cons (cons p names) checks)))
                  (cons ps ())))))
    (let ( ((untyped-names . type-checks) (typed-params->names-and-checks params)) )
      (list* $lambda untyped-names (list* begin type-checks) body))))

(define-macro (define lhs . rhs)
  (if (cons? lhs)
    (list $define! (car lhs) (list* lambda (cdr lhs) rhs))
    (list $define! lhs (car rhs))))

(define (apply appv arg . opt)
  (if (instanceof appv &Function)
      (@apply appv #null (list->array arg))
      (eval (cons (unwrap appv) arg)
            (if (nil? opt)
                (make-environment)
                (car opt)))))

;;;; Simple control

(define-operative (cond . clauses) env
  (if (nil? clauses)
      #undefined
      (let ((((test . body) . clauses) clauses))
        (if (eval test env)
            (apply (wrap begin) body env)
            (apply (wrap cond) clauses env) ))))

(define else #t)

(define-operative (and . x) e
  (cond ((nil? x)         #t)
        ((nil? (cdr x))   (eval (car x) e))
        ((eval (car x) e) (apply (wrap and) (cdr x) e))
        (else             #f)))

(define-operative (or . x) e
  (cond ((nil? x)         #f)
        ((nil? (cdr x))   (eval (car x) e))
        ((eval (car x) e) #t)
        (else             (apply (wrap or) (cdr x) e))))

(define (call-with-escape fun)
  (let ((fresh (list #null)))
    (catch (fun ($lambda opt-arg (throw (list fresh opt-arg))))
      ($lambda (exc)
        (if (and (cons? exc) (=== fresh (car exc)))
            (let ((opt-arg (cadr exc)))
              (if (cons? opt-arg) (car opt-arg) #undefined))
            (throw exc))))))

(define-macro (label name . body)
  (list call-with-escape (list* $lambda (list name) body)))

(define-operative (while test . body) env
  (let ((body (list* begin body)))
    (label return
      (loop
        (if (eval test env)
          (eval body env)
          (return))))))

(define-macro (when test . body)
  (list if test (list* begin body) #null))

(define-macro (unless test . body)
  (list* when (list not test) body))

(define-macro (set (getter . args) new-val)
  (list* (list setter getter) new-val args))

;;;; Delimited dynamic binding

;; Evaluate right hand sides before binding all dynamic variables at once.
(define-operative (dlet bindings . body) env
  (eval
    (let process-bindings ((bs bindings))
       (if (nil? bs)
           (list* begin body)
           (let* ( (((name expr) . rest-bs) bs)
                   (value (eval expr env)) )
             (list vm-dlet name value (process-bindings rest-bs)) )))
    env ))

;;;; Prototypes

(define-operative (define-prototype name super-name prop-names) env
  (eval (list $define! name (make-prototype name super-name prop-names env)) env))

(define (make-prototype name super-name prop-names env)
  (let ((p (apply vm-js-make-prototype (list* (symbol-name name) (map-list symbol-name prop-names))))
        (super (eval super-name env)))
    (set (.prototype p) (@create &Object (.prototype super)))
    (set (.constructor (.prototype p)) super)
    p ))

(define-macro (define-generic (name . #ignore))
  (list $define! name (lambda args (apply ((js-getter name) (car args)) args))))

(define-macro (define-method (name (self ctor) . args) . body)
  (list put-method ctor (symbol-name name) (list* lambda (list* self args) body)))

(define (put-method ctor name fun)
  (set ((js-getter name) (.prototype ctor)) fun))

;;;; Modules

(define-operative (provide symbols . body) env
  (eval
    (list $define! symbols
      (list let ()
        (list* begin body)
        (list* list symbols) ))
    env ))

(define-operative (module exports . body) env
  (let ((menv (make-environment env)))
    (eval (list* provide exports body) menv)
    (make-environment menv) ))

(define-macro (define-module name exports . body)
  (list $define! name (list* module exports body)) )

(define-operative (import module imports) env
  (let* ((m (eval module env))
         (values (map-list ($lambda (import) (eval import m)) imports)))
    (eval (list $define! imports (list* list values)) env) ))

;;;; JavaScript

(define (relational-op name)
  (let ((binop (vm-js-binop name)))
    (letrec ((op (lambda (arg1 arg2 . rest)
                   (if (binop arg1 arg2)
                       (if (nil? rest)
                           #t
                           (apply op (list* arg2 rest)))
                       #f))))
      op)))

(define == (relational-op "=="))
(define === (relational-op "==="))
(define < (relational-op "<"))
(define > (relational-op ">"))
(define <= (relational-op "<="))
(define >= (relational-op ">="))

(define (!= . args) (not (apply == args)))
(define (!== . args) (not (apply === args)))

(define *
  (let ((vm* (vm-js-binop "*")))
    (lambda args
      (fold-list vm* 1 args) )))

;; Can't simply use 0 as unit or it won't work with strings
(define +
  (let ((vm+ (vm-js-binop "+")))
	(lambda args
	  (if (nil? args)
	      0
	      (fold-list vm+ (car args) (cdr args)) ))))

(define (negative-op binop unit)
  (lambda (arg1 . rest)
    (if (nil? rest)
        (binop unit arg1)
        (fold-list binop arg1 rest) )))

(define - (negative-op (vm-js-binop "-") 0))
(define / (negative-op (vm-js-binop "/") 1))

(define % (vm-js-binop "%"))
(define not (vm-js-unop "!"))
(define typeof (vm-js-unop "typeof"))
(define in (vm-js-binop "in"))
(define instanceof (vm-js-binop "instanceof"))

(define bitand (vm-js-binop "&"))
(define bitor (vm-js-binop "|"))
(define bitxor (vm-js-binop "^"))
(define bitnot (vm-js-unop "~"))
(define bitshiftl (vm-js-binop "<<"))
(define bitshiftr (vm-js-binop ">>"))
(define bitshiftr0 (vm-js-binop ">>>"))

(define-operative (object . pairs) env
  (let ((obj (vm-js-make-object)))
    (map-list ($lambda ((name value))
                (set ((js-getter (eval name env)) obj) (eval value env)))
              pairs)
    obj))

(define (elt object key)
  ((js-getter key) object))

(set (setter elt)
  (lambda (new-val object key)
    (set ((js-getter key) object) new-val) ))

(define (array . args) (list->array args))

(define (js-callback fun)
  (vm-js-function ($lambda args (push-prompt vm-root-prompt (apply fun args)))) )

(define-macro (js-lambda params . body)
  (list js-callback (list* lambda params body)))

(define-macro (type? obj type)
  (list vm-type? obj type (symbol-name type)))

(define-macro (the type obj)
  (list if (list type? obj type) obj (list error (list + obj " is not a: " type))) )

(define Array &Array)
(define Boolean &Boolean)
(define Date &Date)
(define Function &Function)
(define Number &Number)
(define Object &Object)
(define RegExp &RegExp)
(define String &String)

(define (log x . xs)
  (apply @log (list* &console x xs))
  x)

;;;; Cells

(define-prototype Cell Object (value))
(define (cell value) (new Cell value))
(define (ref (c Cell)) (.value c))
(set (setter ref) (lambda (new-val (c Cell)) (set (.value c) new-val)))

(define-macro (++ place)
  (list set place (list + place 1)) )
(define-macro (-- place)
  (list set place (list - place 1)) )

;;;; Utilities

;; ugh
(define (map-array fun (arr Array))
  (list->array (map-list fun (array->list arr))) )

(define (array-keep pred (arr Array))
  (list->array (list-keep pred (array->list arr))) )

(define-operative (time expr) env
  (let ((n (@getTime (new Date)))
        (result (eval expr env)))
    (log (+ "time " expr ": " (- (@getTime (new Date)) n) "ms"))
    result ))

(define-operative (assert-true expr) env
  (unless (=== #t (eval expr env))
    (error (+ "Should be true: " expr)) ))

(define-operative (assert-false expr) env
  (unless (=== #f (eval expr env))
     (error (+ "Should be false: " expr)) ))

(define-operative (assert-=== expected expr2) env
  (let ((res (eval expr2 env))
        (exp (eval expected env)))
    (unless (=== exp res)
      (error (+ expr2 " should be " exp " but is " res) ))))

(define-operative (assert-== expected expr2) env
  (let ((res (eval expr2 env))
        (exp (eval expected env)))
    (unless (== exp res)
      (error (+ expr2 " should be " exp " but is " res) ))))

(define-operative (assert-throws expr) env
  (label return
    (catch (eval expr env)
      (lambda (exc) (return)))
    (error (+ "Should throw: " expr)) ))

;;;; Options

(define-prototype Option Object ())
(define-prototype Some Option (value))
(define-prototype None Option ())
(define (some value) (new Some value))
(define none (new None))
(define-operative (if-option (option-name option-expr) then else) env
  (let ((option (the Option (eval option-expr env))))
    (if (type? option Some)
        (eval (list (list lambda (list option-name) then) (.value option)) env)
        (eval else env) )))

;;;; Error break routine, called by VM to print stacktrace and throw

(define (print-stacktrace err)
  (define (print-frame k)
    (log (@toString (.fun k)) (.dbg k) (.e k)) ;; @toString di .dbg == undefined no buono
    (when (type? (.next k) &StackFrame) ;; .next di !StackFrame no buono!
    	(print-frame (.next k)) ))
  (take-subcont vm-root-prompt k
  	(log err)
  	(log k)
    ;(print-frame k)
    (push-prompt vm-root-prompt
      (push-subcont k) )))

(define (user-break err)
  ;(print-stacktrace err)
  (throw err) )
