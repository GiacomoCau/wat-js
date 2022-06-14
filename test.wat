;; -*- mode: scheme -*-

;;;;; Wat Test Suite

;;;; Utilities

(define-operative (assert-true expr) env
  (log
    (if (== (eval expr env) #t) #t
      (error (+ "Should be true: " expr)) )))

(define-operative (assert-false expr) env
  (log
    (if (== (eval expr env) #f) #t
      (error (+ "Should be false: " expr)) )))

(define-operative (assert-equal expected expr2) env
  (let ((res (eval expr2 env)))
	(log
      (if (== (eval expected env) res) #t
        (error (+ expr2 " should be " expected " but is " res)) ))))

(define-operative (assert-throws expr) env
  (log
    (catch (begin (eval expr env) (+ "Should throw: " expr))
      (lambda (e) #t) )))

(assert-throws (lambda))
(assert-throws (lambda 12 12))
(assert-throws (lambda "foo" "bar"))
(assert-throws (def))
(assert-throws (def 12))
(assert-throws (def 12 12))

(assert-equal #null (begin))
(assert-equal 1 (begin 1))
(assert-equal 2 (begin 1 2))

;;;; Delimited Dynamic Binding Tests

;; adapted from 

(define-macro (test-check label expr expected)
  (list assert-equal expr expected))

(define (new-prompt) (list #null))

(define (abortP p e)
  (take-subcont p #ignore e))

(test-check 'test2
  (let ((p (new-prompt)))
    (+ (push-prompt p (push-prompt p 5))
      4))
  9)

(test-check 'test3
  (let ((p (new-prompt)))
    (+ (push-prompt p (+ (abortP p 5) 6))
      4))
  9)

(test-check 'test3-1
  (let ((p (new-prompt)))
    (+ (push-prompt p (push-prompt p (+ (abortP p 5) 6)))
      4))
  9)

(test-check 'test3-2
  (let ((p (new-prompt)))
    (let ((v (push-prompt p
	       (let* ((v1 (push-prompt p (+ (abortP p 5) 6)))
		      (v1 (abortP p 7)))
		 (+ v1 10)))))
      (+ v 20)))
  27)

'(test-check 'test3-3
  (let ((p (new-prompt)))
    (let ((v (push-prompt p (let*
	    ((v1 (push-prompt p (+ (abortP p 5) 6)))
		 (v1 (abortP p 7)) )
		(+ v1 10) ))))
      (abortP p 9)
      (+ v 20) ))
  'must-be-error )

;; (test-check 'test3-3-1
;;   (let ((p (new-prompt)))
;;     (let ((v (push-prompt p
;; 	       (let* ((v1 (push-prompt p (+ (abortP p 5) 6)))
;; 		      (v1 (abortP p 7)))
;; 		 (+ v1 10)))))
;;       (prompt-set? p)))
;;   #f)

(test-check 'test4
  (let ((p (new-prompt)))
    (+ (push-prompt p 
	 (+ (take-subcont p sk (push-prompt p (push-subcont sk 5)))
	   10))
      20))
  35 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (combine cmb ops) (apply (wrap cmb) ops))

(assert-true (combine and (list (== 1 1) (== 2 2))))
(assert-false (combine and (list (!= 1 1) (== 2 2))))

(assert-equal 2 (apply (lambda x x) 2))

(assert-throws (unwrap ($vau () #ignore)))


(let ((obj (object ("x" 1))))
  (set (.x obj) 2)
  (assert-equal 2 (.x obj))
  ;(set (@ obj "x") 3) ; non funzica
  (set (.x obj) 3)
  (assert-equal 3 (.x obj)) )


;(assert-equal &x #undefined) ; non funzica
(set &x 2)
(assert-equal &x 2)

(assert-equal 24 (* 1 2 3 4))
(assert-equal 1 (*))
(assert-equal 3 (* 3))
(assert-equal 10 (+ 1 2 3 4))
(assert-equal 0 (+))
(assert-equal 1 (+ 1))

(assert-equal -5 (- 5))
(assert-equal 3 (- 10 5 2))
(assert-equal (/ 1 5) (/ 5))
(assert-equal 9 (/ 54 2 3))

(assert-equal (@toString (list 1 2 3)) (@toString (reverse-list (list 3 2 1))))

(assert-equal "logging" (log "logging" 1 2 3))

(assert-true (and (== 1 1) (== 4 4) (== 5 5)))
(assert-false (and (== 1 1) (== 4 4) (== 5 10)))
(assert-true (or (== 1 1) (== 4 10) (== 5 5)))
(assert-true (or (== 1 10) (== 4 10) (== 5 5)))

(assert-true (== 4 (+ 2 2) (- 6 2)))
(assert-true (< 1 2 3 4 5))
(assert-false (< 1 2 3 4 5 1))
(assert-true (<= 1 1 2 3 4 5 5))
(assert-false (< 1 1 2 3 4 5 5))

(let ((x (cell 0)))
  (while (< (ref x) 10)
    (++ (ref x)))
  (assert-equal 10 (ref x)) )
