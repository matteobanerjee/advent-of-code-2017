#lang racket
;; TODO: write a racket language for this

(define ns (make-base-namespace))
(define (eval-in-ns f) (eval f ns))

;; define our language
(eval-in-ns '(define (inc x y) (+ x y)))
(eval-in-ns '(define (dec x y) (- x y)))
(eval-in-ns '(define (== x y) (= x y)))
(eval-in-ns '(define (!= x y) (not (= x y))))

(define (to-sym-or-number s)
  (define maybe-num (string->number s))
  (if (not maybe-num) (string->symbol s) maybe-num))

(define (infix->prefix expr)
  (define syms (map to-sym-or-number expr))
  (list (second syms) (first syms) (third syms)))

(define (eval-line line register-mutable-set)
  (define split (string-split line " "))
  (define reg-1 (string->symbol (first split)))
  (define reg-2 (string->symbol (fifth split)))
  (define (init-register reg)
    (begin
      (if (not (set-member? register-mutable-set reg))
          (eval-in-ns (list 'define reg  0))
          null)
      (set-add! register-mutable-set reg)))
  (define cond-part (infix->prefix (drop split 4)))
  (define set-part (infix->prefix (take split 3)))
  (begin
    (init-register reg-1)
    (init-register reg-2)
    (if (eval-in-ns cond-part)
        (eval-in-ns (list 'set! (second set-part) set-part))
        null)))

(begin
  (define s (mutable-set))
  ;; should use for
  (map (lambda (l) (eval-line l s)) (file->lines "./08_input.txt"))
  (apply max (set-map s (lambda (x) (eval-in-ns x)))))
