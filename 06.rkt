#lang racket

;; The next memory bank, the cell with the highest value. in a tie
;; the lowest index wins
(define (next-bank l)
  (index-of l (apply max l)))

;; shift a list
(define (cycle l shift)
  (define len (length l))
  (define (next-index i) (if (= i (- len 1)) 0 (+ 1 i)))
  (define (iter idx remaining accum)
    (if (= remaining 0)
        (reverse accum)
        (iter (next-index idx) (- remaining 1) (cons (list-ref l idx) accum))))
    (iter (- len shift) len '()))

(define (next-state l)
  (let*
      [(bank (next-bank l))
       (bank-val (list-ref l bank))
       (len (length l))
       (base-mask (make-list len (floor (/ bank-val len))))
       (remain (remainder bank-val len))
       (tmp-mask (append (make-list remain 1) (make-list (- len remain) 0)))
       (remainder-mask (cycle tmp-mask (+ 1 bank)))]
    (map + (list-set l bank 0) base-mask remainder-mask)))

(define (solve initial)
  (define seen (mutable-set))
  (define (iter step l)
    (if (set-member? seen l)
        step
        (begin (set-add! seen l)
               (iter (+ 1 step) (next-state l)))))
  (iter 0 initial))


(solve '(2 4 1 2))
(solve '(2 8 8 5 4 2 3 1 5 5 1 2 15 13 5 14))
