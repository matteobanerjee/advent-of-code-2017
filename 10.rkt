#lang racket
(require rackunit)

(define (sublist l from to)
  (take (drop l from) (- to from)))

(define (wrap-around-sublist l from to)
  (define len (length l))
  (if (<= to from)
      ;; wrap around
      (append (sublist l from len) (sublist l 0 to))
      (sublist l from to)))

(define (wrap-around-replace l from to n)
  (define len (length l))
  (if (<= to from)
      ;; wrap around
      (append (drop n (- len from)) (sublist l to from) (take n (- len from)))
      (append (take l from) n (drop l to))))

(define (ints-to j)
  (for/list ([i (in-naturals)]
             #:break (> i j))
    i))

(define (perform-transform l from to)
  (wrap-around-replace l from to (reverse (wrap-around-sublist l from to))))


(define (solve initial-list initial-lengths)
  (define (wrap-around x) (modulo x (length initial-list)))
  (define (iter l lengths offset skip-size)
    (if (empty? lengths)
        l
        (let* ([cur-length (first lengths)]
               [next-l (perform-transform l offset (wrap-around (+ offset cur-length)))]
               [next-lengths (rest lengths)]
               [next-offset (wrap-around (+ offset skip-size cur-length))])
          (iter next-l next-lengths next-offset (+ 1 skip-size)))))
  (iter initial-list initial-lengths 0 0))

(define res
  (solve
   (ints-to 255)
   '(120 93 0 90 5 80 129 74 1 165 204 255 254 2 50 113)))

(* (first res) (second res))
