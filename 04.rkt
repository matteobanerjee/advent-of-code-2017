#lang racket

(define phrases (file->lines (string->path "./04_input.txt")))

(define (contains-dupes l)
  (define sorted (sort l string<?))
  (define (iter curr)
    (cond
      [(= (length curr) 1) false]
      [(string=? (first curr) (second curr)) true]
      [else (iter (rest curr))]))
  (iter sorted))

(length (filter (lambda (x) (not (contains-dupes x)))
                (map string-split phrases)))
