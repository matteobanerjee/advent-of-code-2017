#lang racket

(define (incr x) (+ 1 x))

(define (solve-help l offset step)
  (let* [(jump-dist (list-ref l offset))
         (next-offset (+ offset jump-dist))
         (escaped? (>= next-offset (length l)))
         (updated-l (list-set l offset (incr jump-dist)))]
    (if escaped?
        step
        (solve-help updated-l next-offset (incr step)))))

(define (solve l)
  (solve-help l 0 1))

(solve '(0 3 0 1 -3))
(solve (file->list "./05_input.txt"))
