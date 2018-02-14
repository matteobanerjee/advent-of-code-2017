#lang racket

(define (next-direction d p)
  (define (is-corner p) (= (abs (first p)) (abs (second p))))
  (define direction-hash
    (hash
     'incr-y 'decr-x
     'decr-x 'decr-y
     'decr-y 'incr-x
     'incr-x 'jump
     'jump 'incr-y))
  (cond
    [(eq? d 'jump) 'incr-y]
    [(is-corner p) (hash-ref direction-hash d)]
    [else d]))

(define (x-apply-direction d p)
  (define (incr a) (+ 1 a))
  (define (decr a) (- a 1))
  (define (set-x p x) (list x (second p)))
  (define (set-y p y) (list (first p) y))
  (match d
      ['incr-x (set-x p (incr (first p)))]
      ['decr-x (set-x p (decr (first p)))]
      ['incr-y (set-y p (incr (second p)))]
      ['decr-y (set-y p (decr (second p)))]
      ['jump   (set-x p (incr (first p)))]))

(define (do-it prev-d p)
  (let* [(next-d (if (null? prev-d) 'jump (next-direction prev-d p)))
        (next-p (x-apply-direction next-d p))]
    (list next-d next-p)))

;; (0, 0) (1, 0) (1, 1) (0, 1)

(define (spiral-enum-from start direction)
  (define next-pair (do-it direction start))
  (stream-cons start (spiral-enum-from
                      (second next-pair)
                      (first next-pair))))

(define spiral-enum (spiral-enum-from '(0 0) null))

(define (manhattan-dist coord) (apply + (map abs coord)))
(define (solve n)
  (manhattan-dist (stream-ref spiral-enum (- n 1))))
(solve 1)
(solve 12)
(solve 23)
(solve 1024)
(solve 325489)
