#lang racket

(define (solve l)
  (define (fold-func elem v) ;; accumulator is the pair (previous running-sum)
    (if (= elem (first v))
        (list elem (+ elem (second v)))
        (list elem (second v))))
;; start with the last to mimick wrap around
  (define initial-value (list (last l) 0))
  (second (foldl fold-func initial-value  l)))

(define input
  (map string->number
       (filter non-empty-string?
               (string-split (file->string "./01_input.txt") ""))))

(solve input)
