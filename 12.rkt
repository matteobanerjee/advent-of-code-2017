#lang racket

(define (parse-line s)
  (define (trim-trailing-comma t) (string-trim t ","))
  (define split (map trim-trailing-comma (filter non-empty-string? (string-split s " "))))
  (list
   (string->number (first split))
   (map string->number (drop split 2))))

(define (get-connected g n)
  (define (iter queue pos seen-set)
    (define (neighbors cn)
      (filter-not (λ (x) (set-member? seen-set x)) (hash-ref g cn)))
    (if (= pos (length queue))
        queue
        (let* ([current-node (list-ref queue pos)]
               [next (neighbors current-node)])
          (iter
           (append queue (neighbors current-node))
           (+ 1 pos)
           (set-union seen-set (list->set next))))))
  (iter (list n) 0 (set n)))



(define lines
  (map parse-line (file->lines "./12_input.txt")))

(define g (for/hash ([line (in-list lines)])
  (values (first line) (second line))))

;; part-1
(printf "PART 1: ~a\n" (length (get-connected g 0)))


;; Part 2
(define (filter-hash proc h)
  (for/hash ([(k v) (in-hash h)]
             #:when (proc k v))
    (values k v)))

(define (connected-components g [accum '()])
  (if (= 0 (hash-count g))
      accum
      (let* ([first-key (for/first ([(k v) (in-hash g)]) k)]
             [component (get-connected g first-key)]
             [next-g (filter-hash (λ (k v) (not (member k component))) g)])
        (connected-components next-g (cons component accum)))))

(printf "PART 2: ~a\n" (length (connected-components g)))

