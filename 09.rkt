#lang racket
(require rackunit)

(struct context (score depth in-garbage in-escape) #:transparent)

(define (iter current-string ctx)
  (define next-char (string-ref current-string 0))
  (define next-string (substring current-string 1))
  (define (handle-garbage)
    (if (context-in-escape ctx)
        (iter next-string (struct-copy context ctx [in-escape #f]))
        (match next-char
          [#\> (iter next-string (struct-copy context ctx [in-garbage #f]))]
          [#\! (iter next-string (struct-copy context ctx [in-escape #t]))]
          [_   (iter next-string ctx)])))
  (define (handle-good)
    (define cur-score (context-score ctx))
    (define cur-depth (context-depth ctx))
    (define (handle-close)
      (define next-score (+ cur-depth cur-score))
      (if (= (string-length next-string) 0)
          next-score
          (iter next-string (struct-copy context ctx [score next-score] [depth (- cur-depth 1)]))))
    (match next-char
      [#\{ (iter next-string (struct-copy context ctx [depth (+ 1 cur-depth)]))]
      [#\} (handle-close)]
      [#\< (iter next-string (struct-copy context ctx [in-garbage #t]))]
      [_   (iter next-string ctx)]))
  (if (context-in-garbage ctx) (handle-garbage) (handle-good)))

(define (solve input) (iter input (context 0 0 #f #f)))


(check-equal? (solve "{}") 1)
(check-equal? (solve "{{{}}}") 6)
(check-equal? (solve "{{{},{},{{}}}}") 16)
(check-equal? (solve "{{<a!>},{<a!>},{<a!>},{<ab>}}") 3)

(solve (file->string "./09_input.txt"))
