#lang racket

(struct prog (name weight children) #:prefab)

(define (string->prog str)
  (define (parse-weight w)
    (string->number
     (substring w 1 (- (string-length w) 1))))
  (define split (string-split str))
  (define name (first split))
  (define weight (parse-weight (second split)))
  (define children (if (> (length split) 2)
                       (map (lambda (x) (string-trim x ",")) (drop split 3))
                       '()))
  (prog name weight children))

(define programs (map string->prog (file->lines "07_input.txt")))

(define child-set
  (for*/set ([p (in-list programs)]
             [children (in-list (prog-children p))])
    children))

(define parent-set (list->set (map prog-name programs)))

(set-subtract parent-set child-set)
