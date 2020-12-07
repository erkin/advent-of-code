#lang racket

(define input (file->lines "input07.txt"))

(define (part-1)
  (define parse-bag
    (match-lambda
      ("no other bags."
       #f)
      ((regexp "([0-9]+) ([a-z ]+) bags?" (list _ number colour))
       colour)
      ((regexp "([a-z ]+) bags" (list _ colour))
       colour)))
  (define bags (make-hash))
  (define (chase-bags lst st)
    (if (null? lst)
        st
        (let ((bag (car lst)))
          (set-union
           (chase-bags (hash-ref bags bag '()) st)
           (chase-bags (cdr lst) (set-add st bag))))))
  (for ((rule (in-list input)))
    (let ((parts (map parse-bag (string-split rule #rx"(,| contain) "))))
      (for-each
       (lambda (colour)
         (hash-update! bags colour (curry cons (car parts)) '()))
       (cdr parts))))
  (set-count (chase-bags (hash-ref bags "shiny gold") (set))))

(define (part-2)
  #f)

(module+ main
  (display "Day 7, Part 1: ")
  (time (part-1))
  (display "Day 7, Part 2: ")
  (time (part-2)))
