#lang racket

(define input (file->lines "input02.txt"))

(define (part-1)
  (define substring-count
    (compose length regexp-match*))
  (define (valid-entry? str)
    (define-values (_ min max letter password)
      (apply values (regexp-match #rx"([0-9]+)-([0-9]+) ([a-z]): ([a-z]+)" str)))
    (<= (string->number min)
        (substring-count letter password)
        (string->number max)))
  (count valid-entry? input))

(define (part-2)
  (define (valid-entry? str)
    (define-values (_ yes no letter password)
      (apply values (regexp-match #rx"([0-9]+)-([0-9]+) ([a-z]):( [a-z]+)" str)))
    (let ((positions (regexp-match-positions* letter password)))
      (xor (assoc (string->number yes) positions)
           (assoc (string->number no) positions))))
  (count valid-entry? input))

(module+ main
  (display "Day 2, Part 1: ")
  (time (part-1))
  (display "Day 2, Part 2: ")
  (time (part-2)))
