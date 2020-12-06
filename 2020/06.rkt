#lang racket
(require threading)

(define input (string-split (file->string "input06.txt") "\n\n"))

(define (part-1)
  (for/sum ((group (in-list input)))
    (~> group
        (string-split _ "\n")
        (map string->list _)
        (apply append _)
        list->set
        set-count)))

(define (part-2)
  (for/sum ((group (in-list input)))
    (~> group
        (string-split _ "\n")
        (map (compose list->set string->list) _)
        (apply set-intersect _)
        set-count)))

(module+ main
  (display "Day 6, Part 1: ")
  (time (part-1))
  (display "Day 6, Part 2: ")
  (time (part-2)))
