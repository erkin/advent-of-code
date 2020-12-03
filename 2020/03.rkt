#lang racket
(require srfi/1)

(define input (file->lines "input03.txt"))

(define (part-1)
  (let loop ((rows input) (shift 0) (trees 0))
    (cond
      ((null? rows)
       trees)
      ((>= shift 31)
       (loop rows (- shift 31) trees))
      (else
       (loop (cdr rows)
             (+ shift 3)
             (if (char=? #\# (string-ref (car rows) shift))
                 (add1 trees)
                 trees))))))

#;
(define (part-2)
  )

(module+ main
  (display "Day 3, Part 1: ")
  (time (part-1))
#;  (display "Day 3, Part 2: ")
#;  (time (part-2)))
