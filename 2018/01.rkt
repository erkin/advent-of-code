#lang racket

(define input (file->list "input01.txt" read))

(define (part-1)
  (apply + input))

(define (part-2)
  (let loop ((changes input) (frequency 0) (past (set)))
    (cond
      ((set-member? past frequency)
       frequency)
      ((null? changes)
       (loop input frequency past))
      (else
       (loop (cdr changes) (+ frequency (car changes)) (set-add past frequency))))))

(module+ main
  (display "Day 1, Part 1: ")
  (time (part-1))
  (display "Day 1, Part 2: ")
  (time (part-2)))
