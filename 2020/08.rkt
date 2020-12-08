#lang racket

(define input
  (map
   (curryr with-input-from-string
           (thunk (list (read) (read))))
   (file->lines "input08.txt")))

(define (part-1)
  (define memory (list->vector input))
  (define seen (mutable-set))
  (let cpu ((index 0) (acc 0))
    (define-values (ins val) (apply values (vector-ref memory index)))
    (cond ((set-member? seen index)
           acc)
          (else
           (set-add! seen index)
           (case ins
             ((nop) (cpu (add1 index) acc))
             ((acc) (cpu (add1 index) (+ acc val)))
             ((jmp) (cpu (+ index val) acc)))))))

(define (part-2)
  #f)

(module+ main
  (display "Day 7, Part 1: ")
  (time (part-1))
  (display "Day 7, Part 2: ")
  (time (part-2)))
