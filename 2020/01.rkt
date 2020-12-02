#lang racket

(module amb racket/base
  (provide amb assert!)
  ;; Matthew Might's implementation
  (define fail-stack '())
  (define (fail!)
    (if (null? fail-stack)
        (error 'amb "Back-tracking stack exhausted!")
        (let ((back-track-point (car fail-stack)))
          (set! fail-stack (cdr fail-stack))
          (back-track-point back-track-point))))
  (define (amb choices)
    (let ((cc (call/cc values)))
      (if (null? choices)
          (fail!)
          (let ((choice (car choices)))
            (set! choices (cdr choices))
            (set! fail-stack (cons cc fail-stack))
            choice))))
  (define (assert! condition)
    (when (not condition)
      (fail!))))

(require 'amb)


(define input (file->list "input01.txt" read))

(define (part-1)
  (define ((adds-up? n a) b)
    (= n (+ a b)))
  (let loop ((n (car input)) (lst (cdr input)))
    (cond ((findf (adds-up? 2020 n) lst) => (curry * n))
          (else (loop (car lst) (cdr lst))))))

(define (part-2)
  (let ((a (amb input))
        (b (amb input))
        (c (amb input)))
    (assert! (= 2020 (+ a b c)))
    (* a b c)))

(module+ main
  (display "Day 1, Part 1: ")
  (time (part-1))
  (display "Day 1, Part 2: ")
  (time (part-2)))
