#lang racket

(module amb racket/base
  (provide amb assert!)
  ;; Matthew Might's implementation, with some alterations
  (define-syntax-rule (pop! stack)
    (begin0 (car stack) (set! stack (cdr stack))))
  (define-syntax-rule (push! v stack)
    (set! stack (cons v stack)))
  (define (U f) (f f))

  (define fail-stack '())
  (define (fail!)
    (if (null? fail-stack)
        (error 'amb "Back-tracking stack exhausted!")
        (U (pop! fail-stack))))
  (define (amb choices)
    (let ((cc (call-with-current-continuation values)))
      (cond ((null? choices) (fail!))
            (else (push! cc fail-stack)
                  (pop! choices)))))
  (define (assert! condition)
    (unless condition (fail!))))

(require 'amb)

(define input (file->list "input01.txt"))

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
