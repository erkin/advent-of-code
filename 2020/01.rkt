#lang racket

(define ((adds-up? n a) b)
  (= n (+ a b)))

(define (part-1)
  (define input (file->list "input01.txt" read))
  (let loop ((n (car input)) (lst (cdr input)))
    (cond ((findf (adds-up? 2020 n) lst) => (curry * n))
          (else (loop (car lst) (cdr lst))))))

