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

(define (part-2)
  (define (nth-rest lst n)
    (if (or (zero? n) (null? lst))
        lst
        (nth-rest (cdr lst) (sub1 n))))
  (define (tree-check right down)
    (let loop ((rows input) (shift 0) (trees 0))
      (cond
        ((null? rows)
         trees)
        ((>= shift 31)
         (loop rows (- shift 31) trees))
        (else
         (loop (nth-rest rows down)
               (+ shift right)
               (if (char=? #\# (string-ref (car rows) shift))
                 (add1 trees)
                 trees))))))
  (* (tree-check 1 1)
     (tree-check 3 1)
     (tree-check 5 1)
     (tree-check 7 1)
     (tree-check 1 2)))

(module+ main
  (display "Day 3, Part 1: ")
  (time (part-1))
  (display "Day 3, Part 2: ")
  (time (part-2)))
