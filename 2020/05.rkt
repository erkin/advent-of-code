#lang racket

(define input (file->lines "input05.txt"))

(define (parse-indices str low-char top)
  (let loop ((chars (string->list str)) (down 0) (up top))
    (let ((diff (add1 (quotient (- up down) 2))))
      (cond ((null? chars)
             up)
            ((char=? (car chars) low-char)
             (loop (cdr chars) down (- up diff)))
            (else
             (loop (cdr chars) (+ down diff) up))))))

(define (string->seat-id s)
  (let ((row (substring s 0 7))
        (column (substring s 7)))
    (+ (parse-indices column #\L 7)
       (* 8 (parse-indices row #\F 127)))))

(define (part-1)
  (apply max (map string->seat-id input)))

(define (part-2)
  (define combos
   (let ((r (in-string "FB"))
         (c (in-string "LR")))
     (for*/set ((r0 r) (r1 r) (r2 r) (r3 r) (r4 r) (r5 r) (r6 r)
                       (c0 c) (c1 c) (c2 c)
                       #:unless (char=? r0 r1 r2))
       (string r0 r1 r2 r3 r4 r5 r6 c0 c1 c2))))
  (string->seat-id (car (set->list (set-subtract combos (apply set input))))))

(module+ main
  (display "Day 5, Part 1: ")
  (time (part-1))
  (display "Day 5, Part 2: ")
  (time (part-2)))

