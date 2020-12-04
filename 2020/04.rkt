#lang racket

(define input (string-split (file->string "input04.txt") "\n\n"))

(define (part-1)
  (let ((fields '("byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid")))
    (count (lambda (s) (andmap (curry string-contains? s) fields)) input)))

(define (part-2)
  (define valid?
    (match-lambda
      ;; Birth year
      ((regexp #rx"byr:([0-9]+)" (list _ year))
       (<= 1920 (string->number year) 2002))
      ;; Issue year
      ((regexp #rx"iyr:([0-9]+)" (list _ year))
       (<= 2010 (string->number year) 2020))
      ;; Expiration year
      ((regexp #rx"eyr:([0-9]+)" (list _ year))
       (<= 2020 (string->number year) 2030))
      ;; Height in inches
      ((regexp #rx"hgt:([0-9]+)in" (list _ inches))
       (<= 59 (string->number inches) 76))
      ;; Height in centimetres
      ((regexp #rx"hgt:([0-9]+)cm" (list _ centimetres))
       (<= 150 (string->number centimetres) 193))
      ;; Hair colour
      ((regexp #px"hcl:#([0-9a-f]{6})$")
       #t)
      ;; Eye colour
      ((regexp #rx"ecl:(.+)" (list _ colour))
       (member colour '("amb" "blu" "brn" "gry" "grn" "hzl" "oth")))
      ;; Passport ID
      ((regexp #px"pid:([0-9]{9})$" (list _ ID))
       #t)
      ;; Country ID
      ((regexp #rx"cid:.+")
       #t)
      ;; Invalid field
      (_ #f)))
  (define fields '("byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"))
  (count (lambda (s)
           (and
            ;; Contains all necessary fields?
            (andmap (curry string-contains? s) fields)
            ;; All given fields are valid?
            (andmap valid? (string-split s)))) input))

(module+ main
  (display "Day 4, Part 1: ")
  (time (part-1))
  (display "Day 4, Part 2: ")
  (time (part-2)))

