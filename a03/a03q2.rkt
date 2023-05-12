;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname a03q2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))




;; (listen s) produces a number based on what the string contains.
;;  If just produces number or adds or subtracts or multiplies or 
;;  divides as per the command mentioned in string.
;; listen: Str -> Num
;; Requires:
;;  * string s to be in one of the following formats:
;;   "A"
;;   "add B and C"
;;   "subtract E from D"
;;   "multiply F by G"
;;   "divide H by I"
;;  * Where A to H are all single digit integers from 0 to 9
;;   and I is in the range of 1 to 9.

;; Examples:
(check-expect (listen "subtract 3 from 8") 5)
(check-expect (listen "9") 9)
(check-expect (listen "divide 7 by 4") 1.75)

(define (listen s)
  (cond
    [(string=? (string-ith s 0) "a")
     (+ (string->number (string-ith s 4))
        (string->number (string-ith s 10)))]
    [(string=? (string-ith s 0) "s")
     (- (string->number (string-ith s 16))
        (string->number (string-ith s 9)))]
    [(string=? (string-ith s 0) "m")
     (* (string->number (string-ith s 9))
        (string->number (string-ith s 14)))]
    [(string=? (string-ith s 0) "d")
     (/ (string->number (string-ith s 7))
        (string->number (string-ith s 12)))]
    [else (string->number s)]))

;; Tests:
(check-expect (listen "1") 1)
(check-expect (listen "add 1 and 1") 2)
(check-expect (listen "subtract 1 from 2") 1)
(check-expect (listen "multiply 1 by 2") 2)
(check-expect (listen "divide 4 by 2") 2)
