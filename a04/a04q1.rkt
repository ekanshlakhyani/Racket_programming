;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname a04q1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ******************************************
;;   Ekansh Lakhyani (20945086)
;;   CS 115 Fall 2021
;;   Assignment 04, Question 1
;; ******************************************
;;


;; Helper functions

;; (second-decimal n) produces the second decimal place in the given
;;  number n
;; second-decimal: Num -> Int
;; Requires: A number with maximum 2 decimal places 

;; Examples:
(check-expect (second-decimal 15.32) 2)
(check-expect (second-decimal 0) 0)
(check-expect (second-decimal 5.5) 0)

(define (second-decimal n)
  (remainder  (*(- n (floor n))100) 10))


;; (rounder d) produces the number d rounded to nearest nickel  
;; rounder: Num -> Num
;; Requires: A number with maximum 2 decimal places 

;; Examples:
(check-expect (rounder 17.67) 17.65)
(check-expect (rounder 14.51) 14.5)
(check-expect (rounder 167.39) 167.4)
(check-expect (rounder 35) 35)
(check-expect (rounder 0) 0)

(define (rounder d)
  (cond [(or (= 1  (second-decimal d))
             (= 2 (second-decimal d)))
         (- d (/(second-decimal d)100))]
        [(or (= 3  (second-decimal d))
             (= 4  (second-decimal d))
             (= 6  (second-decimal d))
             (= 7  (second-decimal d)))
         (+ (- d (/(second-decimal d)100)) 0.05)]
        [(or (= 8  (second-decimal d))
             (= 9  (second-decimal d)))
         (+ (- d (/(second-decimal d)100)) 0.1)]
        [else d]))


;; Main Function

;; (no-penny-prices prices) produces a list of numbers representing 
;;  prices in dollars rounded to the nearest nickel.
;; no-penny-prices: (listof Num) -> (listof Num)
;; Requires: A list of numbers with each number not containing more
;;  than 2 decimal places

;;Examples
(check-expect
 (no-penny-prices (cons 15.05 (cons 10.01 (cons 12.38 empty))))
 (cons 15.05 (cons 10 (cons 12.4 empty))))
(check-expect
 (no-penny-prices (cons 4 (cons 3.77 (cons 5.3 empty))))
 (cons 4 (cons 3.75 (cons 5.3 empty))))

(define (no-penny-prices prices)
  (cond
    [(empty? prices) empty]
    [else (cons (rounder (first prices)) 
                (no-penny-prices (rest prices)))]))

;; Tests
(check-expect
 (no-penny-prices (cons 16.01 (cons 10.67 (cons 4656.3 empty))))
 (cons 16 (cons 10.65 (cons 4656.3 empty))))
(check-expect
 (no-penny-prices (cons 11.98 (cons 66 (cons 0.86 empty))))
 (cons 12 (cons 66 (cons 0.85 empty))))
(check-expect
 (no-penny-prices (cons 0 (cons 8.22 (cons 50 empty))))
 (cons 0 (cons 8.2 (cons 50 empty))))