;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname a05q1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ******************************************
;;   Ekansh Lakhyani (20945086)
;;   CS 115 Fall 2021
;;   Assignment 05, Question 1
;; ******************************************
;;


;; Helper Functions

;; (prime-checker n initial) produces true if the natural number n is
;;  a prime, if not produces false, if the natural number initial is
;;  2. Otherwise just produces true if natural numbebr n is the  
;;  multiple of natural number initial or a any natural number greater
;;  than initial other than the number itself.
;; prime-checker: Nat Nat -> Bool
;; Requires: A natural number n that is
;;  * n >= 4
;;  * n is even
;;  and a natural number initial that is greater than 1

;; Examples:
(check-expect (prime-checker 4 2) false)
(check-expect (prime-checker 5 2) true)
(check-expect (prime-checker 6 3) false)
(check-expect (prime-checker 8 2) false)
(check-expect (prime-checker 7 2) true)
(check-expect (prime-checker 9 2) false)

(define (prime-checker n initial)
  (cond [(= initial n) true]
        [(= (remainder n initial) 0) false]
        [else (prime-checker n (+ initial 1))]))


;; (find-pair starting prime) produces the list of the list of pairs
;;  which are natural prime numbers who sum up to be the natural
;;  number n. 
;; find-pair: Nat Nat -> (listof (listof Nat Nat))
;; Requires: starting be a natural number
;;  * starting >= 4
;;  * staring is even
;;  a natural number prime which is greater than 1
;;  (for the function to work as intended the prime should be
;;  2 at the start)

;; Examples:
(check-expect (find-pair 4 2) (list(list 2 2)))
(check-expect (find-pair 10 2) (list (list 3 7) (list 5 5)))
(check-expect (find-pair 6 2) (list(list 3 3)))
(check-expect (find-pair 12 2) (list(list 5 7)))

(define (find-pair starting prime)
  (cond [(< (/ starting 2) prime) empty]
        [(prime-checker (- starting prime) 2)
         (cons (list prime (- starting prime))
               (find-pair starting (+ prime 1)))]
        [else (find-pair starting (+ prime 1))]))


;; Main Function

;; (goldbach-pairs n) produces a list of two elements lists, giving
;;  all distinct pairs of primes that sum to n.
;; goldbach-pairs: Nat -> (listof (listof Nat Nat))
;; Requires: A natural number n that is
;;  * n >= 4
;;  * n is even

;; Examples:
(check-expect (goldbach-pairs 4) (list(list 2 2)))
(check-expect (goldbach-pairs 10) (list (list 3 7) (list 5 5)))
(check-expect (goldbach-pairs 6) (list(list 3 3)))
(check-expect (goldbach-pairs 12) (list(list 5 7)))

(define (goldbach-pairs n)
  (find-pair n 2))

;; Tests
(check-expect (goldbach-pairs 4) (list(list 2 2)))
(check-expect (goldbach-pairs 60)
              (list (list 7 53) (list 13 47) (list 17 43)
                    (list 19 41) (list 23 37) (list 29 31)))
(check-expect (goldbach-pairs 6) (list(list 3 3)))
(check-expect (goldbach-pairs 14)
              (list(list 3 11) (list 7 7)))
(check-expect (goldbach-pairs 16)
              (list(list 3 13) (list 5 11)))
(check-expect (goldbach-pairs 18)
              (list(list 5 13) (list 7 11)))
(check-expect (goldbach-pairs 20)
              (list(list 3 17) (list 7 13) (list 9 11)))
(check-expect (goldbach-pairs 22)
              (list(list 3 19) (list 5 17) (list 9 13) (list 11 11)))


