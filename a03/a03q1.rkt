;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname a03q1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))

;; ******************************************
;;   Ekansh Lakhyani (20945086)
;;   CS 115 Fall 2021
;;   Assignment 03, Question 1
;; ******************************************


;; (abstract-advice low high val  s1 s2 s) produces s1 if val is less
;;  than low,
;;  s2 if val is between low and high, inclusively, and
;;  s3 if val is greater than high.
;; abstract-advice: Num Num Num Sym Sym Sym -> Sym
;; Requires: low <= high

;; Examples:
(check-expect (abstract-advice 0 100 37 'ice 'liquid 'steam)
              'liquid)
(check-expect (abstract-advice 63.75 64.15 65.55 'buy 'hold 'sell)
              'sell)
(check-expect (abstract-advice 50 80 2 'fail 'passing 'honours)
              'fail)

(define (abstract-advice low high val s1 s2 s3)
  (cond
    [(< val low) s1]
    [(> val high) s3]
    [else s2]))

;; Tests:
(check-expect (abstract-advice 1 2 3 'a 'b 'c) 'c)
(check-expect (abstract-advice 1 3 2 'a 'b 'c) 'b)
(check-expect (abstract-advice 2 3 1 'a 'b 'c) 'a)
