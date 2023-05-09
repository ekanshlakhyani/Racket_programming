;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname a05q3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ******************************************
;;   Ekansh Lakhyani (20945086)
;;   CS 115 Fall 2021
;;   Assignment 05, Question 3
;; ******************************************
;;


;; A Pair is a (list Sym Nat)

;; Constructor
(define (make-pair key val) (list key val))
;; Selectors
(define (pair-key p) (first p))
(define (pair-val p) (second p))

;; A Histogram is a (listof Pair)

;; Helper Functions:


;; (al-add key val dict) produces AL counting the amount of times each
;;  consuming a symbol appears in the list. It consumes a symbol key,
;;  a number val and an associative list Dict.
;; al-add: Sym Num AL-> AL

;; Examples:
(check-expect (al-add 'a 4 empty)
              (list (list 'a 4)))
(check-expect (al-add  'ekansh 18 empty)
              (list (list 'ekansh 18)))
(check-expect (al-add  'e 1 empty)
              (list (list 'e 1)))

(define (al-add key val dict)
  (cond
    [(empty? dict) (cons (make-pair key val) empty)]
    [(equal? key (pair-key (first dict)))
     (cons (make-pair key val) (rest dict))]
    [else (cons (first dict) 
            (al-add key val (rest dict)))]))


;; (number-of-occurence los symbol) produces a natural number, which
;;  is the number of times symbol symbol occurs in the list of
;;  symbols los.
;; number-of-occurence: (listof Sym) Sym -> Nat

;; Examples:
(check-expect (number-of-occurence (list 'q 'g) 'q) 1)
(check-expect (number-of-occurence (list 'q 'q) 'q) 2)
(check-expect (number-of-occurence (list 'q 'g) 'hmv) 0)
(check-expect (number-of-occurence (list empty) 'q) 0)

(define (number-of-occurence los symbol)
  (cond [(empty? los) 0]
        [else (+ (cond 
                   [(equal? (first los) symbol) 1]
                   [else 0])
                 (number-of-occurence (rest los) symbol))]))


;; (build-histogram los) produces the list of list of pair of the
;;  symbol from the list and the number of times it occurs in the list
;; build-histogram: (listof Sym) -> AL

;; Examples:
(check-expect (build-histogram (list 'a 'b))
              (list (list 'b 1) (list 'a 1)))
(check-expect (build-histogram (list 'a 'b 'a 'b 'c))
              (list (list 'c 1) (list 'b 2) (list 'a 2)))

(define (build-histogram los)
  (cond
    [(empty? los) empty]
    [else
     (al-add (first los)
             (number-of-occurence los (first los))
                        (build-histogram (rest los)))]))

;; Tests
(check-expect (build-histogram (list 'a 'b 'a 'b 'c 'b))
               (list (list 'b 3) (list 'c 1) (list 'a 2)))
(check-expect (build-histogram (list 'c 'c 'c 'c 'c))
              (list (list 'c 5)))

