;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname a05q2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ******************************************
;;   Ekansh Lakhyani (20945086)
;;   CS 115 Fall 2021
;;   Assignment 05, Question 2
;; ******************************************
;;


;; Helper Functions

;; (insert s los) produces list of string with string s inserted in
;;  the list of string los in the alphabetic order. 
;; insert: Str (listof Str) -> (listof Str)
;; Requires: los is sorted

;; Examples:
(check-expect (insert "WW 33" (list "AA 33")) (list "AA 33" "WW 33"))

(define (insert s los)
  (cond
    [(empty? los) (list s)]
       [(string<=? s (first los)) (cons s los)] ; base case!
    [else (cons (first los) (insert s (rest los)))]))


;; (sort-courses courselst) produces a list containing the same
;;  courses as in courselst in sorted order. Two courses are put in
;;  alphabetical order according to their prefixes. If two courses
;;  have the same prefix, they are sorted in numeric order based on
;;  their numbers.
;; sort-courses: (listof Str) -> (listof Str)
;; Requires: string courselst should contain a prefix and a natural
;;  number, separated by a space.
;;  * The prefix should be made up of English capital letters
;;  * the number should be made of up the digits 0–9
;;  * the number shouldn't start with 0
;;  * The prefix and number each contain at least one character.

;; Examples
(check-expect (sort-courses (list "FF 23" "AA 34"))
              (list "AA 34" "FF 23"))

(define (sort-courses courselst)
  (cond
    [(empty? courselst) empty]
    [else (insert (first courselst)
                  (sort-courses (rest courselst)))]))

;; Tests:
(check-expect (sort-courses (list "AA 23" "AA 34"))
              (list "AA 23" "AA 34"))
