;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname a02q2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))

;; ******************************************
;;   Ekansh Lakhyani (20945086)
;;   CS 115 Fall 2021
;;   Assignment 02, Question 2
;; ******************************************


(define (valid-local? num)
  (cond
    [(and (string-numeric? num)
          (= (string-length num) 10)
          (or (equal? (substring num 0 3) "226")
              (equal? (substring num 0 3) "519")
              (equal? (substring num 0 3) "548"))
          (not (string-contains? "911" num)))
     true]
    [(and (string-numeric? num)
          (= (string-length num) 10)
          (and (not (equal? (substring num 0 1) "0"))
               (not (equal? (substring num 0 1) "1")))
          (not (string-contains? "911" num)))
     false]
    [else 'invalid]))


;; valid-local? takes in string value and if it is a 
;; local phone number then produces true, if it is a
;; a valid phone number but not a local one produces
;; false. If it is an invalid phone number it produces
;; a symbol 'invalid