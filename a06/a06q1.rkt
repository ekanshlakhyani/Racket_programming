;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname a06q1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ******************************************
;;   Ekansh Lakhyani (20945086)
;;   CS 115 Fall 2021
;;   Assignment 06, Question 1
;; ******************************************
;;


;; Constants (example)
(define rolling-stone-2021
    (list "Respect"
          "Fight the Power"
          "A Change Is Gonna Come"
          "Like a Rolling Stone"
          "Smells Like Teen Spirit"	
          "What's Going On"	
          "Strawberry Fields Forever"	
          "Get Ur Freak On"	
          "Dreams"	
          "Hey Ya!"))
(define rolling-stone-2021-tweaked
    (list "Respect"
          "Hey Jude"
          "Fight the Power"
          "A Change Is Gonna Come"
          "Smells Like Teen Spirit"	
          "Like a Rolling Stone"
          "What's Going On"	
          "Strawberry Fields Forever"	
          "Get Ur Freak On"	
          "Dreams"))


;; Main Function

;; (same-ranking los1 los2) produce the list of all the songs which
;;  have the same rank in both lists- los1 and los2.
;; same-ranking: (listof Str) (listof Str) -> (listof Str)
;; Requires: list of string los1 and list of string los2 have the
;;  same length

;; Examples:
(check-expect
 (same-ranking rolling-stone-2021 rolling-stone-2021-tweaked)
 (list "Respect" "Smells Like Teen Spirit"))
(check-expect
 (same-ranking (list "sugar" "animals" "sing")
               (list "sugar" "animals" "sing"))
 (list "sugar" "animals" "sing"))

(define (same-ranking los1 los2)
  (cond
    [(empty? los1) empty]
    [(equal?
      (first los1) (first los2))
     (cons (first los1)
           (same-ranking (rest los1) (rest los2)))]
    [else (same-ranking (rest los1) (rest los2))]))

;; Tests
(check-expect
 (same-ranking empty empty) empty)
(check-expect
 (same-ranking (list "sugar" "animals" "sing")
               (list "sing" "animals" "sugar"))
 (list "animals"))
(check-expect
 (same-ranking (list "sugar" "rose" "blank" "red")
               (list "sugar" "pink" "sing" "best"))
 (list "sugar"))
