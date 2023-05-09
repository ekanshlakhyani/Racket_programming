;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname a01q2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;******************************************
;;   Ekansh Lakhyani (20945086)
;;   CS 115 Fall 2021
;;   Assignment 01, Question 2
;;******************************************
(define (circle-angle r1 r2 r3)
  (acos (/ (- (+ (sqr (+ r1 r2))
                   (sqr (+ r1 r3)))
                        (sqr (+ r2 r3)))
                        (* 2 (+ r1 r2) (+ r1 r3)))))