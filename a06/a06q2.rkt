;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname a06q2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))


;; Helper Functions

;; (listofn element n) produces a list of n element, consuming the
;;  natural number n and any type of element. It makes a list which
;;  element repeated n times.
;; listofn: Any Nat -> (listof Any)

;; Examples:
(check-expect (listofn empty 9) empty)
(check-expect (listofn "jelly" 0) empty)
(check-expect (listofn "a" 3) (list "a" "a" "a"))
(check-expect (listofn 'jk 2) (list 'jk 'jk ))
(check-expect (listofn 100 1) (list 100 ))

(define (listofn element n)
  (cond [(empty? element) empty]
        [(zero? n) empty]
        [else (cons element (listofn element (sub1 n)))]))


;; (help-expand lst n): produces an expanded list of the given list
;;  lst, making a list of 1 element on the first position of the
;;  element on the first position, a list of 2 on second positon of
;;  the element on the second positon and so on. The natural number n
;;  is the number position we are starting from(for this question n
;;  will always be one for the starting case)
;; help-expand: (listof Any) Nat -> (listof (listof Any))

;; Examples:
(check-expect (help-expand (list true 115 "a") 1)
              (list (list true) (list 115 115) (list "a" "a" "a")))
(check-expect (help-expand empty 1) empty)

(define (help-expand lst n)
  (cond [(empty? lst) empty]
        [else (cons (listofn (first lst) n)
                    (help-expand (rest lst) (add1 n)))]))


;; Main Function

;; (expand lst) produces an expanded list of the given list lst,
;;  making a list of 1 element on the first position of the element
;;  on the first position, a list of 2 on second positon of the
;;  element on the second positon and so on.
;; expand: (listof Any) -> (listof (listof Any))

;; Examples:
(check-expect (expand (list true 115 "a"))
              (list (list true) (list 115 115) (list "a" "a" "a")))
(check-expect (expand empty) empty)

(define (expand lst) (help-expand lst 1))

;; Tests
(check-expect (expand (list 'a 'b 'c))
              (list (list 'a) (list 'b 'b) (list 'c 'c 'c)))
(check-expect (expand (list 500)) (list (list 500)))
(check-expect (expand (list 1 2 3 4 5 6))
              (list (list 1) (list 2 2) (list 3 3 3) (list 4 4 4 4)
                    (list 5 5 5 5 5) (list 6 6 6 6 6 6)))
(check-expect (expand (list false false))
              (list (list false) (list false false)))
