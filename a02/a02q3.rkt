;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname a02q3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))




(define bloc-string "Bloc Québécois")
(define cons-string "Conservative Party of Canada")
(define green-string "Green Party of Canada")
(define lib-string "Liberal Party of Canada")
(define ndp-string "New Democratic Party")
(define ppc-string "People's Party of Canada")

(define majority ": MAJORITY")
(define minority ": MINORITY")


(define (half bloc cons green lib ndp ppc)
  (/ (+ bloc cons green lib ndp ppc) 2))


(define (max-party bloc cons green lib ndp ppc)
  (cond
    [(equal? (max bloc cons green lib ndp ppc) bloc)
     bloc-string]
    [(equal? (max bloc cons green lib ndp ppc) cons)
     cons-string]
    [(equal? (max bloc cons green lib ndp ppc) green)
     green-string]
    [(equal? (max bloc cons green lib ndp ppc) lib)
     lib-string]
    [(equal? (max bloc cons green lib ndp ppc) ndp)
     ndp-string]
    [(equal? (max bloc cons green lib ndp ppc) ppc)
     ppc-string]))


(define (election-result bloc cons green lib ndp ppc)
  (cond
    [(< (half bloc cons green lib ndp ppc)
        (max bloc cons green lib ndp ppc))
     (string-append
      (max-party bloc cons green lib ndp ppc) majority)]
    [else
     (string-append
      (max-party bloc cons green lib ndp ppc) minority)]))


;;  election-result takes in the 6 numbers which are the seats won by
;;  the 6 major parties of Canada they are in order
;;  bloc cons green lib ndp ppc. And produces the winning party 
;;  with the type of win i.e. majority or minority.

