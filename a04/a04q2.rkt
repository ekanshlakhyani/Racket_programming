;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname a04q2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;; ******************************************
;;   Ekansh Lakhyani (20945086)
;;   CS 115 Fall 2021
;;   Assignment 04, Question 2
;; ******************************************
;;


;; Helper functions

;; (donorA+ b) produces true if the blood tybe b could be a donor to
;;  blood type A+ and false if not.
;; donorA+: Sym -> Bool
;; Requires: A symbol for one of the blood-types i.e. one of the
;;  following symbols- 'AB+,'AB-,'A+,'A-,'B+,'B-,'O+ or 'O-

;; Examples:
(check-expect (donorA+? 'A-) true)
(check-expect (donorA+? 'O+) true)
(check-expect (donorA+? 'AB-) false)

(define (donorA+? b)
  (cond [(symbol=? b 'A-) true]
        [(symbol=? b 'O+) true]
        [else false]))


;; (donorB+ b) produces true if the blood tybe b could be a donor to
;;  blood type B+ and false if not.
;; donorB+: Sym -> Bool
;; Requires: A symbol for one of the blood-types i.e. one of the
;;  following symbols- 'AB+,'AB-,'A+,'A-,'B+,'B-,'O+ or 'O-

;; Examples:
(check-expect (donorB+? 'B-) true)
(check-expect (donorB+? 'A+) false)

(define (donorB+? b)
  (cond [(symbol=? b 'B-) true]
        [(symbol=? b 'O+) true]
        [else false]))


;; (donorAB- b) produces true if the blood tybe b could be a donor to
;;  blood type AB- and false if not.
;; donorAB-: Sym -> Bool
;; Requires: A symbol for one of the blood-types i.e. one of the
;;  following symbols- 'AB+,'AB-,'A+,'A-,'B+,'B-,'O+ or 'O-

;; Examples:
(check-expect (donorAB-? 'A-) true)
(check-expect (donorAB-? 'AB+) false)

(define (donorAB-? b)
  (cond [(symbol=? b 'A-) true]
        [(symbol=? b 'B-) true]
        [else false]))


;; (compatible? bt blood-type) produces true if the blood type bt
;;  could be a donor to blood type blood-type and false if not.
;; compatible?: Sym Sym -> Bool
;; Requires: 2 symbols of the blood-types i.e. one of the
;;  following symbols- 'AB+,'AB-,'A+,'A-,'B+,'B-,'O+ or 'O-

;; Examples:
(check-expect (compatible? 'O- 'O+) true)
(check-expect (compatible? 'AB+ 'A-) false)
(check-expect (compatible? 'B+ 'B+) true)

(define (compatible? bt blood-type)
  (cond [(symbol=? bt blood-type) true]
        [(symbol=? bt 'O- ) true]
        [(symbol=? blood-type 'AB+) true]
        [(and (symbol=? 'A+ blood-type)
              (donorA+? bt)) true]
        [(and (symbol=? 'B+ blood-type)
              (donorB+? bt)) true]
        [(and (symbol=? 'AB- blood-type)
              (donorAB-? bt)) true]
        [else false]))


;; Main Function

;; (compatible-litres blood-bank blood-type) produces the number of 
;;  litres of blood available which is compatible with the blood type
;;  in symbol blood-type in the list of symbols blood-bank.
;; compatible-litres: (listof Sym) Sym -> Num
;; Requires: A list of symbols and a symbol, which are one of the
;;  following symbols- 'AB+,'AB-,'A+,'A-,'B+,'B-,'O+ or 'O-

;;Examples
(check-expect (compatible-litres 
     (cons 'O- (cons 'O+ (cons 'O- (cons 'B- (cons 'AB- empty))))) 
     'AB-) 2)
(check-expect (compatible-litres (cons 'A+ (cons 'B+ empty)) 'A-) 0)
(check-expect (compatible-litres
               (cons 'A+ (cons 'B+ (cons 'AB+ empty))) 'AB+) 1.5)

(define (compatible-litres blood-bank blood-type)
  (cond [(empty? blood-bank) 0]
        [else
         (+ (cond [(compatible? (first blood-bank) blood-type) 0.5]
                  [else 0])
            (compatible-litres (rest blood-bank) blood-type))]))

;; Tests
(check-expect (compatible-litres 
     (cons 'A- (cons 'A+ (cons 'B+ (cons 'B- (cons 'AB- empty))))) 
     'B-) 0.5)
(check-expect (compatible-litres 
     (cons 'A- (cons 'A+ (cons 'B+ (cons 'B- (cons 'O+ empty))))) 
     'A+) 1.5)
(check-expect (compatible-litres 
     (cons 'O- (cons 'B+ (cons 'B+ (cons 'B- (cons 'O+ empty))))) 
     'B+) 2.5)
(check-expect (compatible-litres 
     (cons 'A- (cons 'B+ (cons 'B+ (cons 'B- (cons 'O+ empty))))) 
     'O-) 0)
(check-expect (compatible-litres 
     (cons 'O+ (cons 'B+ (cons 'O- (cons 'B- (cons 'O+ empty))))) 
     'O+) 1.5)