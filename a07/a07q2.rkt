;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname a07q2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))


;; (lenlen n) produces the LenLen number consuming a natural number n. The
;;  LenLen number can be defined as the sum of- the sum of digits of n,
;;  LenLen number of LenLen(n-1), the number of digits of the LenLen number
;;  of LenLen(n-1), the number of digits of the number of digits of the
;;  LenLen number of LenLen(n-1).
;; lenlen: Nat -> Nat

;; Examples:
(check-expect (lenlen 0) 1)
(check-expect (lenlen 1) 2)
(check-expect (lenlen 4) 18)

(define (lenlen n)
  (local
    [
     ;; (digsum n) produces the sum of digits of the natural number n.
     ;; digsum: Nat -> Nat
     (define (digsum n)
       (cond [(= (quotient n 10) 0) n]
             [else (+ (remainder n 10) (digsum (quotient n 10)))]))


     ;; (len n) produces the number of digits of natural number n.
     ;; len: Nat -> Nat
     (define (len n) (string-length (number->string n)))]

    (cond [(equal? n 0) 1]
          [(equal? n 1) 2]
          [else
           (local
             [(define lenlenn-1 (lenlen (- n 1)))]
             (+ (digsum n)
                lenlenn-1
                (len lenlenn-1)
                (len(len lenlenn-1))))])))

;; Tests
(check-expect (lenlen 123456) 3643889)
(check-expect (lenlen 123457) 3643919)
(check-expect (lenlen 2) 6)
(check-expect (lenlen 3) 11)
