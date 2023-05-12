;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname a03q3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))




;; constants for the months of the year and symbols used in
;;  date formats 
(define Jan "January")
(define Feb "February")
(define Mar "March")
(define Apr "April")
(define May "May")
(define Jun "June")
(define Jul "July")
(define Aug "August")
(define Sep "September")
(define Oct "October")
(define Nov "November")
(define Dec "December")
(define space " ")
(define c-space ", ")
(define dash "-")
(define f-slash "/")


;; (word-month month) produces the month in word format from the
;;  numbers given in the given string.
;; word-month: Str -> Str
;; Requires: month in MM format 

;; Examples:
(check-expect (word-month "01") "January")
(check-expect (word-month "02") "February")

(define (word-month month)
  (cond [(string=? month "01") Jan]
        [(string=? month "02") Feb]
        [(string=? month "03") Mar]
        [(string=? month "04") Apr]
        [(string=? month "05") May]
        [(string=? month "06") Jun]
        [(string=? month "07") Jul]
        [(string=? month "08") Aug]
        [(string=? month "09") Sep]
        [(string=? month "10") Oct]
        [(string=? month "11") Nov]
        [else Dec]))


;; (full-year  YY) produces the full year, adds 20 before the given
;;  year string.
;; full-year: Str -> Str
;; Requires: year in YY format

;; Examples:
(check-expect (full-year "21") "2021")
(check-expect (full-year "20") "2020")
              
(define (full-year YY)
  (string-append "20" YY))


;; (new-date DD) produces date in single digit if there is a zero in
;;  tens's digit otherwise produces as it is.
;; new-date: Str -> Str
;; Requires: date in DD format

;; Examples:
(check-expect (new-date "09") "9")
(check-expect (new-date "15") "15")

(define (new-date DD)
  (cond [(string=? (string-ith DD 0) "0") (string-ith DD 1)]
        [else DD]))


;; (nice-date date) produces date in word format.
;; nice-date: Str -> Str
;; Requires: date in one of the 5 formats:
;;  "YY/MM/DD", "YYYY/MM/DD", "DD-MM-YYYY",
;;  "DDMMYYYY", "Month D, YYYY"

;; Examples:
(check-expect (nice-date "21/09/25") "September 25, 2021")
(check-expect (nice-date "November 4, 1819") "November 4, 1819")
(check-expect (nice-date "1921/05/05") "May 5, 1921")
(check-expect (nice-date "01052012") "May 1, 2012")

(define (nice-date date)
  (cond
    [(string=? (string-ith date 2) f-slash)
     (string-append (word-month (substring date 3 5))
                    space
                    (new-date (substring date 6 8))
                    c-space
                    (full-year (substring date 0 2)))]
    [(string=? (string-ith date 4) f-slash)
     (string-append (word-month (substring date 5 7))
                    space
                    (new-date (substring date 8 10))
                    c-space
                    (substring date 0 4))]
    [(string=? (string-ith date 2) dash)
     (string-append (word-month (substring date 3 5))
                    space
                    (new-date (substring date 0 2))
                    c-space
                    (substring date 6 10))]
    [(string-numeric? date)
     (string-append (word-month (substring date 2 4))
                    space
                    (new-date (substring date 0 2))
                    c-space
                    (substring date 4 8))]
    [else date]))

;; Tests:
(check-expect (nice-date "20/02/11") "February 11, 2020")
(check-expect (nice-date "22/11/01") "November 1, 2022")
(check-expect (nice-date "2020/02/11") "February 11, 2020")
(check-expect (nice-date "2022/11/01") "November 1, 2022")
(check-expect (nice-date "11-02-2020") "February 11, 2020")
(check-expect (nice-date "01-11-2022") "November 1, 2022")
(check-expect (nice-date "11022020") "February 11, 2020")
(check-expect (nice-date "01112022") "November 1, 2022")
(check-expect (nice-date "January 1, 2021") "January 1, 2021")
