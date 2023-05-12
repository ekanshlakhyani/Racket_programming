;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname a04q3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))


;; Helper functions

;; (convert digit) produces the numeric character according to the 
;;  alphabetic character given. Otherwise just produces the character
;;  as it is. 
;; convert: Char -> Char
;; Requires: A character which is an alphabet, number or one of the
;;  following character symbols: +, -, (, ) and space symbol   

;; Examples:
(check-expect (convert #\e) #\3)
(check-expect (convert #\5) #\5)
(check-expect (convert #\ ) #\ )

(define (convert digit)
  (cond [(or (char-ci=? #\A digit) (char-ci=? #\B digit)
             (char-ci=? #\C digit)) #\2]
        [(or (char-ci=? #\D digit) (char-ci=? #\E digit)
             (char-ci=? #\F digit)) #\3]
        [(or (char-ci=? #\G digit) (char-ci=? #\H digit)
             (char-ci=? #\I digit)) #\4]
        [(or (char-ci=? #\J digit) (char-ci=? #\K digit)
             (char-ci=? #\L digit)) #\5]
        [(or (char-ci=? #\M digit) (char-ci=? #\N digit)
             (char-ci=? #\O digit)) #\6]
        [(or (char-ci=? #\P digit) (char-ci=? #\Q digit) 
             (char-ci=? #\R digit) (char-ci=? #\S digit)) #\7]
        [(or (char-ci=? #\T digit) (char-ci=? #\U digit)
             (char-ci=? #\V digit)) #\8]
        [(or (char-ci=? #\W digit) (char-ci=? #\X digit)
             (char-ci=? #\Y digit) (char-ci=? #\Z digit)) #\9]
        [else digit]))


;; (convert-lst lst) produces the list of numeric and symbol   
;;  characters according to the given list of characters. It changes
;;  the alphabetic characters to numeric characters and leaves the
;;  numeric characters and symbol characters as it is.
;; convert-lst: (listof Char) -> (listof Char)
;; Requires: A list of characters which are alphabets, numbers or one
;;  of the following symbols: +, -, (, ) and space symbol   

;; Examples:
(check-expect (convert-lst (cons #\a (cons #\1 empty)))
              (cons #\2 (cons #\1 empty)))
(check-expect (convert-lst (cons #\h (cons #\8 empty)))
              (cons #\4 (cons #\8 empty)))
(check-expect (convert-lst (cons #\- (cons #\9 empty)))
              (cons #\- (cons #\9 empty)))

(define (convert-lst lst)
  (cond
    [(empty? lst) empty]
    [else (cons (convert (first lst)) 
                (convert-lst (rest lst)))]))


;; (remove-unwanted lst1) produces the list of numeric characters   
;;  according to the given list of characters. It removes
;;  the symbol characters and leaves just the numeric characters.
;; remove-unwanted: (listof Char) -> (listof Char)
;; Requires: A list of characters which are numbers or one
;;  of the following symbols: +, -, (, ) and space symbol

;; Examples:
(check-expect (remove-unwanted (cons #\4 (cons #\- empty)))
                               (cons #\4 empty))
(check-expect (remove-unwanted (cons #\1 (cons #\+ empty)))
                               (cons #\1 empty))
(check-expect
 (remove-unwanted (cons #\6 (cons #\space (cons #\- empty))))
                               (cons #\6 empty))

(define (remove-unwanted lst1)
  (cond
    [(empty? lst1) empty]
    [(char-numeric? (first lst1)) 
     (cons (first lst1) (remove-unwanted (rest lst1)))]
    [else (remove-unwanted (rest lst1))]))


;; Main Function

;; (telephone-digits phone-number) produces a string of numbers given
;;  a string containing numbers, alphabets and symbols. It converts
;;  the alphabets to numbers in a set pattern and removes the symbols.
;; telephone-digits: Str -> Str
;; Requires: A string containing numbers, alphabets or one
;;  of the following symbols: +, -, (, ) and space symbol

;; Examples:
(check-expect (telephone-digits "(519) 888-4567")
              "5198884567")
(check-expect (telephone-digits "1-800-COMPUTE")
              "18002667883")
(check-expect (telephone-digits "callme")
              "225563" )
(check-expect (telephone-digits "+86 10 5139 4000")
              "861051394000")
(check-expect (telephone-digits "1)23---S4-   8")
              "123748")

(define (telephone-digits phone-number)
  (list->string
   (remove-unwanted (convert-lst (string->list phone-number)))))

;; Tests
(check-expect (telephone-digits "123 3")
              "1233")
(check-expect (telephone-digits "adgjmptw")
              "23456789")
(check-expect (telephone-digits "abcdefghijklmno")
              "222333444555666")
(check-expect (telephone-digits "1-2(3)4/5 6 7 +8 90")
              "1234567890")
(check-expect (telephone-digits "1+2-3 0")
              "1230")
(check-expect (telephone-digits "")
              "")
(check-expect (telephone-digits " ")
              "")
(check-expect (telephone-digits "mnopqrstuvwxyz")
              "66677778889999")
(check-expect (telephone-digits "AZAZAZAZ ")
              "29292929")
