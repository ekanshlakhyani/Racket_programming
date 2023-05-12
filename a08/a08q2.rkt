;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname a08q2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))



;; (encode message)  produces a string which is encrypted according the
;;  following procedure: 
;;  Only the letters from the original message will be encrypted. Other
;;  characters such as spaces, punctuation, numeric digits, etc. are not
;;  included in the encrypted message at all.
;;  The encrypted message will only contain uppercase letters.
;;  Each letter will be replaced by the uppercase letter at the opposite
;;  end of the English alphabet. 
;;  In other words, A or a will become Z, B or b will become Y, C or c
;;  will become X, ..., Z or z will become A.
;;  The encrypted message will have a single space separating each letter.
;; encode: Str -> Str
;; Requires: The string message should contain english letters only.

;; Examples:
(check-expect (encode "") "")
(check-expect (encode " ") "")
(check-expect (encode "Cat") "X Z G")
(check-expect (encode "A") "Z")
(check-expect (encode "Is this correct?") "R H G S R H X L I I V X G")

(define (encode message)
  (local
    [;; new-message is the string which which produces the encoded message
     ;;  but with an extra space at the end.
     (define new-message
            (foldr
             (lambda (char x) (string-append (string char) " " x)) ""
             (map integer->char
                  (map
                   (lambda (int-char)
                     (- 91 (- int-char 64)))
                   (map char->integer 
                        (map char-upcase
                             (filter char-alphabetic?
                                     (string->list message))))))))]
    (cond [(string-whitespace? message) ""]
          [(empty? (filter char-alphabetic? (string->list message))) ""]
          [else (substring new-message 0
                           (sub1 (string-length new-message)))])))

;; Tests
(check-expect (encode "A BCD -") "Z Y X W")
(check-expect (encode "  ? ") "")
(check-expect (encode "   ") "")
(check-expect (encode "BBBBBBBB") "Y Y Y Y Y Y Y Y")
(check-expect (encode "?    ?   ? ") "")
(check-expect (encode "B ?   ? ") "Y")




