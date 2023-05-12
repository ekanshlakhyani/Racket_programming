;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname a07q1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))


;; (subtract-min lon) produces a list of numbers after subtracting
;;  the minimum number of the given list of numbers lon from every
;;  member of lon.
;; subtract-min:(listof Num) -> (listof Num)

;; Examples:
(check-expect (subtract-min (list 6 3 2 4 1 9 7))
              (list 5 2 1 3 0 8 6))
(check-expect (subtract-min empty)
              empty)

(define (subtract-min lon)
  (local
    [
     ;; (min-list n lon) produces the minimum value when the between the
     ;;  given number n and the list of numbers lon.
     ;; min-list: Num (listof Num) -> Num

     (define (min-list n lon)
       (cond
         [(empty? lon) n]
         [else (min n (min-list (first lon) (rest lon)))]))


     ;; (subtracter n lon) produces a list of numbers after subtracting
     ;;  the number n from every member of the given list of numbers lon. 
     ;; subtracter: Num (listof Num) -> (listof Num)

     (define (subtracter n lon)
       (cond [(empty? lon) empty]
             [else (cons (- (first lon) n) (subtracter n (rest lon)))]))]

    (cond [(empty? lon) empty]
          [else
           (local
             [(define minimum (min-list (first lon) (rest lon)))]
             (subtracter minimum lon))])))   

;; Tests
(check-expect (subtract-min (list 3 3 -1 7.7)) (list 4 4 0 8.7))
(check-expect (subtract-min (build-list 10000 add1)) 
              (build-list 10000 identity))
(check-expect (subtract-min (list 8 3 0 7)) (list 8 3 0 7))
(check-expect (subtract-min (list 9 3 9 8)) (list 6 0 6 5))
(check-expect (subtract-min (list -8 -3 0 -7)) (list 0 5 8 1))
