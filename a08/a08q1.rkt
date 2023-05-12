;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname a08q1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))



(define (compatible-litres blood-bank blood-type)
  (local [(define (add0.5 x) (+ 0.5 x))
          (define (add0 x) x)


          (define (possible-donor? donor recipient)
            (or (symbol=? donor recipient)
                (symbol=? donor 'O-)
                (symbol=? recipient 'AB+)
                (and (symbol=? donor 'B-) (symbol=? recipient 'AB-))
                (and (symbol=? donor 'A-) (symbol=? recipient 'AB-))
                (and (symbol=? donor 'O+) (symbol=? recipient 'A+))
                (and (symbol=? donor 'A-) (symbol=? recipient 'A+))
                (and (symbol=? donor 'O+) (symbol=? recipient 'B+))
                (and (symbol=? donor 'B-) (symbol=? recipient 'B+))))]

    (local
      [(define bt blood-type)]

      (local


        [(define (donor? donor)
           (cond [(possible-donor? donor bt) true]
                 [else false]))]


        (* 0.5 (length (filter donor? blood-bank)))
        ))))

(check-expect (compatible-litres (list 'O- 'O+ 'O- 'B- 'AB-) 'AB-) 2)
(check-expect (compatible-litres (list 'A+ 'B+) 'A-) 0)
(check-expect (compatible-litres (list 'A+ 'B+ 'AB+) 'AB+) 1.5)
