;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname a08q3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; ******************************************
;;   Ekansh Lakhyani (20945086)
;;   CS 115 Fall 2021
;;   Assignment 08, Question 3
;; ******************************************
;;


(define-struct course (code number grades))
;; A Course is a (make-course Str Nat (listof (anyof Num 'DNW))
;; Requires: number > 0
;; the numbers in grades are in the range 0 to 100 inclusive
;; there is at least one number in grades
  
(define-struct summary (code number mean median failures))
;; A Summary is a (make-summary Str Nat Num Num Nat)


;; (calculate-stats course) produces a summary which contains the code
;;  , number, mean, median and faliures given the course which contains
;;  code, number and the list of grades.
;; calculate-stats: course Str Nat (listof (anyof Num 'DNW)
;;  -> summary Str Nat Num Num Nat

;; Examples:
(check-expect (calculate-stats (make-course "CS" 115 empty))
              (make-summary "CS" 115 0 0 0))
(check-expect
 (calculate-stats (make-course "CS" 115 (list 100 'DNW 30 0 75)))
 (make-summary "CS" 115 51.25 52.5 3))
(check-expect
 (calculate-stats (make-course "ECON" 101 (list 60 70 50 90)))
 (make-summary "ECON" 101 67.5 65 0))
(check-expect
 (calculate-stats (make-course "MATH" 135 (list 76.5 0 91.5)))
 (make-summary "MATH" 135 56 76.5 1))


(define (calculate-stats course)
  (local
    [;; (mean lst-grades) produces the mean of the given list of grades
     ;;  lst-grades.
     ;; mean: (listof (anyof Num 'DNW) -> Num
     (define (mean lst-grades)
      (cond [(empty? lst-grades) 0]
            [else (/ (foldr + 0 (filter number? lst-grades))
          (length (filter number? lst-grades)))]))

     ;; (floor-half-length lst-grades) produces the floor of the half of
     ;;  the length of the lst-grades which is the list of grades.
     ;; floor-half-length: (listof (anyof Num 'DNW) -> Nat
     (define (floor-half-length lst-grades)
       (floor (/ (length (filter number? lst-grades)) 2)))

     ;; (median lst-grades) produces the median of the given list of grades
     ;;  lst-grades.
     ;; median: (listof (anyof Num 'DNW) -> Num
     (define (median lst-grades)
       (cond
         [(empty? lst-grades) 0]
         [(odd? (length (filter number? lst-grades)))
              (list-ref
               (sort (filter number? lst-grades) <)
               (floor-half-length lst-grades))]
             [else
              (mean
               (list
                (list-ref (sort (filter number? lst-grades) <)
                          (floor-half-length lst-grades))
                (list-ref (sort (filter number? lst-grades) <)
                          (sub1 (floor-half-length lst-grades)))))]))

     ;; (fail? grade) produces true if the grade is below 50 that is a
     ;;  failing grade and produces false otherwise.
     ;; fail?: Num -> Bool
     (define (fail? grade)
       (cond [(equal? 'DNW grade) true]
             [(> 50 grade) true]
             [else false]))

     ;; (num-fails lst-grades) produces the number of failing grades given
     ;;  the list of grades.
     ;; num-fails: (listof (anyof Num 'DNW) -> Nat
     (define (num-fails lst-grades)
       (length (filter fail? lst-grades)))]


    (make-summary (course-code course)
                  (course-number course)
                  (mean (course-grades course))
                  (median (course-grades course))
                  (num-fails (course-grades course)))))

;; Tests
(check-expect
 (calculate-stats (make-course "ENG" 101 (list 100 'DNW 30 0 75)))
 (make-summary "ENG" 101 51.25 52.5 3))
(check-expect
 (calculate-stats (make-course "ECON" 101 (list 100)))
 (make-summary "ECON" 101 100 100 0))
(check-expect
 (calculate-stats (make-course "" 0 (list 76.5 0 91.5)))
 (make-summary "" 0 56 76.5 1))
(check-expect
 (calculate-stats (make-course "" 0 (list 0 0 0 0 0 0 0)))
 (make-summary "" 0 0 0 7))
(check-expect
 (calculate-stats (make-course "D" 99 (list 88 88)))
 (make-summary "D" 99 88 88 0))
(check-expect
 (calculate-stats (make-course "S" 909 (list 44 88)))
 (make-summary "S" 909 66 66 1))
(check-expect
 (calculate-stats (make-course "" 0 empty))
 (make-summary "" 0 0 0 0))



