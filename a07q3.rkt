;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname a07q3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ******************************************
;;   Ekansh Lakhyani (20945086)
;;   CS 115 Fall 2021
;;   Assignment 07, Question 3
;; ******************************************
;;


;; Constans from the question

;; A Point is a (list Num Num)
  
;; make-point: Num Num -> Point
(define (make-point x y) (list x y))
;; point-x: Point -> Num
(define (point-x pt) (first pt))
;; point-y: Point -> Num
(define (point-y pt) (second pt))

;; A Rectangle is (list Point Point)

;; make-rect: Point Point -> Rectangle
;; Requires:  
;;  * (point-x bottom-left) < (point-x top-right)
;;  * (point-y bottom-left) < (point-y top-right)
(define
  (make-rect bottom-left top-right) (list bottom-left top-right))
;; bottom-left: Rectangle -> Point
(define (bottom-left r) (first r))
;; top-right: Rectangle -> Point
(define (top-right r) (second r))

;; Constants:

(define origin (make-point 0 0))


;; (rectangle-sort rectangles) consumes a list of rectangles and sorts
;;  them according to their areas, from smallest to largest. If two
;;  rectangles have the same area, use the following tie-breaking rules,
;;  in order, moving onto the next rule only when necessary:
;;  1. pick the rectangle that has its bottom-left point is closest to the
;;  origin (0,0).
;;  2. pick the rectangle that has its top-right point is closest to the
;;  origin (0,0).
;;  3. pick the rectangle that has the minimum y value in the bottom-left
;;  corner.
;;  4. pick the rectangle that has the minimum y value in the top-right
;;  corner.
;; rectangle-sort: (listof (list (list Num Num) (list Num Num))) ->
;;                 (listof (list (list Num Num) (list Num Num)))

;; Examples:
(check-expect(rectangle-sort 
              (list 
               (make-rect (make-point 0 0) (make-point 2 4)) 
               (make-rect (make-point 3 3) (make-point 4 4)) 
               (make-rect (make-point 0 0) (make-point 4 2))
               (make-rect (make-point 1 1) (make-point 2 2))))
             (list (list (list 1 1) (list 2 2)) 
                   (list (list 3 3) (list 4 4)) 
                   (list (list 0 0) (list 4 2)) 
                   (list (list 0 0) (list 2 4))))
(check-expect(rectangle-sort empty)  empty)
(define (rectangle-sort rectangles)
  (local
    [;; (distance P Q) produces the distance between point P and Q on a
     ;;  plane, consuming 2 list of 2 numbers representing points P and Q.
     ;; distance: (list Num Num) (list Num Num) -> Num
     
     (define (distance P Q)
       (sqrt (+ (sqr (- (point-x P) (point-x Q)))
                (sqr (- (point-y P) (point-y Q))))))


     ;; (area make-rect) produces the area of the rectangle make-rect
     ;;  which is a list of 2 points which are a list of 2 Numbers.
     ;; area: (list (list Num Num) (list Num Num)) -> Num
     
     (define (area make-rect)
       (* (- (point-x (top-right make-rect))
             (point-x (bottom-left make-rect)))
          (- (point-y (top-right make-rect))
             (point-y (bottom-left make-rect)))))


     ;; (area<? Rectangle-1 Rectangle-2) produces true if the area of
     ;;  Rectangle1 is less than the area of Rectangle2 and false
     ;;  otherwise.
     ;; area<?: (list (list Num Num) (list Num Num))
     ;;         (list (list Num Num) (list Num Num)) -> Bool
     
     (define (area<? Rectangle-1 Rectangle-2)
       (< (area Rectangle-1) (area Rectangle-2)))


     ;; (area>? Rectangle-1 Rectangle-2) produces true if the area of
     ;;  Rectangle1 is more than the area of Rectangle2 and false
     ;;  otherwise.
     ;; area>?: (list (list Num Num) (list Num Num))
     ;;         (list (list Num Num) (list Num Num)) -> Bool
     
     (define (area>? Rectangle-1 Rectangle-2)
       (> (area Rectangle-1) (area Rectangle-2)))


     ;; (correct-sort? Rectangle-1 Rectangle-2) poduces true if the
     ;;  Rectangle1 and Rectangke 2 are sorted corectly according to the
     ;;  criteria below and false otherwise-
     ;;  according to their areas, from smallest to largest. If two
     ;;  rectangles have the same area, use the following tie-breaking
     ;;  rules, in order, moving onto the next rule only when necessary:
     ;;  1. pick the rectangle that has its bottom-left point is closest
     ;;  to the origin (0,0).
     ;;  2. pick the rectangle that has its top-right point is closest to
     ;;  the origin (0,0).
     ;;  3. pick the rectangle that has the minimum y value in the
     ;;  bottom-left corner.
     ;;  4. pick the rectangle that has the minimum y value in the
     ;;  top-right corner.
     ;; correct-sort?: (list (list Num Num) (list Num Num))
     ;;                (list (list Num Num) (list Num Num)) -> Bool
     
     (define (correct-sort? Rectangle-1 Rectangle-2)
       (cond [(area<? Rectangle-1 Rectangle-2) true]
             [(area>? Rectangle-1 Rectangle-2) false]
             [(< (distance (bottom-left Rectangle-1) origin)
                 (distance (bottom-left Rectangle-2) origin)) true]
             [(> (distance (bottom-left Rectangle-1) origin)
                 (distance (bottom-left Rectangle-2) origin)) false]
             [(< (distance (top-right Rectangle-1) origin)
                 (distance (top-right Rectangle-2) origin)) true]
             [(> (distance (top-right Rectangle-1) origin)
                 (distance (top-right Rectangle-2) origin)) false]
             [(< (point-y (bottom-left Rectangle-1))
                 (point-y (bottom-left Rectangle-2))) true]
             [(> (point-y (bottom-left Rectangle-1))
                 (point-y (bottom-left Rectangle-2))) false]
             [(< (point-y (top-right Rectangle-1))
                 (point-y (top-right Rectangle-2))) true]
             [(> (point-y (top-right Rectangle-1))
                 (point-y (top-right Rectangle-2))) false]
             [else true]))]

    (sort rectangles correct-sort?)))
  
;; Tests

(check-expect(rectangle-sort 
              (list 
               (make-rect (make-point 0 0) (make-point 2 4)) 
               (make-rect (make-point 3 3) (make-point 4 4)) 
               (make-rect (make-point 0.1 0) (make-point 4 2))
               (make-rect (make-point 1 1) (make-point 2 2))))
             (list (list (list 1 1) (list 2 2)) 
                   (list (list 3 3) (list 4 4)) 
                   (list (list 0.1 0) (list 4 2)) 
                   (list (list 0 0) (list 2 4))))
(check-expect(rectangle-sort (list (make-rect (make-point 3 3)
                                              (make-point 4 4))
                                   (make-rect (make-point 3 3)
                                              (make-point 4 4))))
                             (list (list (list 3 3) (list 4 4))
                                   (list (list 3 3) (list 4 4))))
(check-expect(rectangle-sort (list (make-rect (make-point 0 0)
                                              (make-point 2 4))))
                             (list (list (list 0 0) (list 2 4))))