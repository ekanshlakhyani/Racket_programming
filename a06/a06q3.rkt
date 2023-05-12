;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname a06q3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))


;; Helper Functions

;; (prev-vacc-check? full-vacc-lst new-vacc) produces either true or
;;  false according to if the name in the list new-vacc is present
;;  in any list of list full-vac-lst
;; prev-vacc-check: (listof (list Str (listof Str))) (list Str Str)
;;                     -> Bool
;; Requires: full-vacc-lst be sorted by name in alphabetical order
;;  with no duplication of names.

;; Examples:
(check-expect
 (prev-vacc-check?
  (list (list "Graham"
              (list "May 15, 2021" "June 25, 2021"))
        (list "Kaplan"
              (list "June 1, 2021" "July 1, 2021"))
        (list "Vasiga"
              (list "July 10, 2021")))
  (list "Pretti" "July 17, 2021"))
 false)
(check-expect
 (prev-vacc-check?
  (list (list "Graham"
              (list "May 15, 2021" "June 25, 2021"))
        (list "Kaplan"
              (list "June 1, 2021" "July 1, 2021"))
        (list "Vasiga"
              (list "July 10, 2021")))
  (list "Vasiga" "July 22, 2021"))
 true)

(define (prev-vacc-check? full-vacc-lst new-vacc)
  (cond [(empty? full-vacc-lst) false]
        [(empty? new-vacc) false]
        [(equal? (first (first full-vacc-lst)) (first new-vacc)) true]
        [else (prev-vacc-check? (rest full-vacc-lst) new-vacc)]))


;; (new-vacc-format new-vacc): produced the changed list new-vacc to
;;  the new format which is the format in which it should be added in
;;  the full-vacc-lst or the updated full-vacc-lst.
;; new-vacc-format: (list Str Str) -> (list Str (list Str))
;; Requires: * full-vacc-lst be sorted by name in alphabetical order
;;  with no duplication of names.
;;  * name in new-vacc is not present in full-vacc-lst.

;; Examples:
(check-expect (new-vacc-format (list "Pretti" "July 17, 2021"))
              (list "Pretti" (list "July 17, 2021")))
(check-expect (new-vacc-format (list "Mike" "July 7, 2021"))
              (list "Mike" (list "July 7, 2021")))

(define (new-vacc-format new-vacc)
  (list (first new-vacc) (rest new-vacc)))
 

;; (new-vacc-lst full-vacc-lst new-vaccs) produces the list of the
;;  vaccinations after changing its format to the one in which it
;;  should be added in full-vacc-lst or the updated full-vacc-lst.
;; new-vacc-lst: (listof (list Str (listof Str)))
;;  (listof (list Str Str)) -> (listof (list Str (list Str)))
;; Requires: full-vacc-lst and new-vaccs are sorted by alphabetical
;;  order of the names and with no duplication of names individually
;;  in both the lists.

;; Examples:
(check-expect
 (new-vacc-lst
  (list (list "Graham"
              (list "May 15, 2021" "June 25, 2021"))
        (list "Kaplan" (list "June 1, 2021" "July 1, 2021"))
        (list "Vasiga" (list "July 10, 2021")))
  (list (list "Pretti" "July 17, 2021")
        (list "Vasiga" "July 22, 2021")))
 (list (list "Pretti" (list "July 17, 2021"))))
(check-expect (new-vacc-lst empty
                            (list (list "Pretti" "July 17, 2021")
                                  (list "Vasiga" "July 22, 2021")))
              (list (list "Pretti" (list "July 17, 2021"))
                    (list "Vasiga" (list "July 22, 2021"))))
(check-expect (new-vacc-lst empty empty) empty)
(check-expect (new-vacc-lst (list (list "Graham"
              (list "May 15, 2021" "June 25, 2021"))
        (list "Kaplan" (list "June 1, 2021" "July 1, 2021"))
        (list "Vasiga" (list "July 10, 2021"))) empty) empty)

(define (new-vacc-lst full-vacc-lst new-vaccs)
  (cond 
    [(empty? new-vaccs) empty]
    [(not (prev-vacc-check? full-vacc-lst (first new-vaccs)))
     (cons (new-vacc-format (first new-vaccs))
           (new-vacc-lst full-vacc-lst (rest new-vaccs)))]
    [else (new-vacc-lst full-vacc-lst (rest new-vaccs))]))


;; (prev-vacc-add full-vacc-lst last-vacc) produces the list of the
;;  name and list of dates with added date from the same name from
;;  both the full-vacc-lst and last-vacc. The format of the list
;;  produced is as per the full-vacc-lst.
;;  the second dose date from the 
;; prev-vacc-add: (listof (list Str (listof Str)))
;;                    -> (list Str (listof Str))
;; Requires: * full-vacc-lst is sorted by alphabetical order of the
;;  names and there is no duplication of names.
;;  * the name in last-vacc is present in full-vacc-lst

;; Examples:
(check-expect (prev-vacc-add
               (list
                (list "Graham" (list "May 15, 2021" "June 25, 2021"))
                (list "Kaplan" (list "June 1, 2021" "July 1, 2021"))
                (list "Vasiga" (list "July 10, 2021")))
               (list "Vasiga" "July 22, 2021"))
              (list "Vasiga" (list "July 10, 2021" "July 22, 2021")))
(check-expect (prev-vacc-add empty empty) empty)

(define (prev-vacc-add full-vacc-lst last-vacc)
  (cond [(empty? full-vacc-lst) last-vacc]
        [(equal? (first (first full-vacc-lst)) (first last-vacc))
         (list (first last-vacc)
               (append (second (first  full-vacc-lst))
                       (rest last-vacc)))]
        [else (prev-vacc-add (rest full-vacc-lst) last-vacc)]))


;; (prev-vacc-lst full-vacc-lst new-vaccs) produces a list of the
;;  and dates of doses added in a list from both the full-vacc-lst and
;;  new-vaccs. This list only posses the the names which have been
;;  repeated both in the full-vacc-lst and the new-vaccs.
;; prev-vacc-lst: (listof (list Str (listof Str)))
;;  (listof (list Str Str)) -> (listof (list Str (listof Str)))
;; Requires: full-vacc-lst and new-vaccs are sorted by alphabetical
;;  order of the names and with no duplication of names individually
;;  in both the lists.

;; Examples:
(check-expect
 (prev-vacc-lst
  (list (list "Graham" (list "May 15, 2021" "June 25, 2021"))
        (list "Kaplan" (list "June 1, 2021" "July 1, 2021"))
        (list "Vasiga" (list "July 10, 2021")))
  (list (list "Pretti" "July 17, 2021")
        (list "Vasiga" "July 22, 2021")))
 (list (list "Vasiga" (list "July 10, 2021" "July 22, 2021"))))
(check-expect
 (prev-vacc-lst empty
                (list (list "Pretti" "July 17, 2021")
                      (list "Vasiga" "July 22, 2021"))) empty)
(check-expect (prev-vacc-lst empty empty)empty)
(check-expect
 (prev-vacc-lst
  (list (list "Graham" (list "May 15, 2021" "June 25, 2021"))
        (list "Kaplan" (list "June 1, 2021" "July 1, 2021"))
        (list "Vasiga" (list "July 10, 2021")))empty) empty)

(define (prev-vacc-lst full-vacc-lst new-vaccs)
  (cond [(empty? full-vacc-lst) empty]
        [(empty? new-vaccs) empty]
        [(prev-vacc-check? full-vacc-lst (first new-vaccs))
         (cons (prev-vacc-add full-vacc-lst (first new-vaccs))
               (prev-vacc-lst full-vacc-lst (rest new-vaccs)))]
        [else (prev-vacc-lst full-vacc-lst (rest new-vaccs))]))


;; (merge-unique lst1 lst2) poduces a alphabetically sorted list
;;  merging the 2 alphabetically sorted lists lst1 and lst2 and also
;;  removing the lists with repeated names. therefore producing a
;;  unique and sorted list by name.
;; merge-unique: (listof (list Str (listof Str))) 
;;  (listof (list Str (listof Str))) ->
;;  (listof (list Str (listof Str)))
;; Requires: lst1 and lst2 are sorted alphabetically by name and with
;;  no duplication individually.

;; Examples:
(check-expect
 (merge-unique
  (list(list "Graham" (list "May 15, 2021" "June 25, 2021"))
       (list "Kaplan" (list "June 1, 2021" "July 1, 2021"))
       (list "Vasiga" (list "July 10, 2021")))
  (list(list "Harry" (list "May 15, 2021"))
       (list "Karl" (list "June 1, 2021"))
       (list "Viko" (list "July 10, 2021"))))
 (list(list "Graham" (list "May 15, 2021" "June 25, 2021"))
      (list "Harry" (list "May 15, 2021"))
      (list "Kaplan" (list "June 1, 2021" "July 1, 2021"))
      (list "Karl" (list "June 1, 2021"))
      (list "Vasiga" (list "July 10, 2021"))
      (list "Viko" (list "July 10, 2021"))))
(check-expect
 (merge-unique empty
               (list(list "Happy" (list "May 17, 2021"))
                    (list "King" (list "June 15, 2021"))
                    (list "Viking" (list "July 1, 2021"))))
 (list(list "Happy" (list "May 17, 2021"))
      (list "King" (list "June 15, 2021"))
      (list "Viking" (list "July 1, 2021"))))
(check-expect
 (merge-unique
  (list(list "Harold" (list "May 1, 2021"))
       (list "Kar" (list "June 16, 2021"))
       (list "Vagita" (list "July 17, 2021"))) empty)
 (list(list "Harold" (list "May 1, 2021"))
      (list "Kar" (list "June 16, 2021"))
      (list "Vagita" (list "July 17, 2021"))))
(check-expect
 (merge-unique
  (list(list "Harold" (list "May 1, 2021")))
  (list(list "Harold" (list "May 1, 2021"))))
 (list(list "Harold" (list "May 1, 2021"))))
(check-expect (merge-unique empty empty)empty)
(check-expect
 (merge-unique (list(list "Harold" (list "May 1, 2021"))) empty)
 (list(list "Harold" (list "May 1, 2021"))))
(check-expect
 (merge-unique empty (list(list "Harold" (list "May 1, 2021"))))
 (list(list "Harold" (list "May 1, 2021"))))

(define (merge-unique lst1 lst2)
  (cond
    [(empty? lst1) lst2]
    [(empty? lst2) lst1]
    [(equal? (first (first lst1)) (first (first lst2)))
     (cons  (first lst1) (merge-unique (rest lst1) (rest lst2)))]
    [(string<? (first (first lst1)) (first (first lst2)))
     (cons (first lst1) (merge-unique (rest lst1) lst2))]
    [else
     (cons (first lst2) (merge-unique lst1 (rest lst2)))]))


;; Main Function

;; (update full-vacc-lst new-vaccs) produces a list like full-vacc-lst
;;  except with each entry in new-vaccs taken into account. That is,
;;  if an entry for a person appearing in new-vaccs already exists in
;;  full-vacc-lst, then this new date is added to the end of the
;;  existing list of dates for this person. On the other hand, if an
;;  entry for a person appearing in new-vaccs does not already exist
;;  in full-vacc-lst, then a new entry for this person is added to
;;  full-vacc-lst with this new date. The produced list is also
;;  ordered by name.
;; update: (listof (list Str (listof Str))) (listof (list Str Str))
;;           -> (listof (list Str (listof Str)))
;; Requires: full-vacc-lst and new-vaccs are sorted by alphabetical
;;  order of the names and with no duplication of names individually
;;  in both the lists.

;; Examples:
(check-expect
 (update
  (list(list "Graham" (list "May 15, 2021" "June 25, 2021"))
       (list "Kaplan" (list "June 1, 2021" "July 1, 2021"))
       (list "Vasiga" (list "July 10, 2021")))
  (list (list "Pretti" "July 17, 2021")
        (list "Vasiga" "July 22, 2021")))
 (list(list "Graham" (list "May 15, 2021" "June 25, 2021"))
      (list "Kaplan" (list "June 1, 2021" "July 1, 2021"))
      (list "Pretti" (list "July 17, 2021"))
      (list "Vasiga" (list "July 10, 2021" "July 22, 2021"))))
(check-expect
 (update
  (list(list "Graham" (list "May 15, 2021" "June 25, 2021"))
       (list "Kaplan" (list "June 1, 2021" "July 1, 2021"))
       (list "Vasiga" (list "July 10, 2021")))
  (list (list "Kaplan" "July 22, 2021")
        (list "Pretti" "July 17, 2021")))
 (list (list "Graham" (list "May 15, 2021" "June 25, 2021"))
       (list "Kaplan"
             (list "June 1, 2021" "July 1, 2021" "July 22, 2021"))
       (list "Pretti" (list "July 17, 2021"))
       (list "Vasiga" (list "July 10, 2021"))))

(define (update full-vacc-lst new-vaccs)
  (merge-unique
   (new-vacc-lst full-vacc-lst new-vaccs)
   (merge-unique (prev-vacc-lst full-vacc-lst new-vaccs)
                 full-vacc-lst)))

;; Tests
(check-expect (update empty empty) empty)
(check-expect
 (update  (list(list "Graham" (list "May 15, 2021" "June 25, 2021"))
               (list "Kaplan" (list "June 1, 2021" "July 1, 2021"))
               (list "Vasiga" (list "July 10, 2021"))) empty)
 (list(list "Graham" (list "May 15, 2021" "June 25, 2021"))
      (list "Kaplan" (list "June 1, 2021" "July 1, 2021"))
      (list "Vasiga" (list "July 10, 2021"))))
(check-expect (update empty (list (list "Kaplan" "July 22, 2021")
                                  (list "Pretti" "July 17, 2021")))
              (list (list "Kaplan" (list "July 22, 2021"))
                    (list "Pretti" (list "July 17, 2021"))))
(check-expect
 (update  (list(list "Alice" (list "May 10, 2021" "May 22, 2021"))
               (list "Hammer" (list "June 12, 2021" "July 18, 2021"))
               (list "Tye" (list "July 5, 2021")))
          (list (list "Alice" "July 30, 2021")
                (list "Tracy" "July 30, 2021")))
 (list(list "Alice"
            (list "May 10, 2021" "May 22, 2021" "July 30, 2021"))
      (list "Hammer" (list "June 12, 2021" "July 18, 2021"))
      (list "Tracy" (list "July 30, 2021"))
      (list "Tye" (list "July 5, 2021"))))
