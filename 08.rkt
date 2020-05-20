;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |08|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; my-sort: (listof Num) -> (listof Num)
(check-expect (my-sort '( 2 0 1)) '(0 1 2))
(define (my-sort lon)
  (cond[(empty? lon) empty]
       [else (insert (first lon) (my-sort (rest lon)))]))

;; insert: Num (listof Num) -> (listof Num)
;; requires: slon is sorted in nondecreasing order.
(define (insert n slon)
  (cond[(empty? slon) (cons n empty)]
       [(<= n (first slon)) (cons n slon)]
       [else (cons (first slon) (insert n (rest slon)))]))

;; An association list (AL) is one of:
;; * empty
;; * (cons (list Num Str) AL)

;; al-template: AL -> Any
(define (al-template al)
  (cond[(empty? al) ...]
       [else (... (first (first al))... ;; first key (Num)
                  (second (first al))...;; first value (Str)
                  (al-template (rest al))...)]))

;; lookup-al: Num AL -> (Anyof false Str)
(check-expect (lookup-al 3 (list (list 1 "John") (list 3 "Winnie"))) "Winnie")
(check-expect (lookup-al 2 (list (list 1 "John") (list 3 "Winnie"))) false)
(define (lookup-al k al)
  (cond[(empty? al) false]
       [(= k (first(first al))) (second(first al))]
       [else (lookup-al k (rest al))]))

;; mult-table: Nat Nat -> (listof (listof Nat))
(define (mult-table nr nc)
  (rows-from 0 nr nc))

;; rows-from: Nat Nat Nat -> (listof (listof Nat))
(define (rows-from r nr nc)
  (cond[(>= r nr) empty]
       [else (cons (row r nc) (rows-from (add1 r) nr nc))]))

;; row: Nat Nat -> (listof Nat)
(define (row r nc)
  (cols-from 0 r nc))

;; cols-from: Nat Nat Nat -> (listof Nat)
(define (cols-from c r nc)
  (cond[(>= c nc) empty]
       [else (cons (* r c) (cols-from (add1 c) r nc))]))

;; processing just one list
;; my-append: (listof Any) (listof Any) -> (listof Any)
(check-expect (my-append empty '(1 2)) '(1 2))
(check-expect (my-append '(3 4) '(1 2 5)) '(3 4 1 2 5))
(define (my-append loa1 loa2)
  (cond[(empty? loa1) loa2]
       [else (cons (first loa1) (my-append (rest loa1) loa2))]))
;;(my-append (cons 1 (cons 2 empty)) (cons 3 (cons 4 empty)))
;; ⇒ (cons 1 (my-append (cons 2 empty) (cons 3 (cons 4 empty))))
;; ⇒ (cons 1 (cons 2 (my-append empty (cons 3 (cons 4 empty)))))
;; ⇒ (cons 1 (cons 2 (cons 3 (cons 4 empty))))

;; processing in lockstep (the same pace)
;; lockstep-template: (listof Any) (listof Any) -> Any
;; requires: lst1 lst2 of the same length
(define (lockstep-template lst1 lst2)
  (cond[(empty? lst1)...]
       [else (... (first lst1)...(first lst2)...
                  (lockstep-template (rest lst1) (rest lst2))...)]))

;; dot-product: (listof Num) (listof Num) -> Num
;; requires: lon1 and lon2 are the same length
(check-expect (dot-product empty empty) 0)
(check-expect (dot-product '(2) '(3)) 6)
(check-expect (dot-product '(2 3) '(4 5)) 23)
(define (dot-product lon1 lon2)
  (cond[(empty? lon1) 0]
       [else (+ (* (first lon1) (first lon2))
                (dot-product (rest lon1) (rest lon2)))]))

;; processing at different rates
;; twolist-template: (listof Any) (listof Any) -> Any
(define (twolist-template lst1 lst2)
  (cond[(and (empty? lst1) (empty? lst2)) ...]
       [(and (cons? lst1) (empty? lst2))
        (...(first lst1)...(rest lst2)...)]
       [(and (empty? lst1) (cons? lst2))
        (...(first lst2)...(rest lst1)...)]
       [(and (cons? lst1) (cons? lst2)) ...]))

;; last ... of the template could be any of:
;; ... (first lst2) ... (twolist-template lst1 (rest lst2)) ...
;; ... (first lst1) ... (twolist-template (rest lst1) lst2) ...
;; ... (first lst1) ... (first lst2)...
;; (twolist-template (rest lst1) (rest lst2)) ...

;; merge: (listof Num) (listof Num) -> (listof Num)
;; requires: each list is sorted in ascending order.
(check-expect (merge (list 1 8 10) (list 2 4 6 12)) (list 1 2 4 6 8 10 12))
(define (merge lon1 lon2)
  (cond[(empty? lon1) lon2]
       [(empty? lon2) lon1]
       [else (cond[(<= (first lon1) (first lon2))
                   (cons (first lon1) (merge (rest lon1) lon2))]
                  [else (cons (first lon2) (merge lon1 (rest lon2)))])]))

;; at-least?: Nat Any (listof Any) -> Bool
(check-expect (at-least? 0 'red (list 1 2 3)) true)
(check-expect (at-least? 3 "hi" empty) false)
(check-expect (at-least? 2 'red (list 'red 'blue 'red 'green)) true)
(check-expect (at-least? 3 'red (list 'red 'blue 'red 'green)) false)
(check-expect (at-least? 1 7 (list 5 4 0 5 3)) false)
(define (at-least? n elem lst)
  (cond[(zero? n) true]
       [(empty? lst) false]
       [(equal? elem (first lst)) (at-least? (sub1 n) elem (rest lst))]
       [else (at-least? n elem (rest lst))]))

;; list=?: (listof Num) (listof Num) -> Bool
(check-expect (list=? '(1 2 3) '(1 2 3)) true)
(define (list=? lst1 lst2)
  (cond[(and(empty? lst1) (empty? lst2)) true]
       [(and(cons? lst1) (empty? lst2)) false]
       [(and(empty? lst1) (cons? lst2)) false]
       [else (and (= (first lst1) (first lst2))
                  (list=? (rest lst1) (rest lst2)))]))
       


   
