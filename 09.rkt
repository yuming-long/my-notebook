;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |09|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; max-list: (listof Num) -> Num
;; requires: lon is nonempty
(define (max-list lon)
  (cond[(empty? (rest lon)) (first lon)]
       [(> (first lon) (max-list (rest lon))) (first lon)]
       [else (max-list (rest lon))]))

;; max-list/acc: (listof Num) Num -> Num
(define (max-list/acc lon max-so-far)
  (cond[(empty? lon) max-so-far]
       [(> max-so-far (first lon)) (max-list/acc (rest lon) max-so-far)]
       [else (max-list/acc lon (first lon))]))
(define (max-list2 lon)
  (max-list/acc (rest lon) (first lon)))

;; accumulative recursion:
;; * all arguments to recursive function applictions are:
;; unchanged, or
;; one step closer to a base case in the data definition, or
;; a partial answer (passed in an accmulator).
;; * the value(s) in the accumulator(s) are uned in one or more base
;; cases.
;; * the accumulatively recursive function usually has a wrapper
;; function that sets the initial value of the accumulator(s).

;; my-reverse: (listof X) -> (listof X)
(check-expect (my-reverse '(1 2 3 4)) '(4 3 2 1))
(define (my-reverse lst)
  (cond[(empty? lst) empty]
       [else (append (my-reverse (rest lst)) (list(first lst)))]))
;; check built-in function's appliction

;; my-rev/acc: (listof X) (listof X) -> (listof X)
;; accumulator has the same type of data with the output.
;; accumulate X and turn it in the acc in the base case
(check-expect (my-rev '(1 2 3 4)) '(4 3 2 1))
(define (my-rev/acc lst acc)
  (cond[(empty? lst) acc]
       [else (my-rev/acc (rest lst) (cons (first lst) acc))]))
(define (my-rev lst)
  (my-rev/acc lst empty))

;; euclid-gcu: Nat Nat -> Nat
(define (euclid-gcd n m)
  (cond[(zero? m) n]
       [else (euclid-gcd m (remainder n m))]))

;; generative recursion:
;; parameters are freely calculated at each step.