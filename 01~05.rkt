;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |01|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; Using more constants

;; Design recipe
;; take more care of Contract and requires
;; read the questions more careful

;; Simple data
;; Bool: > < = >= <= empty? number? cons? symbol=? string<?
;; looks very simple, do not use cond
(define (between? low high numb)
  (and (< low numb) (< numb high)))

(define (can-vote? age)
  (>= age 18))

;; Simplifying conditional functions
(define (course-after-cs135-complex grade)
  (cond
    [(< grade 40) 'cs115]
    [(and (>= grade 40) (< grade 50)) 'cs135]
    [(and (>= grade 50) (< grade 60)) 'cs116]
    [(>= grade 60) 'cs136]))

(define (course-after-cs135 grade)
  (cond[(< grade 40) 'cs115]
       [(< grade 50) 'cs135]
       [(< grade 60) 'cs116]
       [else 'cs136]))

;; Tests for conditional expressions
;; when the problem contains boundary conditons (like the cut-off between
;; passing and falling), they should be tested explicitly. One way to do it
;; uses the breakpoints.
;; testing and and or expressions
;; (and cond1 cond2)
;; * cond1 is false
;; * cond1 is true but cond2 is false
;; * cond1 and cond2 are both false

;; Improve clarity with short definitions using well-chosen names.
;; Name all functions (including helpers) meaningfully.
;; Design Recipe

;; Stepping
;; apply functions only when all arguments are simple values
;; when you have a choice, take the leftmost one
;; (cond [false exp] ...) -> (cond ...)
;; (cond [true exp] ...) -> exp
;; (cond [else exp]) -> exp
;; Stepper in and and or expressions
;; (and true ...) -> (and) -> true
;; (or false ...) -> (or)  -> false

