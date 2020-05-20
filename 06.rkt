;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |06|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; write examples and tests before the function's definition
(check-expect (next-concert empty) false)
(check-expect (next-concert (cons "a" (cons "b" empty))) "a")
(define (next-concert loc)
  (cond[(empty? loc) false]
       [else (first loc)]))

(check-expect (same-consec? (cons "a" (cons "b" empty))) false)
(check-expect (same-consec? (cons "a" (cons "a" empty))) true)
(check-expect (same-consec? empty) false)
(define (same-consec? loc)
  (and (not (empty? loc))
       (not (empty? (rest loc)))
       (string=? (first loc) (first (rest loc)))))

;; A (listof X) is one of:
;; * empty
;; * (cons X (listof X))
;; listof-X-template: (listof X) -> Any
;;(rest lox) is of type (listof X), we apple listof-X-template.
;; X-template: X -> Any
(define (X-template x) ...)
(define (listof-X-template lox)
  (cond[(empty? lox) ...]
       [(cons? lox) (...(X-template(first lox))...
                        (listof-X-template(rest lox))...)]))

;;count-concerts: (listof Str) -> Nat
(check-expect (count-concerts empty) 0)
(check-expect (count-concerts (cons "a" (cons "b" empty))) 2)
(define (count-concerts los)
  (cond[(empty? los) 0]
       [else (+ 1 (count-concerts (rest los)))]))

;;count-waterboys: (listof Str) -> Nat
(check-expect (count-waterboys empty) 0)
(check-expect (count-waterboys (cons "Waterboys" empty)) 1)
(check-expect (count-waterboys (cons "DaCapo" (cons "U2" empty))) 0)
(define (count-waterboys los)
  (cond[(empty? los) 0]
       [else (+ (cond [(string=? "Waterboys" (first los)) 1]
                       [else 0])
                (count-waterboys (rest los)))]))

;; * think about the data definiton 
;; * use data definition to write the template
;; * fill in the template

;; Simple recursion
;; every argument in a recursive function application is either:
;; * unchanged, or
;; * one step closer to a base case according to a data definition

;;nagete-list: (listof Num) -> (listof Num)
(check-expect (negate-list empty) empty)
(check-expect (negate-list (cons 2 (cons -12 empty)))
              (cons -2 (cons 12 empty)))
(define (negate-list lon)
  (cond
    [(empty? lon) empty]
    [else (cons (- (first lon)) (negate-list(rest lon)))]))

(check-expect (equal? (string->list "test")
                      (list #\t #\e #\s #\t)) true)

;; count-char/list: Char (listof Char) -> Nat
(check-expect (count-char/list #\e (string->list "")) 0)
(check-expect (count-char/list #\e (string->list "beekeeper")) 5)
(check-expect (count-char/list #\o (string->list "food")) 2)
(define (count-char/list ch loc)
  (cond
    [(empty? loc) 0]
    [else (+ (cond[(char=? ch (first loc)) 1]
                   [else 0])
             (count-char/list ch (rest loc)))]))

;; Warpper fuctions
;; * short and simple
;; * always call another function that does much more
              
              




