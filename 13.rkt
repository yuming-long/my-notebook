;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |13|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;  Advantages of using abstract list function
;; reduce code siza
;; avoid cut-abd-paste
;; fix bug

;; my-filiter: (X -> Bool) (listof X) -> (list of X)
(define (my-filiter pred lst)
  (cond[(empty? lst) empty]
       [(pred (first lst))
        (cons (first lst) (my-filiter pred (rest lst)))]
       [else (my-filiter pred (rest lst))]))
(check-expect (my-filiter even? '(1 2 3 4 5 6)) '(2 4 6))
(define (div-by-3? n)
  (= (remainder n 3) 0))
(check-expect (my-filiter div-by-3? '(1 2 3 4 5 6)) '(3 6))

(define (my-adder n)
  (local [(define (func m) (+ m n))] func))
(define add6 (my-adder 6))
(check-expect ((my-adder 6) 7) 13)
(check-expect (add6 7) 13)

;; filter: (X -> Bool) (listof X) -> (list of X)
(check-expect (filter div-by-3? '(1 2 3 4 5 6)) '(3 6))
(define (eat-apples lst)
  (filter (lambda (sym) (not(symbol=? sym 'apple))) lst))

(define (keep-odds lst)
  (filter odd? lst))
(check-expect (keep-odds '(1 2 3 4 5)) '(1 3 5))

;; producing anonymous functions
(check-expect
 (filter (lambda (n) (= (remainder n 3) 0)) '(1 2 3 4 5 6)) '(3 6))
;; (lambda (x1 ... xn) exp)

(define interest-rate 0.03)
(define interest-earned (lambda (amount) (* interest-rate amount)))

(define (make-adder n)
  (local[(define (f m) (+ n m))] f))
(define (my-adder2 n)
  (lambda (m) (+ n m)))
(define adder (lambda (x) (lambda (y) (+ x y))))

(define (my-map f lst)
  (cond[(empty? lst) empty]
       [else (cons(f(first lst))
                  (my-map f (rest lst)))]))

(check-expect (my-map sqr (list 3 6 5)) (list 9 36 25))

;; map: (X -> Y) (listof X) -> (listof Y)

(define(list-templat lst)
  (cond[(empty? lst)...]
       [else (...(first lst)
                 (list-template (rest lst))...)]))

;; foldr: first argument must be a function that expects two arguments
;; (x y -> y) y (listof x) -> y
(define (combine m n) (+ m n))
(define (my-foldr combine base lst)
  (cond[(empty? lst) base]
       [else (combine(first lst)
                     (my-foldr combine base (rest lst)))]))

(define (sum-of-numbers lst) (foldr + 0 lst))
(define (prod-of-numbers lst) (foldr * 1 lst))
(define (count-symbols lst)
  (foldr (lambda (x rror) (add1 rror)) 0 lst))
(define (bar lon)
  (foldr max (first lon) (rest lon)))
(define (foo los)
  (foldr (lambda (s rror) (+ (string-length s) rror)) 0 los))
(define (negate-list lst)
  (foldr (lambda (x rror) (cons (- x) rror)) empty lst))
(define (my-reverse lst)
  (foldr (lambda (x rror) (append rror (list x))) empty lst))
(define (my-reverse2 lst)
  (foldl cons '() lst))
 

(check-expect (build-list 4 (lambda (x) x)) (list 0 1 2 3))
(check-expect (build-list 4 (lambda (x) (sqr x))) (list 0 1 4 9))

(define (sum n f)
  (foldr + 0 (build-list n f)))
(check-expect (sum 4 sqr) 14)

(define (my-member? n lst)
  (cond[(empty?(filter (lambda (m) (equal? n m)) lst)) false]
       [else true]))

(define (my-append lst1 lst2)
  (foldr (lambda (x rror) (cons x rror)) lst2 lst1))