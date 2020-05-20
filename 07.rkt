;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |07|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; A Nat is one of:
;; * 0
;; * (add1 Nat)

(define (nat-template n)
  (cond[(zero? n) ...]
       [else (... n ...
                  (nat-template (sub1 n))...)]))

;; countdown: Nat -> (listof Nat)
(check-expect (countdown 0) (cons 0 empty))
(check-expect (countdown 2) (cons 2 (cons 1 (cons 0 empty))))
(define (countdown n)
  (cond[(zero? n) (cons 0 empty)]
       [else (cons n (countdown (sub1 n)))]))

;; countdown-to-7: Nat -> (listof Nat)
;; requires: n >= 7

(check-expect (countdown-to-7 9) (cons 9 (cons 8 (cons 7 empty))))
(define (countdown-to-7 n)
  (cond[(= 7 n) (cons 7 empty)]
       [else (cons n (countdown-to-7 (sub1 n)))]))
;; 7 "go along for the ride" ( be passed unchanged)

;; countdown-to : Int Int -> (listof Int)
;; requires: n >= b
(check-expect (countdown-to 4 2) (cons 4 (cons 3 (cons 2 empty))))
(define (countdown-to n b)
  (cond[(= n b) (cons b empty)]
       [else (cons n (countdown-to (sub1 n) b))]))

;; similarily we have countup and countup-to
