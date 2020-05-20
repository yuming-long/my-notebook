;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |14|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; euclid-gcd: Nat Nat -> Nat
(define (euclid-gcd n m)
  (cond[(zero? m) n]
       [else (euclid-gcd m (remainder n m))]))

;; Generative recursion
;; each step could be more away from the base case

;; (quick-sort lon) sorts lon in non-decreasing order
;; quick-sort: (listof Num) -> (listof Num)
(define (quick-sort lon)
  (cond[(empty? lon) empty]
       [else (local
               [(define pivot (first lon))
                (define less
                  (filter(lambda(x) (< x pivot)) (rest lon)))
                (define greater
                  (filter(lambda(x) (>= x pivot)) (rest lon)))]
               (append (quick-sort less)
                       (list pivot)
                       (quick-sort greater)))]))

;; A list is any of:
;; * empty
;; * (cons any list)

;; A Paragraph is any of:
;; * empty
;; * (cons Line Paragraph)
;; A Line is a Str without #/newline

(define (first-line loc)
  (cond[(empty? loc) empty]
       [(char=? (first loc) #\newline) empty]
       [else (cons(first loc) (first-line(rest loc)))]))
(define (rest-of-lines loc)
  (cond[(empty? loc) empty]
       [(char=? (first loc) #\newline) (rest loc)]
       [else (rest-of-lines (rest loc))]))
(define (loc->lol loc)
  (local
    [(define fline (first-line loc))
     (define rlines (rest-of-lines loc))]
    (cond[(empty? loc) empty]
         [else (... fline ... (loc->lol rlines) ...)])))

(check-expect (string->list "\n") (list #\newline))


              