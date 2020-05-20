;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |12|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(define (t-area a b c)
  (local[(define s (/ (+ a b c) 2))]
    (sqrt (* s (- s a) (- s b) (- s c)))))

(define (max-list lon)
  (cond[(empty? (rest lon)) (first lon)]
       [else (local[(define max-rest (max-list (rest lon)))]
               (cond[(> (first lon) max-rest) (first lon)]
                    [else max-rest]))]))

(define-struct node (key left right))
(define (search-bt-path k tree)
  (cond[(empty? tree) false]
       [(= k (node-key tree)) '()]
       [else (local[(define left (search-bt-path k (node-left tree)))]
               (cond[(list? left) (cons 'left left)]
                    [else (local
                            [(define right
                               (search-bt-path k (node-right tree)))]
                            (cond[(list? right) (cons 'right right)]
                                 [else false]))]))]))

(define (isort lon)
  (local[(define (insert n slon)
           (cond[(empty? slon) (cons n empty)]
                [(<= n (first slon)) (cons n slon)]
                [else (cons(first slon) (insert n (rest slon)))]))]
    (cond[(empty? lon) empty]
         [else (insert (first lon) (isort (rest lon)))])))
                

(define (sum-list lon)
  (local[(define (sum-list/acc lst sofar)
           (cond[(empty? lst) sofar]
                [else (sum-list/acc (rest lst)
                                    (+ (first lst) sofst))]))]
    (sum-list/acc lon 0)))
;; we can not call sum-list/acc because it is inside the local
;; Reasons to use local:
;; naming subexpressions
;; aviod recomputation
;; hiding stuff
;; reusing parameters

;; local function still needs to write desgin recipe




 



  
