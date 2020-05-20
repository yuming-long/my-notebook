;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 11-1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(define-struct node (key left right))
;; A Node is a (make-node Nat BT BT)
;; A BT is one of:
;; * empty
;; * Node

;; count-nodes: BT Nat -> Nat
(define (count-nodes tree k)
  (cond[(empty? tree) 0]
       [else (+ (cond[(= k (node-key tree)) 1]
                     [else 0])
                (count-nodes (node-left tree) k)
                (count-nodes (node-right tree) k))]))

;; increment: BT -> BT
(define (increment tree)
  (cond[(empty? tree) empty]
       [else (make-node (add1 (node-key tree))
                        (increment (node-left tree))
                        (increment (node-right tree)))]))

;; search-bt: Nat BT -> Bool
(define (search-bt k tree)
  (cond[(empty? tree) false]
       [(= k (node-key tree)) true]
       [else (or (search-bt k (node-left tree))
                 (search-bt k (node-right tree)))]))
;; think twice: and & or

;; search-bt-path: Nat BT -> (anyof false (listof Sym))
(define (search-bt-path k tree)
  (cond[(empty? tree) false]
       [(= k (node-key tree)) empty]
       [(list? (search-bt-path k (node-left tree)))
        (cons 'left (search-bt-path k (node-left tree)))]
       [(list? (search-bt-path k (node-right tree)))
        (cons 'right (search-bt-path k (node-right tree)))]
       [else false]))

;; A BST is one of:
;; * empty
;; * a Node
;; A Node is a (make-node Nat BST BST)
;; requires: key > every key in left BST
;;           key < every key in right BST

;; search-bst: Nat BST -> Bool
(define (search-bst k t)
  (cond[(empty? t) false]
       [(= k (node-key t)) true]
       [(> k (node-key t)) (search-bst k (node-right t))]
       [(< k (node-key t)) (search-bst k (node-left t))]))
;; think twice: node-right & node-left



                  
                        
                