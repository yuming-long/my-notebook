;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |15|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; A Graph is one of
;; * empty
;; * (listof (list Node (listof Node)))
;; A Node is a Sym

(define graph1 '((A (C D E))
                 (B (E J))
                 (C ())
                 (D (F J))
                 (E (K))
                 (F (K H))
                 (H ())
                 (J (H))
                 (K ())))

;; graph-template: Graph -> Any
(define (graph-template G)
  (cond[(empty? G) ...]
       [else (... (first (first G)) ...
                  (listof-node-template(second(first G))) ...
                  (graph-template (rest G)) ...)]))

;; neighbours: Node Graph -> (listof Node)
;; requires: v is a node in G
(define (neighbours v G)
  (cond[(empty? G) empty]
       [(symbol=? v (first(first G))) (second(first G))]
       [else (neighbours v (rest G))]))

;; (find-route orig dest G)finds route from orig to dest in G if
;; it exists
;; find-route: Node Node Graph -> (any of (listof Node) false)
(define (find-route1 orig dest G visited)
  (cond[(symbol=? orig dest) (list dest)]
       [else (local[(define nbrs (neighbours orig G))
                    (define route
                      (find-route/list1 nbrs dest G (cons orig visited)))]
               (cond[(false? route) false]
                    [else (cons orig route)]))]))

;; (find-route/list los dest G) produces route frome an element of
;; los to dest in G, if one exists
;; find-route/list: (listof Node) Node Graph ->
;; (anyof (listof Node) false)
(define (find-route/list1 los dest G visited)
  (cond[(empty? los) false]
       [(member? (first los) visited)
        (find-route/list1 (rest los) dest G)]
       [else
        (local[(define route
                 (find-route1 (first los) dest G visited))]
          (cond[(false? route)
                (find-route/list1 (rest los) dest G visited)]
               [else route]))]))

(define-struct routepair (valid-path? nodes))
;; a RoutePair is a (make-routepair Bool (listof Node))

(define (find-route/list los dest G visited)
  (cond[(empty? los) (make-routepair false visited)]
       [(member? (first los) visited)
        (find-route/list (rest los) dest G visited)]
       [else (local
               [(define route
                  (find-route/acc (first los) dest G visited))]
               (cond[(not(routepair-valid-path? route))
                     (find-route/list (rest los) dest G
                                      (routepair-nodes route))]
                    [else route]))]))

(define (find-route/acc orig dest G visited)
  (cond[(symbol=? orig dest) (make-routepair true (list dest))]
       [else (local[(define nbrs (neighbours orig G))
                    (define route
                      (find-route/list nbrs dest G
                                       (cons orig visited)))]
               (cond[(not(routepair-valid-path? route)) route]
                    [else
                     (make-routepair true
                                     (cons orig
                                           (routepair-nodes route)))
                     ]))]))

(define (find-route orig dest G)
  (local[(define route (find-route/acc orig dest G empty))]
    (cond[(routepair-valid-path? route) (routepair-nodes route)]
         [else false])))