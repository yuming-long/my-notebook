;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname 11-2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(define-struct node (key val left right))
;; val has arbitrary name and type

;; A BSTD is either
;; empty or (make-node Nat Str BSTD BSTD)

;; search-bst: Nat BSTD -> anyof (Str false)
(define test-tree (make-node 5 "Susan"
                             (make-node 1 "Juan" empty empty)
                             (make-node 14 "David"
                                        (make-node 6 "Lucy" empty empty)
                                        empty)))
(check-expect (search-bst-dict 5 empty) false)
(check-expect (search-bst-dict 5 test-tree) "Susan")
(check-expect (search-bst-dict 6 test-tree) "Lucy")
(check-expect (search-bst-dict 2 test-tree) false)
(define (search-bst-dict k t)
  (cond[(empty? t) false]
       [(= k (node-key t)) (node-val t)]
       [(> k (node-key t)) (search-bst-dict k (node-right t))]
       [(< k (node-key t)) (search-bst-dict k (node-left t))]))

;; An EvoTree is one of:
;; * a RSpecies
;; * a EvoEvent
(define-struct rspecies (name endangered))
;; A RSpecies is a (make-respecies Str Bool)
(define-struct evoevent (name age left right))
;; A EvoEvent is a (make-evoevent Str Num EvoTree EvoTree)
(define human (make-rspecies "human" false))
(define chimp (make-rspecies "chimp" true))
(define rat (make-rspecies "rat" false))
(define crane (make-rspecies "crane" true))
(define chicken (make-rspecies "chicken" false))
(define worm (make-rspecies "worm" false))
(define fruit-fly (make-rspecies "fruit fly" false))
(define primate (make-evoevent "Primate" 5 human chimp))
(define mammal (make-evoevent "Mammal" 65 primate rat))
(define bird (make-evoevent "Bird" 100 crane chicken))
(define vertebrate (make-evoevent "Vertebrate" 320 mammal bird))
(define invertebrate (make-evoevent "Invertebrate" 530 worm fruit-fly))
(define animal (make-evoevent "Animal" 535 vertebrate invertebrate))

;; repecies-template: RSpecies -> Any
(define (repecies-template rs)
  (... (repecies-name rs) ...
       (repecies-endangered rs) ...))
;; evoevent-template: EvoEvent -> Any
(define (evoevent-template ee)
  (... (evoevent-name ee) ...
       (evoevent-age ee) ...
       (evoevent-template (evoevent-left ee)) ...
       (evoevent-template (evoevent-right ee)) ...))
;; mutual recursion
;; evotree-template: EvoTree -> Any
(define (evotree-template t)
  (cond[(rspecies? t) (repecies-template t)]
       [(evoevent? t) (evoevent-template t)]))

;; count-species: EvoTree -> Nat
(check-expect (count-species animal) 7)
(check-expect (count-species human) 1)
(define (count-species t)
  (cond[(rspecies? t) (count-recent t)]
       [(evoevent? t) (count-evoevent t)]))
;; count-recent: RSpecies -> Nat
(define (count-recent t) 1)
;; count-evoevent: Evoevent -> Nat
(define (count-evoevent t)
  (+ (count-species (evoevent-left t))
     (count-species (evoevent-right t))))

;; list-names: EvoTree -> (listof Str)
(define (list-names t)
  (list-names/acc t empty))
;; list-names/acc: EvoTree (listof Str) -> (listof Str)
(define (list-names/acc t names)
  (cond[(rspecies? t) (list-rs-names t names)]
       [(evoevent? t) (list-ee-names t names)]))
;; list-rs-names: RSpecies (listof Str) -> (listof Str)
(define (list-rs-names rs names)
  (cons (rspecies-name rs) names))
;; list-ee-names: EvoEvent (listof Str) -> (listof Str)
(define (list-ee-names ee names)
  (cons (evoevent-name ee)
        (list-names/acc (evoevent-left ee)
                        (list-names/acc (evoevent-right ee) names))))

(define-struct binode (op left right))
;; A BINode is a
;; (make-binode (anyof '* '+ '- '/) BinExp Binexp)
;; A BinExp is one of:
;; * a Num
;; * a BINode

;; eval: BinExp -> Num
(check-expect (eval 5) 5)
(check-expect (eval (make-binode '+ 2 5)) 7)
(check-expect (eval (make-binode '/ (make-binode '- 10 2)
                                 (make-binode '+ 2 2))) 2)
(define (eval ex)
  (cond[(number? ex) ex]
       [else (cond
               [(symbol=? '* (binode-op ex))
                (* (eval(binode-left ex)) (eval(binode-right ex)))]
               [(symbol=? '+ (binode-op ex))
                (+ (eval(binode-left ex)) (eval(binode-right ex)))]
               [(symbol=? '/ (binode-op ex))
                (/ (eval(binode-left ex)) (eval(binode-right ex)))]
               [(symbol=? '- (binode-op ex))
                (- (eval(binode-left ex)) (eval(binode-right ex)))])]))

(define-struct ainode (op args))
;; An AINode is a
;; (make-ainode (anyof '+ '*) (listof AExp))
;; An AExp is one of
;; * a Num
;; * an AINode

;; my-eval: AExp -> Num
(check-expect (my-eval 3) 3)
(check-expect (my-eval (make-ainode '+ (list 3 4))) 7)
(check-expect (my-eval (make-ainode '+ '())) 0)
(define (my-eval ex)
  (cond[(number? ex) ex]
       [(ainode? ex) (my-apply (ainode-op ex)
                            (ainode-args ex))]))
;; apply: op (listof AExp) -> Num
(define (my-apply op args)
  (cond[(empty? args) (cond[(symbol=? op '+) 0]
                           [(symbol=? op '*) 1])]
       [(symbol=? op '+) (+ (my-eval (first args))
                            (my-apply op (rest args)))]
       [(symbol=? op '*) (* (my-eval (first args))
                            (my-apply op (rest args)))]))

;; always use a helper function (apply op args)
;; look at the date type to decide which function to call
;; An AltAExp is one of:
;; * a Num
;; * (cons (anyof '* '+) (listof AlltAExp))
;; eval-aax: AltAExp -> Num
(define (eval-aax aax)
  (cond[(number? aax) aax]
       [else (apply-aax (first aax) (rest aax))]))
(define (apply-aax op aaxl)
  (cond[(and (empty? aaxl) (symbol=? op '∗)) 1]
       [(and (empty? aaxl) (symbol=? op '+)) 0]
       [(symbol=? op '*)
        (* (eval-aax (first aaxl)) (apply op (rest aaxl)))]
       [(symbol=? op '+)
        (+ (eval-aax (first aaxl)) (apply op (rest aaxl)))]))

;; conclusion:
;; (eval ...) (cond[(number? ...)][else (apply ... ...)])
;; (apply op ...) (cond[(empty? ...][(symbol=? ...])

;; structuring data using mutual recursion
;; * create templates from the data definitions and
;; * create one function for each template.

;; A Nest-List-X is one of:
;; * empty
;; * (cons X Nest-List-X)
;; * (cons Nest-List-X Nest-List-X)

;; nest-lst-template: Nest-List-Num -> Any
(define (nest-lst-template lst)
  (cond[(empty? lst) ...]
       [(number? (first lst)) (...(first lst)...
                                  (nest-lst-template (rest lst))...)]
       [else (...(nest-lst-template (first lst))...
                 (nest-lst-template (rest lst))...)]))

;; count-items: Nest-List-Num -> Nat
(define (count-items nln)
  (cond[(empty? lst) 0]
       [(number? (first lst)) (+ 1 (count-items (rest nln)))]
       [else (+ (count-items (first nln))
                (count-items (rest nln)))]))

;; flatten: Nest-List-Num -> (listof Num)
(check-expect (flatten '(1 2 3)) '(1 2 3))
(check-expect (flatten '((1 2 3) (a b c))) '(1 2 3 a b c))
(define (flatten lst)
  (cond[(empty? lst) empty]
       [(number? (first lst)) (cons (first lst) (flatten (rest lst)))]
       [else (apped (flatten (first lst)) (flatten (rest lst)))]))
;; append in the last line not cons.
        
                      





       


