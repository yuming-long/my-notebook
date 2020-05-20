;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |10|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;; distance: Posn Posn -> Num
(check-expect (distance (make-posn 1 1) (make-posn 4 5)) 5)
(define (distance posn1 posn2)
  (sqrt (+ (sqr (- (posn-x posn2) (posn-x posn1)))
           (sqr (- (posn-y posn2) (posn-y posn1))))))

;; point-on-line: Num Num Num -> Posn
(check-expect (point-on-line 3 7 2) (make-posn 2 13))
(define (point-on-line slope intercept x)
  (make-posn x (+ (* x slope) intercept)))

;; scale: Posn Num -> Posn
(check-expect (scale (make-posn 3 4) 0.5) (make-posn 1.5 2))
(define (scale v factor)
  (make-posn (* factor (posn-x v))
             (* factor (posn-y v))))

;; Structure and data definitions
;; constructor: make-....
;; selectors: constructor-...
;; type predicate: constrcutor?

;; Templates foe compound data
;; songinfo-template: SongInfo -> Any
(define-struct songinfo (performer title genre length))
;; An SongIfo is a (make-songinfo Str Str Sym Nat)
(define (songinfo-template info)
  (...  (songinfo-performer info) ...
        (songinfo-title info) ...
        (songinfo-genre info) ...
        (songinfo-length info) ...))

;; undate-genre: SongInfo Sym -> SongInfo
(check-expect (update-genre
(make-songinfo "C.O.C." "Eye For An Eye" 'Folk 78) 'Punk)
(make-songinfo "C.O.C." "Eye For An Eye" 'Punk 78))
(define (update-genre oldinfo newgenre)
  (make-songinfo (songinfo-performer oldinfo)
                 (songinfo-title oldinfo)
                 newgenre
                 (songinfo-length oldinfo)))

(define-struct movieinfo (director title genre duration))
;; A MovieInfo is a (make-movieinfo Str Str Sym Num)
;;
;; An MmInfo is one of
;; * a SongInfo
;; * a MovieInfo

(define (mminfo-template info)
  (cond[(songinfo? info)
        (...  (songinfo-performer info) ...
        (songinfo-title info) ...
        (songinfo-genre info) ...
        (songinfo-length info) ...)]
       [(movieinfo? info)
        (...  (movieinfo-director info) ...
        (movieinfo-title info) ...
        (movieinfo-genre info) ...
        (movieinfo-duration info) ...)]))

