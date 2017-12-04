#lang racket

(require "bitmaps.rkt")
(require "pieces.rkt")
(require "board.rkt")

(provide (all-defined-out))

(define (set-avaliable-backgrounds pos)
  (define i (position-i pos))
  (define j (position-j pos))
  (define p-name (piece-name (position-piece pos)))
  (define p-color (piece-color (position-piece pos)))
  (cond
    [(equal? p-name pawn) ;; Movimento do Peão
     (define op -) (define inv-op +) (define check-op check-sub) (define inv-check-op check-sum) (define j1 6) (define inv-color black)
     (cond
       [(equal? p-color black)
        (set! op +) (set! inv-op -) (set! check-op check-sum) (set! inv-check-op check-sub) (set! j1 1) (set! inv-color white)])
     (cond
       [(equal? j j1)
        (if (and (check-op j 1) (equal? empty-piece (board-piece i (op j 1))))
            (board-set-valid i (op j 1)) 0)
        (if (and (check-op j 2) (equal? empty-piece (board-piece i (op j 1))) (equal? empty-piece (board-piece i (op j 2))))
            (board-set-valid i (op j 2)) 0)]
       [(not (equal? j j1))
        (if (and (check-op j 1) (equal? empty-piece (board-piece i (op j 1))))
            (board-set-valid i (op j 1)) 0)])
     (cond
       [(and (check-op i 1) (check-op j 1) (equal? inv-color (board-color (op i 1) (op j 1))))
        (board-set-valid (op i 1) (op j 1))])
     (cond
       [(and (inv-check-op i 1) (check-op j 1) (equal? inv-color (board-color (inv-op i 1) (op j 1))))
        (board-set-valid (inv-op i 1) (op j 1))])]))