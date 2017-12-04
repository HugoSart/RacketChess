#lang racket

(require math/array)
(require "bitmaps.rkt")

(provide (all-defined-out))

(define empty-code -1)
(define white-code 0)
(define black-code 1)
(struct piece (name color bm) #:transparent)

(define pawn-code   5)
(define king-code   4)
(define queen-code  3)
(define bishop-code 2)
(define knight-code 1)
(define rook-code   0)

(define white-pawn   (piece pawn-code   white-code white-pawn-bm))
(define white-king   (piece king-code   white-code white-king-bm))
(define white-queen  (piece queen-code  white-code white-queen-bm))
(define white-bishop (piece bishop-code white-code white-bishop-bm))
(define white-knight (piece knight-code white-code white-knight-bm))
(define white-rook   (piece rook-code   white-code white-rook-bm))
(define black-pawn   (piece pawn-code   black-code black-pawn-bm))
(define black-king   (piece king-code   black-code black-king-bm))
(define black-queen  (piece queen-code  black-code black-queen-bm))
(define black-bishop (piece bishop-code black-code black-bishop-bm))
(define black-knight (piece knight-code black-code black-knight-bm))
(define black-rook   (piece rook-code   black-code black-rook-bm))
(define empty-piece (piece empty-code empty-code non-selected))

(define empty empty-code)
(define white white-code)
(define black black-code)
(define pawn pawn-code)
(define rook rook-code)
(define king king-code)
(define queen queen-code)
(define bishop bishop-code)
(define knight knight-code)

(define pieces (array
                #[#[white-rook white-knight white-bishop white-queen white-king white-pawn]
                  #[black-rook black-knight black-bishop black-queen black-king black-pawn]]))

;; Retorna o bitmap da peça referente ao código da cor e do bitmap
(define (pieces-ref color-code piece-code)
  (if (or (< color-code 0) (> color-code 1) (< piece-code 0) (> piece-code 5))
      empty
      (array-ref pieces-bitmaps (vector color-code piece-code))))