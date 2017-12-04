#lang racket

(require math/array)
(require 2htdp/image)

(provide (all-defined-out))
  
(define warning-bm (bitmap "chess-pieces/warning.png"))
(define selected-bm (bitmap "chess-pieces/selected.png"))
(define non-selected-bm (bitmap "chess-pieces/non-selected.png"))
(define green-bm (bitmap "chess-pieces/green.png"))

(define selected selected-bm)
(define non-selected non-selected-bm)

(define white-pawn-bm   (bitmap "chess-pieces/white-pawn.png"))
(define white-king-bm   (bitmap "chess-pieces/white-king.png"))
(define white-queen-bm  (bitmap "chess-pieces/white-queen.png"))
(define white-bishop-bm (bitmap "chess-pieces/white-bishop.png"))
(define white-knight-bm (bitmap "chess-pieces/white-knight.png"))
(define white-rook-bm   (bitmap "chess-pieces/white-rook.png"))
(define black-pawn-bm   (bitmap "chess-pieces/black-pawn.png"))
(define black-king-bm   (bitmap "chess-pieces/black-king.png"))
(define black-queen-bm  (bitmap "chess-pieces/black-queen.png"))
(define black-bishop-bm (bitmap "chess-pieces/black-bishop.png"))
(define black-knight-bm (bitmap "chess-pieces/black-knight.png"))
(define black-rook-bm   (bitmap "chess-pieces/black-rook.png"))
(define chess-grid-bm   (bitmap "chess-pieces/chess-grid.png"))

(define pawn-bm-code   5)
(define king-bm-code   4)
(define queen-bm-code  3)
(define bishop-bm-code 2)
(define knight-bm-code 1)
(define rookt-bm-code  0)

(define white-color-code 0)
(define black-color-code 1)

(define pieces-bitmaps (array
                  #[#[white-rook-bm white-knight-bm white-bishop-bm white-queen-bm white-king-bm white-pawn-bm]
                    #[black-rook-bm black-knight-bm black-bishop-bm black-queen-bm black-king-bm black-pawn-bm]]))

;; Retorna o bitmap da peça referente ao código da cor e do bitmap
(define (pieces-bitmaps-ref color-code bm-code)
  (if (or (< color-code 0) (> color-code 1) (< bm-code 0) (> bm-code 5))
      (error "pieces-bitmaps-ref: invalid parameter.")
      (array-ref pieces-bitmaps (vector color-code bm-code))))