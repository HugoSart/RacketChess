#lang racket

(require racket/draw
         mred)

(require racket/include)
(require 2htdp/image)
(require 2htdp/universe)
(include "bitmaps.rkt")

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
(define black-rook-bm  (bitmap "chess-pieces/black-rook.png"))
(define chess-grid-bm (bitmap "chess-pieces/chess-grid.png"))

(struct piece (name color bm))

(define white-pawn   (piece "Peão"   "Branco" white-pawn-bm))
(define white-king   (piece "Rei"    "Branco" white-king-bm))
(define white-queen  (piece "Rainha" "Branco" white-queen-bm))
(define white-bishop (piece "Bispo"  "Branco" white-bishop-bm))
(define white-knight (piece "Cavalo" "Branco" white-knight-bm))
(define white-rook   (piece "Torre"  "Branco" white-rook-bm))

(struct coord (x y))
(struct position (coord piece))

(define grid-class%
  (class object%
    (field (grid-bm chess-grid-bm))
    (field (a1 (position (coord 1 1) white-rook)))
    (field (a2 (position (coord 1 2) white-knight)))
    (field (a3 (position (coord 1 3) white-bishop)))
    (field (a4 (position (coord 1 4) white-king)))
    (field (a5 (position (coord 1 5) white-queen)))
    (field (a6 (position (coord 1 6) white-bishop)))
    (field (a7 (position (coord 1 7) white-knight)))
    (field (a8 (position (coord 1 8) white-rook)))))

;; define a canvas that displays a bitmap when its on-paint
;; method is called
(define bitmap-canvas%
  (class canvas%
    (init-field [bitmap1 #f])
    (inherit get-dc)
    (define/override (on-paint)
      (send (get-dc) draw-bitmap bitmap1 0 0)
      (send (get-dc) draw-bitmap bitmap2 0 448))
    (super-new)))

;; load the bitmap
(define bitmap1 (read-bitmap "chess-pieces/chess-grid.png"))
(define bitmap2 (read-bitmap "chess-pieces/white-rook.png"))

;; create a new frame (top-level window)
(define f (new frame% [label "foo"] [width 512] [height 512]))

;; create a canvas
(define the-canvas (new bitmap-canvas% [parent f] [bitmap1 bitmap1]))

;; show the canvas
(send f show #t)
