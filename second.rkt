#lang racket

(require math/array)
(require 2htdp/image)
(require 2htdp/universe)

(define warning-bm (bitmap "chess-pieces/warning.png"))

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

(struct piece (name color bm) #:transparent)

(define white-pawn   (piece "Peão"   "Branco" white-pawn-bm))
(define white-king   (piece "Rei"    "Branco" white-king-bm))
(define white-queen  (piece "Rainha" "Branco" white-queen-bm))
(define white-bishop (piece "Bispo"  "Branco" white-bishop-bm))
(define white-knight (piece "Cavalo" "Branco" white-knight-bm))
(define white-rook   (piece "Torre"  "Branco" white-rook-bm))

(define black-pawn   (piece "Peão"   "Preto" black-pawn-bm))
(define black-king   (piece "Rei"    "Preto" black-king-bm))
(define black-queen  (piece "Rainha" "Preto" black-queen-bm))
(define black-bishop (piece "Bispo"  "Preto" black-bishop-bm))
(define black-knight (piece "Cavalo" "Preto" black-knight-bm))
(define black-rook   (piece "Torre"  "Preto" black-rook-bm))

(define empty (piece "EMPTY" "EMPTY" warning-bm))

(struct position (x y piece) #:transparent #:mutable)

(define A1 (position 0 7 white-rook))
(define A2 (position 1 7 white-knight))
(define A3 (position 2 7 white-bishop))
(define A4 (position 3 7 white-king))
(define A5 (position 4 7 white-queen))
(define A6 (position 5 7 white-bishop))
(define A7 (position 6 7 white-knight))
(define A8 (position 7 7 white-rook))

(define B1 (position 0 6 white-pawn))
(define B2 (position 1 6 white-pawn))
(define B3 (position 2 6 white-pawn))
(define B4 (position 3 6 white-pawn))
(define B5 (position 4 6 white-pawn))
(define B6 (position 5 6 white-pawn))
(define B7 (position 6 6 white-pawn))
(define B8 (position 7 6 white-pawn))

(define C1 (position 0 5 empty))
(define C2 (position 1 5 empty))
(define C3 (position 2 5 empty))
(define C4 (position 3 5 empty))
(define C5 (position 4 5 empty))
(define C6 (position 5 5 empty))
(define C7 (position 6 5 empty))
(define C8 (position 7 5 empty))

(define D1 (position 0 4 empty))
(define D2 (position 1 4 empty))
(define D3 (position 2 4 empty))
(define D4 (position 3 4 empty))
(define D5 (position 4 4 empty))
(define D6 (position 5 4 empty))
(define D7 (position 6 4 empty))
(define D8 (position 7 4 empty))

(define E1 (position 0 3 empty))
(define E2 (position 1 3 empty))
(define E3 (position 2 3 empty))
(define E4 (position 3 3 empty))
(define E5 (position 4 3 empty))
(define E6 (position 5 3 empty))
(define E7 (position 6 3 empty))
(define E8 (position 7 3 empty))

(define F1 (position 0 2 empty))
(define F2 (position 1 2 empty))
(define F3 (position 2 2 empty))
(define F4 (position 3 2 empty))
(define F5 (position 4 2 empty))
(define F6 (position 5 2 empty))
(define F7 (position 6 2 empty))
(define F8 (position 7 2 empty))

(define G1 (position 0 1 black-pawn))
(define G2 (position 1 1 black-pawn))
(define G3 (position 2 1 black-pawn))
(define G4 (position 3 1 black-pawn))
(define G5 (position 4 1 black-pawn))
(define G6 (position 5 1 black-pawn))
(define G7 (position 6 1 black-pawn))
(define G8 (position 7 1 black-pawn))

(define H1 (position 0 0 black-rook))
(define H2 (position 1 0 black-knight))
(define H3 (position 2 0 black-bishop))
(define H4 (position 3 0 black-king))
(define H5 (position 4 0 black-queen))
(define H6 (position 5 0 black-bishop))
(define H7 (position 6 0 black-knight))
(define H8 (position 7 0 black-rook))

(define positions (array
                  #[#[A1 A2 A3 A4 A5 A6 A7 A8]
                    #[B1 B2 B3 B4 B5 B6 B7 B8]
                    #[C1 C2 C3 C4 C5 C6 C7 C8]
                    #[D1 D2 D3 D4 D5 D6 D7 D8]
                    #[E1 E2 E3 E4 E5 E6 E7 E8]
                    #[F1 F2 F3 F4 F5 F6 F7 F8]
                    #[G1 G2 G3 G4 G5 G6 G7 G8]
                    #[H1 H2 H3 H4 H5 H6 H7 H8]]))

(define (real-pos x)
  (* x 64))

(define (virtual-pos x)
  (floor (/ x 64)))

(define game-scene (empty-scene 512 512))

(define (update-game-scene image x y)
  (set! game-scene (place-image/align image x y "left" "top" game-scene))
  game-scene)

(define (draw-pos pos)
  (update-game-scene (piece-bm (position-piece pos)) (real-pos (position-x pos)) (real-pos (position-y pos)))
  game-scene)

(define (draw-game-scene time)
  (update-game-scene chess-grid-bm 0 0)
  (for/array: ([i (in-range 8)])
    (for/array: ([j (in-range 8)])
      (draw-pos (array-ref positions (vector i j)))))
  game-scene)

(define (mouse-event-handler event x y result)
  (display (virtual-pos x))
  (display ",")
  (display (virtual-pos y))
  (display " "))

(big-bang 0
  (on-mouse mouse-event-handler)
  (to-draw draw-game-scene))