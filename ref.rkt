#lang racket
(define MAX-BYTES 1000000000)
(custodian-limit-memory (current-custodian) MAX-BYTES)

(require math/array)
(require 2htdp/image)
(require 2htdp/universe)

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

(define empty (piece "EMPTY" "EMPTY" non-selected))

(struct position (x y piece background) #:transparent #:mutable)

(define (position-change-piece p new-piece)
  (struct-copy position p [piece new-piece]))

(define (position-change-background p bm)
  (struct-copy position p [background bm]))

(define A1 (position 0 7 white-rook non-selected))
(define A2 (position 1 7 white-knight non-selected))
(define A3 (position 2 7 white-bishop non-selected))
(define A4 (position 3 7 white-king non-selected))
(define A5 (position 4 7 white-queen non-selected))
(define A6 (position 5 7 white-bishop non-selected))
(define A7 (position 6 7 white-knight non-selected))
(define A8 (position 7 7 white-rook non-selected))

(define B1 (position 0 6 white-pawn non-selected))
(define B2 (position 1 6 white-pawn non-selected))
(define B3 (position 2 6 white-pawn non-selected))
(define B4 (position 3 6 white-pawn non-selected))
(define B5 (position 4 6 white-pawn non-selected))
(define B6 (position 5 6 white-pawn non-selected))
(define B7 (position 6 6 white-pawn non-selected))
(define B8 (position 7 6 white-pawn non-selected))

(define C1 (position 0 5 empty non-selected))
(define C2 (position 1 5 empty non-selected))
(define C3 (position 2 5 empty non-selected))
(define C4 (position 3 5 empty non-selected))
(define C5 (position 4 5 empty non-selected))
(define C6 (position 5 5 empty non-selected))
(define C7 (position 6 5 empty non-selected))
(define C8 (position 7 5 empty non-selected))

(define D1 (position 0 4 empty non-selected))
(define D2 (position 1 4 empty non-selected))
(define D3 (position 2 4 empty non-selected))
(define D4 (position 3 4 empty non-selected))
(define D5 (position 4 4 empty non-selected))
(define D6 (position 5 4 empty non-selected))
(define D7 (position 6 4 empty non-selected))
(define D8 (position 7 4 empty non-selected))

(define E1 (position 0 3 empty non-selected))
(define E2 (position 1 3 empty non-selected))
(define E3 (position 2 3 empty non-selected))
(define E4 (position 3 3 empty non-selected))
(define E5 (position 4 3 empty non-selected))
(define E6 (position 5 3 empty non-selected))
(define E7 (position 6 3 empty non-selected))
(define E8 (position 7 3 empty non-selected))

(define F1 (position 0 2 empty non-selected))
(define F2 (position 1 2 empty non-selected))
(define F3 (position 2 2 empty non-selected))
(define F4 (position 3 2 empty non-selected))
(define F5 (position 4 2 empty non-selected))
(define F6 (position 5 2 empty non-selected))
(define F7 (position 6 2 empty non-selected))
(define F8 (position 7 2 empty non-selected))

(define G1 (position 0 1 black-pawn non-selected))
(define G2 (position 1 1 black-pawn non-selected))
(define G3 (position 2 1 black-pawn non-selected))
(define G4 (position 3 1 black-pawn non-selected))
(define G5 (position 4 1 black-pawn non-selected))
(define G6 (position 5 1 black-pawn non-selected))
(define G7 (position 6 1 black-pawn non-selected))
(define G8 (position 7 1 black-pawn non-selected))

(define H1 (position 0 0 black-rook non-selected))
(define H2 (position 1 0 black-knight non-selected))
(define H3 (position 2 0 black-bishop non-selected))
(define H4 (position 3 0 black-king non-selected))
(define H5 (position 4 0 black-queen non-selected))
(define H6 (position 5 0 black-bishop non-selected))
(define H7 (position 6 0 black-knight non-selected))
(define H8 (position 7 0 black-rook non-selected))

(define positions (mutable-array
                  #[#[H1 G1 F1 E1 D1 C1 B1 A1]
                    #[H2 G2 F2 E2 D2 C2 B2 A2]
                    #[H3 G3 F3 E3 D3 C3 B3 A3]
                    #[H4 G4 F4 E4 D4 C4 B4 A4]
                    #[H5 G5 F5 E5 D5 C5 B5 A5]
                    #[H6 G6 F6 E6 D6 C6 B6 A6]
                    #[H7 G7 F7 E7 D7 C7 B7 A7]
                    #[H8 G8 F8 E8 D8 C8 B8 A8]]))

(define (positions-change-piece i j piece)
  (define new-pos (position-change-piece (array-ref positions (vector i j)) piece))
  (array-set! positions (vector i j) new-pos))

(define (positions-change-background i j bm)
  (cond
    [(and (and (<= i 7) (<= j 7)) (and (>= i 0) (>= j 0)))
     (define new-pos (position-change-background (array-ref positions (vector i j)) bm))
     (array-set! positions (vector i j) new-pos)]))

(define (real-pos x)
  (* x 64))

(define (virtual-pos x)
  (floor (/ x 64)))

(define game-scene (empty-scene 512 512))

(define (update-game-scene image x y)
  (set! game-scene (place-image/align image x y "left" "top" game-scene))
  game-scene)

(define (draw-pos pos)
  (update-game-scene (position-background pos) (real-pos (position-x pos)) (real-pos (position-y pos)))
  (update-game-scene (piece-bm (position-piece pos)) (real-pos (position-x pos)) (real-pos (position-y pos)))
  game-scene)

(define (possibilities pos)
  (for/array: ([i (in-range 8)])
    (for/array: ([j (in-range 8)])
      (positions-change-background i j non-selected)))
  (define i (position-x pos))
  (define j (position-y pos))
  (define p-name (piece-name (position-piece pos)))
  (define p-color (piece-color (position-piece pos)))
  (cond
    [(equal? p-name "Peão")
     (cond
       [(equal? p-color "Branco")
        (define p1 (- j 1))
        (define p2 (- j 2))
        (positions-change-background i p1 green-bm)
        (positions-change-background i p2 green-bm)]
       [(equal? p-color "Preto")
        (define p1 (+ j 1))
        (define p2 (+ j 2))
        (positions-change-background i p1 green-bm)
        (positions-change-background i p2 green-bm)])]))

(define (draw-green pos)
  (possibilities pos))

(define (draw-game-scene time)
  (set! game-scene (empty-scene 512 512))
  (update-game-scene chess-grid-bm 0 0)
  (for/array: ([i (in-range 8)])
    (for/array: ([j (in-range 8)])
      (draw-pos (array-ref positions (vector i j)))))
  game-scene)

(define selected-pos A1)

(define (select pos)
  (possibilities pos)
  (positions-change-background (position-x selected-pos) (position-y selected-pos) non-selected)
  (positions-change-background (position-x pos) (position-y pos) selected)
  (set! selected-pos pos))

(define (clean-all-backgrounds pos)
  (for/array: ([i (in-range 8)])
    (for/array: ([j (in-range 8)])
      (positions-change-background i j non-selected))))

(define (selected? pos)
  (and (= (position-x selected-pos) (position-x pos)) (= (position-y selected-pos) (position-y pos))))

(define (try-to-select pos)
  (cond
    [(not (equal? (position-piece pos) empty))
     (select pos)]
    [(cond
       [(equal? (position-background pos) green-bm)
        (positions-change-piece (position-x pos) (position-y pos) (position-piece selected-pos))
        (positions-change-piece (position-x selected-pos) (position-y selected-pos) empty)
        (clean-all-backgrounds pos)])]))

(define (handle-button-down x y)
  (define i (virtual-pos x))
  (define j (virtual-pos y))
  (try-to-select (array-ref positions (vector i j))))

(define (mouse-event-handler result x y event)
  (define i (virtual-pos x))
  (define j (virtual-pos y))
  (if (and (equal? event "button-down") (and (and (<= i 7) (<= j 7)) (and (>= i 0) (>= j 0))))
      (handle-button-down x y)
      0))

(define (tick w)
  (display selected-pos))

(big-bang 0
  (on-mouse mouse-event-handler)
  (to-draw draw-game-scene))