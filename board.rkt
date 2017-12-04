#lang racket

(require math/array)
(require "bitmaps.rkt")
(require "pieces.rkt")

(provide (all-defined-out))

;; Verifica se a subtração de x por y é uma posição válida no tabuleiro
(define (check-sub i1 i2)
  (if (or (< (- i1 i2) 0) (> (- i1 i2) 7))
      #f #t))

;; Verifica se a soma de x por y é uma posição válida no tabuleiro
(define (check-sum i1 i2)
  (if (or (> (+ i1 i2) 7) (< (+ i1 i2) 0))
      #f #t))

;; Retorna o valor equivalente a tela
(define (real-pos i)
  (* i 64))

;; Retorna o valor equivalente ao tabuleiro
(define (board-pos x)
  (floor (/ x 64)))

;; Representa uma posição no tabuleiro
(struct position (i j piece background) #:transparent #:mutable)

;; Retorna uma posição com a peça alterada
(define (position-change-piece p new-piece)
  (struct-copy position p [piece new-piece]))

;; Retorna uma posição com o background alterado
(define (position-change-background p bm)
  (struct-copy position p [background bm]))

;; Retorna uma posição do tabuleiro
(define (board-ref i j)
  (define coord (vector i j))
  (array-ref board coord))

;; Retorna a peça na posição i j do tabuleiros
(define (board-piece i j)
  (position-piece (board-ref i j)))

;; Retorna a cor da peça na posição i j do tabuleiro
(define (board-color i j)
  (piece-color (position-piece (board-ref i j))))

;; Retorna o background da posição i j do tabuleiro
(define (board-background i j)
  (position-background (board-ref i j)))

;; Retorna o nome da posição i j do tabuleiro
(define (board-name i j)
  (piece-name (position-piece (board-ref i j))))

(define (board-set-valid i j)
  (board-set-background i j green-bm))

;; Altera a peça de uma posição do tabuleiro
(define (board-set-piece i j piece)
  (define coord (vector i j))
  (define new-pos (position-change-piece (board-ref i j) piece))
  (array-set! board coord new-pos))

;; Altera o background de uma posição do tabuleiro
(define (board-set-background i j bm)
  (cond
    [(and (and (<= i 7) (<= j 7)) (and (>= i 0) (>= j 0)))
     (define new-pos (position-change-background (board-ref i j) bm))
     (array-set! board (vector i j) new-pos)]))

;; Define os backgrounds para o estado inicial
(define (board-reset-background)
  (for/array: ([i (in-range 8)])
    (for/array: ([j (in-range 8)])
      (board-set-background i j non-selected))))

;; Define as peças para o estado inicial
(define (board-reset-pieces)
  (for/array: ([i (in-range 8)])
    (for/array: ([j (in-range 8)])
      (board-set-piece i j (position-piece (array-ref initial-board (vector i j)))))))

;; Retorna o tabuleiro para o estado inicial
(define (board-reset-all)
  (board-reset-background)
  (board-reset-pieces))

;; Definições das posições iniciais
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
(define C1 (position 0 5 empty-piece non-selected))
(define C2 (position 1 5 empty-piece non-selected))
(define C3 (position 2 5 empty-piece non-selected))
(define C4 (position 3 5 empty-piece non-selected))
(define C5 (position 4 5 empty-piece non-selected))
(define C6 (position 5 5 empty-piece non-selected))
(define C7 (position 6 5 empty-piece non-selected))
(define C8 (position 7 5 empty-piece non-selected))
(define D1 (position 0 4 empty-piece non-selected))
(define D2 (position 1 4 empty-piece non-selected))
(define D3 (position 2 4 empty-piece non-selected))
(define D4 (position 3 4 empty-piece non-selected))
(define D5 (position 4 4 empty-piece non-selected))
(define D6 (position 5 4 empty-piece non-selected))
(define D7 (position 6 4 empty-piece non-selected))
(define D8 (position 7 4 empty-piece non-selected))
(define E1 (position 0 3 empty-piece non-selected))
(define E2 (position 1 3 empty-piece non-selected))
(define E3 (position 2 3 empty-piece non-selected))
(define E4 (position 3 3 empty-piece non-selected))
(define E5 (position 4 3 empty-piece non-selected))
(define E6 (position 5 3 empty-piece non-selected))
(define E7 (position 6 3 empty-piece non-selected))
(define E8 (position 7 3 empty-piece non-selected))
(define F1 (position 0 2 empty-piece non-selected))
(define F2 (position 1 2 empty-piece non-selected))
(define F3 (position 2 2 empty-piece non-selected))
(define F4 (position 3 2 empty-piece non-selected))
(define F5 (position 4 2 empty-piece non-selected))
(define F6 (position 5 2 empty-piece non-selected))
(define F7 (position 6 2 empty-piece non-selected))
(define F8 (position 7 2 empty-piece non-selected))
(define G1 (position 0 1 black-pawn non-selected))
(define G2 (position 1 1 black-pawn non-selected))
(define G3 (position 2 1 black-pawn non-selected))
(define G4 (position 3 1 black-pawn non-selected))
(define G5 (position 4 1 black-pawn non-selected))
(define G6 (position 5 1 black-pawn non-selected))
(define G7 (position 6 1 white-pawn non-selected))
(define G8 (position 7 1 black-pawn non-selected))
(define H1 (position 0 0 black-rook non-selected))
(define H2 (position 1 0 black-knight non-selected))
(define H3 (position 2 0 black-bishop non-selected))
(define H4 (position 3 0 black-king non-selected))
(define H5 (position 4 0 black-queen non-selected))
(define H6 (position 5 0 black-bishop non-selected))
(define H7 (position 6 0 black-knight non-selected))
(define H8 (position 7 0 black-rook non-selected))

;; Representa o estado atual tabuleiro
(define board (mutable-array
                  #[#[H1 G1 F1 E1 D1 C1 B1 A1]
                    #[H2 G2 F2 E2 D2 C2 B2 A2]
                    #[H3 G3 F3 E3 D3 C3 B3 A3]
                    #[H4 G4 F4 E4 D4 C4 B4 A4]
                    #[H5 G5 F5 E5 D5 C5 B5 A5]
                    #[H6 G6 F6 E6 D6 C6 B6 A6]
                    #[H7 G7 F7 E7 D7 C7 B7 A7]
                    #[H8 G8 F8 E8 D8 C8 B8 A8]]))

;; Estado inicial do tabuleiro
(define initial-board (array
                  #[#[H1 G1 F1 E1 D1 C1 B1 A1]
                    #[H2 G2 F2 E2 D2 C2 B2 A2]
                    #[H3 G3 F3 E3 D3 C3 B3 A3]
                    #[H4 G4 F4 E4 D4 C4 B4 A4]
                    #[H5 G5 F5 E5 D5 C5 B5 A5]
                    #[H6 G6 F6 E6 D6 C6 B6 A6]
                    #[H7 G7 F7 E7 D7 C7 B7 A7]
                    #[H8 G8 F8 E8 D8 C8 B8 A8]]))