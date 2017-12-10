#lang racket/base

(require racket/include)
(require dyoo-while-loop)
(require math/array)
(require 2htdp/image)
(require 2htdp/universe)

;; ------------------------------------------------------------------------------------ BITMAP ------------------------------------------------------------------------------------
  
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

;; ------------------------------------------------------------------------------------ PIECES ------------------------------------------------------------------------------------

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
      empty-piece
      (array-ref pieces (vector color-code piece-code))))

;; ------------------------------------------------------------------------------------ BOARD ------------------------------------------------------------------------------------

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

;; Posições sob ataque
(define (board-under-atack color-code)
  (for/array: ([i (in-range 8)])
    (for/array: ([j (in-range 8)])
      (if (and (not (equal? (board-color i j) color-code)) (not (equal? empty-piece (board-piece i j))))
               (set-avaliable-backgrounds (board-ref i j)) 0))))

;; ------------------------------------------------------------------------------------ MOVES ------------------------------------------------------------------------------------

(define (rook-moves i j p-color)
  (define count1 i)
  (while (check-sum count1 1)
         (set! count1 (+ count1 1))
         (define a-pos (array-ref board (vector count1 j)))
         (cond
           [(equal? empty-piece (position-piece a-pos))
            (board-set-valid count1 j)
            (continue)])
         (cond
           [(not (equal? p-color (piece-color (position-piece a-pos))))
            (board-set-valid count1 j)])
         (break))
  (define count2 i)
  (while (check-sub count2 1)
         (set! count2 (- count2 1))
         (define a-pos (array-ref board (vector count2 j)))
         (cond
           [(equal? empty-piece (position-piece a-pos))
            (board-set-valid count2 j)
            (continue)])
         (cond
           [(not (equal? p-color (piece-color (position-piece a-pos))))
            (board-set-valid count2 j)])
         (break))
  (define count3 j)
  (while (check-sum count3 1)
         (set! count3 (+ count3 1))
         (define a-pos (array-ref board (vector i count3)))
         (cond
           [(equal? empty-piece (position-piece a-pos))
            (board-set-valid i count3)
            (continue)])
         (cond
           [(not (equal? p-color (piece-color (position-piece a-pos))))
            (board-set-valid i count3)])
         (break))
  (define count4 j)
  (while (check-sub count4 1)
        (set! count4 (- count4 1))
         (define a-pos (array-ref board (vector i count4)))
         (cond
           [(equal? empty-piece (position-piece a-pos))
            (board-set-valid i count4)
            (continue)])
         (when (not (equal? p-color (piece-color (position-piece a-pos))))
           (board-set-valid i count4)))
  ) ;; Rook moves

(define (bishop-moves i j p-color)
  (define i1 i)
  (define j1 j)
  (while (and (check-sum i1 1) (check-sum j1 1))
         (set! i1 (+ i1 1))
         (set! j1 (+ j1 1))
         (define a-pos (array-ref board (vector i1 j1)))
         (cond
           [(equal? empty-piece (position-piece a-pos))
            (board-set-background i1 j1 green-bm)
            (continue)])
         (cond
           [(not (equal? p-color (piece-color (position-piece a-pos))))
            (board-set-background i1 j1 green-bm)])
         (break))
  (define i2 i)
  (define j2 j)
  (while (and (check-sum i2 1) (check-sub j2 1))
         (set! i2 (+ i2 1))
         (set! j2 (- j2 1))
         (define a-pos (array-ref board (vector i2 j2)))
         (cond
           [(equal? empty-piece (position-piece a-pos))
            (board-set-background i2 j2 green-bm)
            (continue)])
         (cond
           [(not (equal? p-color (piece-color (position-piece a-pos))))
            (board-set-background i2 j2 green-bm)])
         (break))
  (define i3 i)
  (define j3 j)
  (while (and (check-sub i3 1) (check-sum j3 1))
         (set! i3 (- i3 1))
         (set! j3 (+ j3 1))
         (define a-pos (array-ref board (vector i3 j3)))
         (cond
           [(equal? empty-piece (position-piece a-pos))
            (board-set-background i3 j3 green-bm)
            (continue)])
         (cond
           [(not (equal? p-color (piece-color (position-piece a-pos))))
            (board-set-background i3 j3 green-bm)])
         (break))
  (define i4 i)
  (define j4 j)
  (while (and (check-sub i4 1) (check-sub j4 1))
         (set! i4 (- i4 1))
         (set! j4 (- j4 1))
         (define a-pos (array-ref board (vector i4 j4)))
         (cond
           [(equal? empty-piece (position-piece a-pos))
            (board-set-background i4 j4 green-bm)
            (continue)])
         (cond
           [(not (equal? p-color (piece-color (position-piece a-pos))))
            (board-set-background i4 j4 green-bm)])
         )) ;; Bishop moves

(define (set-avaliable-backgrounds pos)
  (cond [(not (equal? (position-piece pos) empty-piece))
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
        (board-set-valid (inv-op i 1) (op j 1))])]
    [(equal? p-name king) ;; Movimento do Rei
     ;;(board-under-atack p-color)
     (if (and (check-sum i 1) (check-sum j 1) (not (equal? p-color (board-color (+ i 1) (+ j 1))))) (board-set-valid (+ i 1) (+ j 1)) 0)
     (if (and (check-sum i 1) (check-sub j 1) (not (equal? p-color (board-color (+ i 1) (- j 1))))) (board-set-valid (+ i 1) (- j 1)) 0)
     (if (and (check-sub i 1) (check-sum j 1) (not (equal? p-color (board-color (- i 1) (+ j 1))))) (board-set-valid (- i 1) (+ j 1)) 0)
     (if (and (check-sub i 1) (check-sub j 1) (not (equal? p-color (board-color (- i 1) (- j 1))))) (board-set-valid (- i 1) (- j 1)) 0)
     (if (and (check-sum j 1) (not (equal? p-color (board-color i (+ j 1)))))                       (board-set-valid i (+ j 1)) 0)
     (if (and (check-sub j 1) (not (equal? p-color (board-color i (- j 1)))))                       (board-set-valid i (- j 1)) 0)
     (if (and (check-sum i 1) (not (equal? p-color (board-color (+ i 1) j))))                       (board-set-valid (+ i 1) j) 0)
     (if (and (check-sub i 1) (not (equal? p-color (board-color (- i 1) j))))                       (board-set-valid (- i 1) j) 0)]
    [(equal? p-name knight) ;; Movimento do Cavalo
           (if (and (check-sum i 1) (check-sum j 2) (not (equal? p-color (piece-color (position-piece (array-ref board (vector (+ i 1) (+ j 2))))))))
            (board-set-background (+ i 1) (+ j 2) green-bm) 0)
           (if (and (check-sum i 1) (check-sub j 2) (not (equal? p-color (piece-color (position-piece (array-ref board (vector (+ i 1) (- j 2))))))))
            (board-set-background (+ i 1) (- j 2) green-bm) 0)
           (if (and (check-sum i 2) (check-sum j 1) (not (equal? p-color (piece-color (position-piece (array-ref board (vector (+ i 2) (+ j 1))))))))
            (board-set-background (+ i 2) (+ j 1) green-bm) 0)
           (if (and (check-sum i 2) (check-sub j 1) (not (equal? p-color (piece-color (position-piece (array-ref board (vector (+ i 2) (- j 1))))))))
            (board-set-background (+ i 2) (- j 1) green-bm) 0)
           (if (and (check-sub i 1) (check-sum j 2) (not (equal? p-color (piece-color (position-piece (array-ref board (vector (- i 1) (+ j 2))))))))
            (board-set-background (- i 1) (+ j 2) green-bm) 0)
           (if (and (check-sub i 1) (check-sub j 2) (not (equal? p-color (piece-color (position-piece (array-ref board (vector (- i 1) (- j 2))))))))
            (board-set-background (- i 1) (- j 2) green-bm) 0)
           (if (and (check-sub i 2) (check-sum j 1) (not (equal? p-color (piece-color (position-piece (array-ref board (vector (- i 2) (+ j 1))))))))
            (board-set-background (- i 2) (+ j 1) green-bm) 0)
           (if (and (check-sub i 2) (check-sub j 1) (not (equal? p-color (piece-color (position-piece (array-ref board (vector (- i 2) (- j 1))))))))
            (board-set-background (- i 2) (- j 1) green-bm) 0)]
    [(equal? p-name rook)
     (rook-moves i j p-color)]
    [(equal? p-name bishop) ;; Movimentos do Bispo
     (bishop-moves i j p-color)]
    [(equal? p-name queen) ;; Movimentos da Rainha
     (rook-moves i j p-color)
     (bishop-moves i j p-color)])]))

;; ------------------------------------------------------------------------------------ GAME ------------------------------------------------------------------------------------

;; Variável que controla o turno atual
(define turn-color white)

;; Cenas a serem renderizada
(define game-scene (empty-scene 512 512))
(define piece-scene (empty-scene 256 64))
(define win-scene (empty-scene 128 128))

;; Atualiza a cena do jogo
(define (update-game-scene image x y)
  (set! game-scene (place-image/align image x y "left" "top" game-scene)))

;; Atualiza a cena seletora de peças
(define (update-piece-scene image x y)
  (set! piece-scene (place-image/align image x y "left" "top" piece-scene)))

(define (update-win-scene image x y)
  (set! win-scene (place-image/align image x y "left" "top" win-scene)))

;; Atualiza a cena do jogo com as respectivas peças do tabuleiro
(define (draw-pos pos)
  (update-game-scene (position-background pos) (real-pos (position-i pos)) (real-pos (position-j pos)))
  (update-game-scene (piece-bm (position-piece pos)) (real-pos (position-i pos)) (real-pos (position-j pos))))

;; Se verdadeiro, não é possível realizar nenhuma ação no jogo
(define disable-main-game #f)

(define (draw-game-scene time)
  (set! game-scene (empty-scene 512 512))
  (update-game-scene chess-grid-bm 0 0)
  (for/array: ([i (in-range 8)])
    (for/array: ([j (in-range 8)])
      (draw-pos (board-ref i j))))
  game-scene)

;; Posição atual selecionada no tabuleiro (background azul)
(define selected-pos A1)

;; Verifica se a posição está selecionada
(define (selected? pos)
  (and (= (position-i selected-pos) (position-i pos)) (= (position-j selected-pos) (position-j pos))))

;; Informações sobre o último peão que atingiu a ultima casa do tabuleiro
(define winner-pawn-i -1)
(define winner-pawn-j -1)
(define winner-pawn-color empty)
(define mouse-hover 256)

(define (draw-piece-menu x)
  (set! piece-scene (empty-scene 256 64))
  (update-piece-scene selected-bm (real-pos mouse-hover) 0)
  (update-piece-scene (pieces-bitmaps-ref winner-pawn-color rook)   0   0)
  (update-piece-scene (pieces-bitmaps-ref winner-pawn-color knight) 64  0)
  (update-piece-scene (pieces-bitmaps-ref winner-pawn-color bishop) 128 0)
  (update-piece-scene (pieces-bitmaps-ref winner-pawn-color queen)  192 0)
  piece-scene)

(define (draw-win-scene x)
  (set! win-scene (text "Winner!" 48 "blue"))
   win-scene)

(define (piece-menu-mouse-event-handler result x y event)
  (define i (board-pos x)) (define j (board-pos y))
  (set! mouse-hover i)
  (cond
    [(and (equal? event "button-down"))
     (cond
       [(= i 0)
        (board-set-piece winner-pawn-i winner-pawn-j (pieces-ref winner-pawn-color rook))]
       [(= i 1)
        (board-set-piece winner-pawn-i winner-pawn-j (pieces-ref winner-pawn-color knight))]
       [(= i 2)
        (board-set-piece winner-pawn-i winner-pawn-j (pieces-ref winner-pawn-color bishop))]
       [(= i 3)
        (board-set-piece winner-pawn-i winner-pawn-j (pieces-ref winner-pawn-color queen))])]))

;; Cria uma janela de seleção de peça
(define (start-piece-menu)
  (set! disable-main-game #t)
  (big-bang 0
    (name "Piece selector")
    (on-mouse piece-menu-mouse-event-handler)
    (to-draw draw-piece-menu))
  (set! disable-main-game #f))

(define (start-win-scene color-code)
  (set! disable-main-game #t)
  (big-bang 0
    (name "Victory")
    (to-draw draw-win-scene))
  (set! disable-main-game #f))

;; Seleciona a posição
(define (select pos)
  (board-reset-background)
  (set-avaliable-backgrounds pos)
  (board-set-background (position-i selected-pos) (position-j selected-pos) non-selected)
  (board-set-background (position-i pos) (position-j pos) selected)
  (set! selected-pos pos))

;; Movimento da IA
(define (make-ai-move)
  (for/array: ([i (in-range 8)])
    (for/array: ([j (in-range 8)])
      (set-avaliable-backgrounds (board-ref i j)))))

;; Termina o turno da cor atual
(define (end-turn)
  (if (equal? turn-color white)
      (set! turn-color black)
      (set! turn-color white))
  ;;(if (equal? turn-color black)
      ;;(make-ai-move) 0))
    )  
;; Tenta selecionar uma posição. Falha se disable-main-game for true ou se o turno não corresponder a peça selecionada.
(define (try-to-select pos)
  (define i (position-i pos))
  (define j (position-j pos))
  (cond
    [(equal? disable-main-game #f)
     (cond
       [(and (equal? (board-color i j) turn-color) (not (equal? (board-piece i j) empty-piece)) (not (equal? green-bm (board-background i j))))
        (select pos)]
       [(cond
          [(equal? (position-background pos) green-bm)
           (if (equal? (board-name i j) king)
               (start-win-scene (board-color (position-i selected-pos) (position-j selected-pos))) 0)
           (board-set-piece i j (position-piece selected-pos))
           (board-set-piece (position-i selected-pos) (position-j selected-pos) empty-piece)
           (board-reset-background)
           (cond ;; Peão chegou ao final do tabuleiro
             [(and (equal? (board-name i j) pawn) (or (= j 0) (= j 7)))
              (set! winner-pawn-i i)
              (set! winner-pawn-j j)
              (set! winner-pawn-color (board-color i j))
              (start-piece-menu)])
          (end-turn)])])]))

(define (handle-button-down x y)
  (define i (board-pos x))
  (define j (board-pos y))
  (try-to-select (board-ref i j)))

(define (mouse-event-handler result x y event)
  (define i (board-pos x))
  (define j (board-pos y))
  (if (and (equal? event "button-down") (and (and (<= i 7) (<= j 7)) (and (>= i 0) (>= j 0))))
      (handle-button-down x y)
      0))

(define (start-game)
  (big-bang 0
    (name "Chess")
    (on-mouse mouse-event-handler)
    (to-draw draw-game-scene)))

(start-game)
