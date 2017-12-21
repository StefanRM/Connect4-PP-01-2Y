#lang racket

(require racket/include)
(include "connect4-test.rkt")

(define RED 1)
(define YELLOW 2)
(define EMPTY 0)
(define INFINITY 99999)

;; Task pregătitor
; Functia care initializeaza tabla si jucatorul, returnand starea initiala.
; Starea este de forma '(tabla jucator).
(define init-state
  (λ (height width player)
    (list (init-board height width) player)))

; Functia care initializeaza tabla.
(define init-board
  (λ (height width)
    (build-list width (λ (el) (build-list height (λ (x) EMPTY))))))

; Functie pentru a aplica pe toate elementele dintr-o lista functia AND.
; Folosita in cadrul functiei is-empty-board?.
(define (apply-and L)
  (foldl (λ (el acc) (and el acc))
         #t
         L))

; Functia care verifica daca tabla este goala.
(define is-empty-board?
  (λ (board)
    (apply-and (map
                (λ (pair)
                  (apply-and (map (λ (el) (equal? el EMPTY)) pair)))
                board))))

; Functia care returneaza inaltimea (nr de linii).
(define get-height
  (λ (board)
    (length (car board))))

; Functia care returneaza latimea (nr de coloane).
(define get-width
  (λ (board)
    (length board)))

; Functia care returneaza valoarea discului de pe o pozitie data.
(define get-disc
  (λ (board position)
    (list-ref (list-ref board (car position)) (cdr position))))

; Functia care returneaza jucatorul curent.
(define get-player
  (λ (state)
    (second state)))

; Functia care returneaza tabla de joc curenta.
(define get-board
  (λ (state)
    (car state)))

; Starea pentru testele din checker.
(define state-test
  '(((0 0 0 0 0 0) (0 0 0 0 0 0) (1 2 1 1 2 1) (2 1 1 2 1 0) (1 0 0 0 0 0) (2 2 1 0 0 0) (2 2 0 0 0 0)) 2))

;; Task 1 a) - Determinarea acțiunilor posibile
(define get-available-actions
  (λ (board)
    (car (foldl (λ (col acc)
                  (if (equal? (last col) EMPTY)
                      (list (append (car acc) (list (last acc))) (add1 (last acc)))
                      (list (car acc) (add1 (last acc)))))
                '(() 0) board))))

;; Task 1 b) - Aplicarea unei acțiuni
(define apply-action
  (λ (state action)
    (let* ([board (get-board state)]
           [player (get-player state)]
           [column-to-add (list-ref board action)])
      (list (append
             (take board action)
             (list (car (foldl (λ (el acc)
                                 (if (and (equal? el EMPTY) (not (last acc)))
                                     (list (append (car acc) (list player)) (not (last acc)))
                                     (list (append (car acc) (list el)) (last acc))))
                               '(() #f) column-to-add)))
             (drop board (add1 action)))
            (add1 (modulo player 2))))))

; Functie care primeste o lista de actiuni si le aplica succesiv.
(define apply-actions
  (λ (state actions)
    (foldl (λ (action acc)
             (apply-action acc action))
           state actions)))

;; Task 1 c) - Verificarea stării finale

; Functia calculeaza transpusa unei matrici (in cazul nostru tabla
; este vazuta ca o matrice -> lista de liste)
(define (transpose board)
  (apply map list board))

; Functia care verifica daca jucatorul a castigat pe coloana trimisa
; ca parametru
(define (col-win? col player in-row)
  (if (null? col)
      #f
      (if (equal? (car col) player)
          (if (equal? in-row 3)
              #t
              (col-win? (cdr col) player (add1 in-row)))
          (col-win? (cdr col) player 0))))

; Functie pentru a aplica pe toate elementele dintr-o lista functia OR.
; Folosita in cadrul functiei win?.
(define (apply-or L)
  (foldl (λ (el acc) (or el acc))
         #f
         L))

; Functie care verifica daca jucatorul a castigat (foloseste doar functia
; de verificare pe coloana)
(define (win? board player)
  (apply-or (map (λ (col)
                   (col-win? col player 0))
                 board)))

; Functie care transpune diagonalele board-ului astfel incat diagonalele
; castigatoare sa devina linii castigatoare. (diagonale sus-jos "\")
(define (diag-up-down-to-row-check board)
  (let ([height (get-height board)])
    (car (foldl (λ (col acc)
                  (list (append (car acc) (list (append
                                                 (drop col (modulo (last acc) height))
                                                 (take col (modulo (last acc) height))
                                                 )
                                                )) (add1 (last acc))))
                '(() 0) board))))

; Functie care transpune diagonalele board-ului astfel incat diagonalele
; castigatoare sa devina linii castigatoare. (diagonale jos-sus "/")
(define (diag-down-up-to-row-check board)
  (let ([height (get-height board)])
    (car (foldr (λ (col acc)
                  (list (append (car acc) (list (append
                                                 (drop col (modulo (last acc) height))
                                                 (take col (modulo (last acc) height))
                                                 )
                                                )) (add1 (last acc))))
                '(() 0) board))))

; Verifica starea joculului si intoarce un rezultat corespunzator in cazul in
; care este finala.
(define is-game-over?
  (λ (state)
    (let ([board (get-board state)])
      (cond
        [(win? board RED) RED]
        [(win? board YELLOW) YELLOW]
        [(win? (transpose board) RED) RED]
        [(win? (transpose board) YELLOW) YELLOW]
        [(win? (transpose (diag-up-down-to-row-check board)) RED) RED]
        [(win? (transpose (diag-up-down-to-row-check board)) YELLOW) YELLOW]
        [(win? (transpose (diag-down-up-to-row-check board)) RED) RED]
        [(win? (transpose (diag-down-up-to-row-check board)) YELLOW) YELLOW]
        [(null? (get-available-actions board)) 3]
        [else #f])
      )))

;; Task 2 - Euristică simplă
; Functia care implementeaza strategia random.
(define select-random-action
  (λ (state rand-gen)
    (let ([possible-actions (get-available-actions (get-board state))])
      (list-ref possible-actions (random (sub1 (length possible-actions)) rand-gen)))))

; Functia care schimba player-ul in cadrul state-ului curent
(define (switch-player state)
  (list (get-board state) (add1 (modulo (get-player state) 2))))

; Functie care implementeaza o strategie minimala.
(define get-action
  (λ (state)
    (let* ([player (get-player state)]
           [opponent-state (switch-player state)]
           [opponent (get-player opponent-state)]
           [winning-moves
            (foldl (λ (game-won acc)
                     (if (equal? (car game-won) player)
                         (append acc (list (cdr game-won)))
                         acc))
                   '()
                   (map (λ (game)
                          (cons (is-game-over? (car game)) (cdr game)))
                        (map (λ (action)
                               (cons (apply-action state action) action))
                             (get-available-actions (get-board state)))))]
           [contra-attack-moves
            (foldl (λ (game-won acc)
                     (if (equal? (car game-won) opponent)
                         (append acc (list (cdr game-won)))
                         acc))
                   '()
                   (map (λ (game)
                          (cons (is-game-over? (car game)) (cdr game)))
                        (map (λ (action)
                               (cons (apply-action opponent-state action) action))
                             (get-available-actions (get-board opponent-state)))))])
      (if (null? winning-moves)
          (if (null? contra-attack-moves)
              (car (get-available-actions (get-board state)))
              (car contra-attack-moves))
          (car winning-moves)))))

(define AI #t) ;Trebuie modificat în #t după ce se implementează play-game

; Functia care simuleaza jocul
(define play-game
  (λ (state strategy1 strategy2)
    (play-game-aux state strategy1 strategy2 1 '())))

; Functia care simuleaza jocul, tinand cont de runde.
(define (play-game-aux state strategy1 strategy2 round acc)
  (if (is-game-over? state)
      (cons acc (is-game-over? state))
      (let ([action1 ((car strategy1) state (cdr strategy1))]
            [action2 ((car strategy2) state (cdr strategy2))])
        (if (zero? (modulo round 2))
            (play-game-aux (apply-action state action2) strategy1 strategy2 (add1 round) (append acc (list action2)))
            (play-game-aux (apply-action state action1) strategy1 strategy2 (add1 round) (append acc (list action1)))))))

;; Bonus
; Functia de evaluare necesara algoritmului Negamax.
(define evaluate
  (λ (state)
    (let ([evaluation (is-game-over? state)]
          [player (get-player state)])
      (if (equal? evaluation 3)
          '(0.5 0)
          (if (not evaluation)
              '(0 0)
              (if (equal? evaluation player)
                  '(1 0)
                  '(-1 0)))))))

; Functia care implementeaza algoritmul Negamax
(define negamax
  (λ (state maxDepth)
    (cdr (negamax-aux state maxDepth))
    ))

; Functie ajutatoare functiei Negamax, care returneaza perechea
; (bestScore, bestMove)
(define (negamax-aux state maxDepth)
  (if (or (is-game-over? state) (zero? maxDepth))
      (evaluate state)
      (foldl (λ (move acc)
               (letrec ([currentScore (- 0 (car (negamax-aux (apply-action state move) (sub1 maxDepth))))])
                 (if (> currentScore (car acc))
                     (cons currentScore move)
                     acc)))
             (cons (- 0 INFINITY) 0) (get-available-actions (get-board state)))))
     
;; SECȚIUNE DE TESTARE - NU modificați această linie!
;; ATENȚIE! Pentru a primi punctaj pe temă, NU modificați această secțiune!
;;
;; CHECK - TASK 0 - NU modificați această linie!
;; ATENȚIE! NU modificați această secțiune
(Task 0 : 20 puncte) ;;check-exp
(check-exp-part 'is-empty-board?1 .02 (is-empty-board? (init-board 7 7)) #t)
(check-exp-part 'is-empty-board?2 .02 (is-empty-board? (get-board state-test)) #f)
(check-exp-part 'is-empty-board?3 .02 (is-empty-board? (get-board (init-state 7 8 RED))) #t)
(check-exp-part 'get-height1 .02 (get-height (get-board (init-state 7 8 YELLOW))) 7)
(check-exp-part 'get-height2 .02 (get-height (get-board state-test)) 6)
(check-exp-part 'get-height3 .02 (get-height (init-board 10 14)) 10)
(check-exp-part 'get-width1 .02 (get-width (get-board (init-state 7 8 YELLOW))) 8)
(check-exp-part 'get-width2 .02 (get-width (get-board state-test)) 7)
(check-exp-part 'get-width3 .02 (get-width (init-board 10 14)) 14)
(check-exp-part 'get-width4 .01 (get-width (init-board 20 20)) 20)
(check-exp-part 'get-player1 .02 (get-player state-test) YELLOW)
(check-exp-part 'get-player2 .02 (get-player (init-state 15 7 RED)) RED)
(check-exp-part 'get-player3 .02 (get-player (init-state 10 8 YELLOW)) YELLOW)
(check-exp-part 'get-disc1 .05 (get-disc (get-board (init-state 10 8 YELLOW)) (cons 1 2)) EMPTY)
(check-exp-part 'get-disc2 .05 (get-disc (get-board state-test) (cons 1 2)) EMPTY)
(check-exp-part 'get-disc3 .05 (get-disc (get-board state-test) (cons 5 0)) YELLOW)
(check-exp-part 'get-disc4 .05 (get-disc (get-board state-test) (cons 5 1)) YELLOW)
(check-exp-part 'get-disc5 .05 (get-disc (get-board state-test) (cons 5 2)) RED)
(check-exp-part 'get-disc6 .05 (get-disc (get-board state-test) (cons 5 3)) EMPTY)
(check-exp-part 'get-disc7 .05 (get-disc (get-board state-test) (cons 2 3)) RED)
(check-exp-part 'get-disc8 .05 (get-disc (get-board state-test) (cons 2 5)) RED)
(check-exp-part 'get-disc9 .05 (get-disc (get-board state-test) (cons 3 0)) YELLOW)
(check-exp-part 'get-disc10 .05 (get-disc (get-board state-test) (cons 3 3)) YELLOW)
(check-exp-part 'get-disc11 .05 (get-disc (get-board state-test) (cons 6 0)) YELLOW)
(check-exp-part 'get-disc12 .05 (get-disc (get-board state-test) (cons 6 1)) YELLOW)
(check-exp-part 'get-disc13 .05 (get-disc (get-board state-test) (cons 6 5)) EMPTY)
(check-exp-part 'get-disc14 .05 (get-disc (get-board state-test) (cons 0 0)) EMPTY)
(check-exp-part 'get-disc15 .05 (get-disc (get-board state-test) (cons 6 5)) EMPTY)
;; SFÂRȘIT CHECK - TASK 0 - NU modificați această linie!
;;
;; CHECK - Task1 - NU modificați această linie!
;; ATENȚIE! NU modificați această secțiune!
(Task 1 : 50 puncte) ;;check-exp
(check-exp-part 'get-available-actions1 .04 (get-available-actions (get-board state-test)) '(0 1 3 4 5 6))
(check-exp-part 'get-available-actions2 .04 (get-available-actions (init-board 7 9)) '(0 1 2 3 4 5 6 7 8))
(check-exp-part 'get-available-actions3 .04 (get-available-actions (get-board (init-state 10 8 YELLOW))) '(0 1 2 3 4 5 6 7))
(check-exp-part 'get-available-actions4 .04 (get-available-actions (get-board (apply-action state-test 3))) '(0 1 4 5 6))
(check-exp-part 'get-available-actions5 .04 (get-available-actions (get-board (apply-actions state-test '(3 5 5 5)))) '(0 1 4 6))
(check-exp-part 'apply-action1 .02 (get-disc (get-board (apply-action state-test 3)) (cons 3 5)) YELLOW)
(check-exp-part 'apply-action2 .02 (get-player (apply-action state-test 3)) RED)
(check-exp-part 'apply-action3 .02 (get-player (apply-action (init-state 7 7 YELLOW) 1)) RED)
(check-exp-part 'apply-action4 .02 (get-disc (get-board (apply-action (init-state 6 6 RED) 1)) (cons 1 0)) RED)
(check-exp-part 'apply-action5 .02 (get-disc (get-board (apply-action (init-state 4 6 YELLOW) 2)) (cons 2 0)) YELLOW)
(check-exp-part 'apply-actions1 .02 (get-player (apply-actions (init-state 7 6 YELLOW) '(1 0 2 1 1 3 4 1 2 3))) YELLOW)
(check-exp-part 'apply-actions2 .02 (get-disc (get-board (apply-actions (init-state 6 6 RED) '(0 1 1 0 1 1 0 2 3))) (cons 0 2)) RED)
(check-exp-part 'apply-actions3 .02 (get-available-actions (get-board (apply-actions (init-state 6 6 RED) '(0 1 1 0 1 1 0 2 3 1 1)))) '(0 2 3 4 5))
(check-exp-part 'apply-actions4 .02 (get-available-actions (get-board (apply-actions (init-state 4 4 RED) '(1 1 1 1 0 0 0 0 2 2 2 2 3 3)))) '(3))
(check-exp-part 'apply-actions5 .02 (get-available-actions (get-board (apply-actions state-test '(1 1 1 1 1 1 0 0 0 0 0 0)))) '(3 4 5 6))
(check-exp-part 'apply-actions6 .02 (get-available-actions (get-board (apply-actions (init-state 4 4 RED) '(2 0 0 0 1 1 3 0 2 2 1 1 3 3 3 2)))) '())
(check-exp-part 'apply-actions7 .02 (get-available-actions (get-board (apply-actions (init-state 7 9 YELLOW) '(5 5 8 5 5 8 8 0 0 4 1 5 5 6 1 1 7 8 2 3 1 5 3 6 1 0 3 1 1 0 4 3 2)))) '(0 2 3 4 6 7 8))
(check-exp-part 'apply-actions8 .02 (get-available-actions (get-board (apply-actions (init-state 12 12 RED) '(9 6 8 10 3 1 1 3 7 5 11 11 7 3 11 0 5 6 7 9 5 3 0 10 5 10 10 6 1 7 0 3)))) '(0 1 2 3 4 5 6 7 8 9 10 11))
(check-exp-part 'apply-actions9 .02 (get-available-actions (get-board (apply-actions (init-state 15 15 YELLOW) '(8 10 0 13 9 2 9 6 1 5 14 6 3 3 11 5 13 7 13 13 3 13 10 8 9 11 1 12 12 6 4 5 2 12)))) '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14))
(check-exp-part 'apply-actions10 .02 (get-available-actions (get-board (apply-actions (init-state 8 15 RED) '(6 1 3 3 10 12 14 1 8 3 9 10 3 11 3 9 7)))) '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14))
(check-exp-part 'is-game-over?1 .01 (is-game-over? (apply-actions (init-state 4 4 RED) '(1 1 1 1 0 0 0 0 2 2 2 2 3))) RED)  
(check-exp-part 'is-game-over?2 .01 (is-game-over? (apply-actions (init-state 5 7 RED) '(0 3 1 3 2 3 4 4 5 0 1 5 0 6 6 2))) YELLOW)  
(check-exp-part 'is-game-over?3 .01 (is-game-over? (apply-actions (init-state 5 6 YELLOW) '(0 1 2 3 4 5 5 4 3 2 1 0 0 1 3 2 3 2 0 3))) RED)  
(check-exp-part 'is-game-over?4 .01 (is-game-over? (apply-actions (init-state 4 4 RED) '(1 1 1 1 0 0 0 0 2 2 2 2))) #f)
(check-exp-part 'is-game-over?5 .01 (is-game-over? (apply-actions (init-state 6 6 YELLOW) '(0 5 0 5 0 3 0))) YELLOW)
(check-exp-part 'is-game-over?6 .01 (is-game-over? state-test) #f)
(check-exp-part 'is-game-over?7 .02 (is-game-over? (apply-actions (init-state 4 4 RED) '(2 0 0 0 1 1 3 0 2 2 1 1 3 3 3 2))) 3)
(check-exp-part 'is-game-over?8 .02 (is-game-over? (apply-actions (init-state 8 15 RED) '(6 1 3 3 10 12 14 1 8 3 9 10 3 11 3 9 7))) RED)
(check-exp-part 'is-game-over?9 .02 (is-game-over? (apply-actions (init-state 8 15 YELLOW) '(6 8 5 2 1 4 9 7 12 9 12 9 8 3 3 10 6 7 11 6 12 13 9 6 0 6 10 7 1 10 7 12 13 14 8 11 7 7 7 5 7 9 2 11 1 3 11 12 3 12 11 9 11 1 8 9 12 14 5 3 2))) YELLOW)
(check-exp-part 'is-game-over?10 .02 (is-game-over? (apply-actions (init-state 8 8 RED) '(2 0 0 1 7 0 1 4 7 2 1 3 6 7 7 6 3 3 3 7 7 2 0 4 4 3 7 4))) YELLOW)
(check-exp-part 'is-game-over?11 .02 (is-game-over? (apply-actions (init-state 4 20 RED) '(16 13 8 13 17 14 5 0 15 13 18))) RED)
(check-exp-part 'is-game-over?12 .02 (is-game-over? (apply-actions (init-state 7 7 RED) '(4 4 0 1 1 4 1 1 1 5 3 1 4 6 4 2 0 5 6 3 6 5 0 6 3 1 5 3))) YELLOW)
(check-exp-part 'is-game-over?13 .02 (is-game-over? (apply-actions (init-state 5 8 RED) '(5 1 0 0 1 6 7 4 7 2 4 2 3 3 4 4 6 2 2 0 1 6 4 3))) YELLOW)
(check-exp-part 'is-game-over?14 .02 (is-game-over? (apply-actions (init-state 4 8 RED) '(0 0 0 0 1 1 1 1 2 3 2 2 2 3 4 3 3 4 4 4 5 5 5 5 7 6 6 6 6 7 7 7))) 3)
(check-exp-part 'is-game-over?15 .02 (is-game-over? (apply-actions (init-state 4 8 RED) '(1 0 5 3 7 0 0 1 6 0 7 1 7 6 5 2))) #f)
(check-exp-part 'is-game-over?16 .02 (is-game-over? (apply-actions (init-state 9 4 YELLOW) '(0 2 1 1 3 3 1 0 2 1 2 2 3 3 1 0 2 3 2 3 0))) YELLOW)
(check-exp-part 'is-game-over?17 .02 (is-game-over? (apply-actions (init-state 9 4 RED) '(1 1 1 1 3 0 0 2 1 1 2 2 0 2 1 1 3 2))) #f)
(check-exp-part 'is-game-over?18 .02 (is-game-over? (apply-actions (init-state 5 5 RED) '(4 3 2 4 3 2 3 2 0 1 1 0 4 2 3 4 2 1 4 0 1 3))) 2)
(check-exp-part 'is-game-over?19 .02 (is-game-over? (apply-actions (init-state 7 4 RED) '(2 0 0 2 1 3 1 0 3 0 2 0 0 0 2 1))) 2)
(check-exp-part 'is-game-over?20 .02 (is-game-over? (apply-actions (init-state 7 9 YELLOW) '(1 6 5 2 0 3 2 2 6 4 3 1 1 0 1 5 8 5 5 7 2 8 2 1 1 2 5 0 5 6 6 8 4 7 2 0 1 8 7 7))) 1)
(check-exp-part 'is-game-over?21 .02 (is-game-over? (apply-actions (init-state 5 9 YELLOW) '(8 0 7 6 6 0 1 5 6 0 7 5 4 8 5 6 0 0 5))) 2)
(check-exp-part 'is-game-over?22 .02 (is-game-over? (apply-actions (init-state 10 4 YELLOW) '(2 1 2 0 3 1 3 0 0 3 0 2 1))) 2)
(check-exp-part 'is-game-over?23 .02 (is-game-over? (apply-actions (init-state 10 6 RED) '(0 0 4 1 2 5 0 0 1 4 0 1 1 3 2 0 4 1 2 2 0 1 1 5 1 1 1 5 2 2 5 5 3))) 1)
(check-exp-part 'is-game-over?24 .02 (is-game-over? (apply-actions (init-state 10 4 YELLOW) '(2 1 2 0 3 1 3 0 0 3 0 2 1))) 2)
(check-exp-part 'is-game-over?25 .02 (is-game-over? (apply-actions (init-state 10 7 YELLOW) '(3 1 3 1 4 5 2 0 1 0 2 0 2 6 4 2 1))) 2)
(check-exp-part 'is-game-over?26 .02 (is-game-over? (apply-actions (init-state 10 10 YELLOW) '(2 9 3 5 7 2 6 1 0 4 8 7 2 5 0 9 5 5 7 1 6 7 7 6))) 1)
(check-exp-part 'is-game-over?27 .02  (is-game-over? (apply-actions (init-state 6 4 RED) '(0 1 2 3 3 2 1 0 0 1 2 3 0 1 3 2))) 1)
(check-exp-part 'is-game-over?28 .02 (is-game-over? (apply-actions (init-state 5 6 RED) '(0 1 2 3 4 5 5 4 2 3 1 0 0 1 3 2 3 4 4 5 0 5 5))) RED)
;; SFÂRȘIT CHECK - TASK 1 - NU modificați această linie!
;;
;; CHECK - Task2 - NU modificați această linie!
;; ATENȚIE! NU modificați această secțiune!
(Task 2 : 30 puncte) ;;check-exp
(define FUNCTIONS (list is-game-over? play-game get-available-actions apply-actions)) ;;check-exp
(check-in-part 'get-action1 .1 (get-action (apply-actions (init-state 9 4 YELLOW) '(0 0 3 3 3 2 2 1 2 2 3 1 0 1 0))) '(1 3))
(check-in-part 'get-action2 .1 (get-action (apply-actions (init-state 7 7 RED) '(1 4 2 4 6 2 1 4 1 0 5 3 6 0 5 1 5 0 4 2 1 5))) '(0 3))
(check-exp-part 'get-action3 .1 (get-action (apply-actions (init-state 10 6 YELLOW) '(3 4 3 3 4 5 4 2 3 2 3 3 2 4 2 3 3 2 4 4 0 0 3 4 1 0 3 1 0 1 5 2))) 1)
(check-exp-part 'get-action4 .05 (get-action (apply-actions (init-state 4 4 YELLOW) '(1 0 1 1 0 3 0 0 3 2 3 3))) 2)
(check-exp-part 'get-action5 .05 (get-action (apply-actions (init-state 4 4 RED) '(0 3 2 3 3 1 1 2 2))) 3)
(check-exp-part 'get-action6 .05 (get-action (apply-actions (init-state 8 8 RED) '(3 0 2 1 6 6 5 1))) 4)
(check-exp-part 'get-action7 .1 (get-action (apply-actions (init-state 10 5 YELLOW) '(1 2 4 1 3 0 3 2 2 1 2 1 2))) 1)
(check-exp-part 'get-action8 .1 (get-action (apply-actions (init-state 12 12 YELLOW) '(6 3 0 9 4 2 10 7 1 7 0 0 9 2 0 8 2 8 8 10 10 10 5 2 3 11 4 4 4 8 3 2 2 11 11 8 8))) 9)
(check-in-part 'get-action9 .1 (get-action (apply-actions (init-state 20 8 RED) '(3 2 2 1 2 7 2 5 0 6 1 5 0 5))) '(2 3))
(check-in-part 'get-action10 .1 (get-action (apply-actions (init-state 10 10 YELLOW) '(0 4 0 5 0 6 1 7 1 8 1 9 2 9 2 8 2))) '(0 1 2 3 4 5 6 7 8 9))
(check-exp-part 'play-game1 .025 (check-game AI (init-state 8 9 RED) (cons select-random-action (current-pseudo-random-generator)) (cons  select-random-action (current-pseudo-random-generator)) FUNCTIONS 1) #t)
(check-exp-part 'play-game2 .025 (check-game AI (init-state 9 8 YELLOW) (cons simple-strategy 'None) (cons  simple-strategy 'None) FUNCTIONS 1) #t) 
(check-exp-part 'play-game3 .025 (check-play-game AI (init-state 7 7 YELLOW) (cons simple-strategy 'None) (cons  select-random-action (current-pseudo-random-generator)) FUNCTIONS 4 YELLOW #f) #t) 
(check-exp-part 'play-game4 .025 (check-game AI state-test (cons select-random-action (current-pseudo-random-generator)) (cons  select-random-action (current-pseudo-random-generator)) FUNCTIONS 1) #t) 
(check-exp-part 'play-game5 .025 (check-play-game AI (init-state 10 5 YELLOW) (cons simple-strategy 'None) (cons  select-random-action (current-pseudo-random-generator)) FUNCTIONS 4 YELLOW #f) #t) 
(check-exp-part 'play-game6 .025 (check-play-game AI (init-state 10 10 YELLOW) (cons simple-strategy 'None) (cons  select-random-action (current-pseudo-random-generator)) FUNCTIONS 4 YELLOW #f) #t) 
;; SFÂRȘIT CHECK - TASK 2 - NU modificați această linie!
;;
;; CHECK - BONUS - NU modificați această linie!
;; ATENȚIE! NU modificați această secțiune
(Bonus 3 : 20 puncte BONUS) ;;check-exp
(check-exp-part 'bonus1 .1 (check-play-game AI state-test (cons negamax 1) (cons negamax 3) FUNCTIONS 4 RED #t) #t)
(check-exp-part 'bonus2 .1 (check-play-game AI state-test (cons select-random-action (current-pseudo-random-generator)) (cons negamax 4) FUNCTIONS 4 RED #t) #t)
(check-exp-part 'bonus3 .1 (check-play-game AI (apply-actions (init-state 4 6 RED) '(1 0 1 1 0 3)) (cons select-random-action (current-pseudo-random-generator)) (cons negamax 5) FUNCTIONS 6 YELLOW #t) #t)
(check-exp-part 'bonus4 .1 (check-play-game AI (init-state 7 7 YELLOW) (cons select-random-action (current-pseudo-random-generator)) (cons negamax 3) FUNCTIONS 6 RED #t) #t)
(check-exp-part 'bonus5 .1 (check-play-game AI (apply-actions (init-state 9 6 YELLOW) '(1 2 0 3 1 0 2 4 5 0)) (cons select-random-action (current-pseudo-random-generator)) (cons negamax 3) FUNCTIONS 6 RED #t) #t)
(check-exp-part 'bonus6 .1 (check-play-game AI (init-state 8 8 RED) (cons negamax 1) (cons negamax 3) FUNCTIONS 6 YELLOW #t) #t)
(check-exp-part 'bonus7 .1 (check-play-game AI (init-state 4 6 RED) (cons negamax 1) (cons negamax 5) FUNCTIONS 4 YELLOW #t) #t)
(check-exp-part 'bonus8 .1 (check-play-game AI (init-state 5 7 RED) (cons negamax 2) (cons negamax 5) FUNCTIONS 2 YELLOW #t) #t)
(check-exp-part 'bonus9 .1 (check-play-game AI (apply-actions (init-state 10 8 RED) '(1 5 2 6 7 1 0 2 6 4 0 7 7)) (cons negamax 2) (cons negamax 4) FUNCTIONS 4 YELLOW #t) #t)
(check-exp-part 'bonus10 .1 (check-play-game AI (apply-actions (init-state 10 6 RED) '(1 0 1 1 0 3)) (cons select-random-action (current-pseudo-random-generator)) (cons negamax 3) FUNCTIONS 4 YELLOW #t) #t)
;; SFÂRȘIT CHECK - BONUS - NU modificați această linie!
;; SFÂRȘIT SECȚIUNE DE TESTARE
(sumar)
