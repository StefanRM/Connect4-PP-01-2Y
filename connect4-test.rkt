;;#lang racket

(define default-results '(#f 0 () your-code-here)) ; ce rezultate default sunt întoarse în exerciții
(define show-defaults 200) ; câte exerciții la care s-au întors rezultate default să fie arătate detaliat
(define prepend #t) (define name-ex '(testul testele trecut)) 
(define : 'separator) (define punct 'string) (define puncte 'string) (define BONUS 'string) (define total 0) (define all '()) (define n-ex 0) (define p-ex 0) (define defaults '())
(define (ex n sep p . s) (set! n-ex n) (set! p-ex p) (set! all (cons (list n p) all))) (define exercițiul ex) (define (p L) (map (λ (e) (display e) (display " ")) L) (newline))
(define (check-exp given expected) (check-exp-part "" 1 given expected)) (define (check-exp-part part per given expected) (check-test part per equal? given expected "diferă de cel așteptat"))
(define (check-in  given expected) (check-in-part  "" 1 given expected)) (define (check-in-part part per given expected) (check-test part per member given expected "nu se află printre variantele așteptate"))
(define (check-set given expected) (check-set-part  "" 1 given expected)) (define (check-set-part part per given expected) (check-test part per (λ (x y) (apply equal? (map list->seteqv `(,given ,expected)))) given expected "nu este echivalent cu cel așteptat"))
(define (check-set-unique given expected) (check-set-unique-part  "" 1 given expected)) (define (check-set-unique-part part per given expected) (check-test part per (λ (x y) (and (apply = (map length `(,given ,expected))) (apply equal? (map list->seteqv `(,given ,expected))))) given expected "nu este echivalent cu cel așteptat"))
(define (check-test part percent tester given expected err-m) (if (not (tester given expected)) (and (when (member given default-results) (set! defaults (cons (if (< percent 1) (cons n-ex part) n-ex) defaults)))
  (when (or (not (member given default-results)) (<= (length defaults) show-defaults))
    (p `(NU: la ,(car name-ex) ,(if (< percent 1) (cons n-ex part) n-ex) rezultatul ,given ,err-m : ,expected))))
 (let ((pts (* p-ex percent))) (and (if prepend (printf "+~v: " pts) (printf "OK: "))
  (p (list (car name-ex) (if (< percent 1) (cons n-ex part) n-ex) (caddr name-ex) '+ pts (if (= pts 1) 'punct 'puncte))) (set! total (+ total pts))))))
(define (sumar) (when (and (not (null? defaults)) (< show-defaults (length defaults))) (p `(... rezultatul implicit dat la ,(cadr name-ex) ,(reverse defaults)))) (p `(total: ,total puncte)))
(define Task ex) (define Bonus ex)

(define (simple-strategy state dummy-arg)
  (get-action state))

;; Apelez funcția play-game și verific rezultatul întors de aceasta 
(define check-game
  (λ (AI state strategy1 strategy2 functions type)
    (let* ((is-game-over? (car functions))
           (play-game (cadr functions))
           (get-available-actions (caddr functions))
           (apply-actions (cadddr functions))
           (result (play-game state strategy1 strategy2)))
      ; Verific dacă a fost modificată valoarea pentru AI
      (and AI
           (if (= type 1)
               ; Verificarea cerinței 3 (funcția euristică simplă)
               (= (is-game-over? (apply-actions state (car result))) (cdr result))
               (λ (player)
                 ; Verificarea bonusului (trebuie ca player să câștige jocul)
                 (= (is-game-over? (apply-actions state (car result))) player)))))))

;; Funcție folosită pentru a testa bonusul
;; Calculează câte jocuri sunt câștigate de player și întoarce #t dacă acest număr este mai mare
;; decât jumătatea numărului total de jocuri.
(define check-play-game
  (λ (AI state strategy1 strategy2 functions noOfGames player bonus)
    (and AI
         ; Compar numărul de jocuri câștigate cu noOfGames / 2
	 (or (not bonus) (not (equal? (negamax state-test 1) 'your-code-here)))
         (> (let iter ((result 0) (index 0))
              (cond [(= index noOfGames) result]
                    ; Apelez funcția check-game pentru a testa rezultatul întors de play-game, iar dacă
                    ; player a câștigat jocul incrementez result.
                    [((check-game AI state strategy1 strategy2 functions 0) player) (iter (+ result 1) (+ index 1))]
                    ; Player a pierdut jocul curent.
                    [else (iter result (+ index 1))]))
            (quotient noOfGames 2)))))

(define print-board (λ (B) (andmap (λ (l) (and (display (map (λ (c) (get-disc B (cons c l))) (range (get-width B)))) (newline))) (reverse (range (get-height B))))))
(define print-state (λ (S) (and (print-board (get-board S)) (display "Next player: ") (display (cdr (assoc (get-player S) (list (cons 1 'RED) (cons 2 'YELLOW))))))))

