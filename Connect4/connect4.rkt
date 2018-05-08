#lang racket

(require racket/include)
(include "connect4-test.rkt")

(define RED 1)
(define YELLOW 2)
(define EMPTY 0)
(define INFINITY 99999)

;; Task pregătitor
(define (num->list n)
  (if (equal? n 0) null
      (append (num->list (quotient n 10)) (list (modulo n 10)))))

(define (list->num L)
  (/ (foldl (λ (x acc) (* (+ acc x) 10)) 0 (flatten (map num->list L))) 10))

(define list-helper
  (λ (count element)
    (if(equal? count 0)
       '()
       (cons element (list-helper (- count 1) element)))))

(define init-state
  (λ (height width player)
    ;done
    (cons player (list-helper height (list-helper width EMPTY)))))

;(init-state 2 3 RED)



(define init-board
  (λ (height width)
    ;done
    (list-helper height (list-helper width EMPTY))))

;(cons RED (init-board 2 3))

(define empty-help
  (λ (board counter)
    (if (empty? board)
        counter
        (empty-help (cdr board) (+ counter (count (λ (e) (if (equal? 0 e) #t #f)) (car board)))))))



(define is-empty-board?
  (λ (board)
    ;done
    (if (equal? (empty-help board 0) (* (length board) (length (car board))))
        #t
        #f)))


(define get-height
  (λ (board)
    ;done
    (length board)))

(define get-width
  (λ (board)
    ;done
    (length (car board))))

(define disc-helper
  (λ (pos list)
    (car (drop list pos))))

(define get-disc
  (λ (board position)
    ;done?
    (disc-helper (car position) (disc-helper (cdr position) (reverse board)))))

(define get-player
  (λ (state)
    ;done
    (car state)))

(define get-board
  (λ (state)
    ;done
    (cdr state)))



(define state-test
  ;done
  (cons YELLOW
        (list
         (list 0 0 RED 0 0 0 0)
         (list 0 0 YELLOW RED 0 0 0)
         (list 0 0 RED YELLOW 0 0 0)
         (list 0 0 RED RED 0 RED 0)
         (list 0 0 YELLOW RED 0 YELLOW YELLOW)
         (list 0 0 RED YELLOW RED YELLOW YELLOW))))

;; Task 1 a) - Determinarea acțiunilor posibile

(define find-zeros
  (λ (list result count)
    (if (empty? list)
        result
        (if (equal? (car list) 0)
            (find-zeros (cdr list) (cons count result) (+ count 1))
            (find-zeros (cdr list) result (+ count 1))))))



(define get-available-actions
  (λ (board)
    ;done
    (reverse (find-zeros (car board) '() 0))))
  
;; Task 1 b) - Aplicarea unei acțiuni
(define get-column
  (λ (board index result)
    (if (empty? board)
        (reverse result)
        (get-column (cdr board) index (cons (disc-helper index (car board)) result))))) 

;(get-column (get-board state-test) 3 '())

(define find-next-position
  (λ (list counter)
    (if (pair? list)
        (if (equal? 0 (car list))
            (find-next-position (cdr list) (+ counter 1))
            (- counter 1))
        (if (equal? 0 list)
            counter
            (- counter 1)))))

(define replace-position
  (λ (list pos elem)
    (append (take list pos) (cons elem (drop list (+ pos 1))))))

;(find-next-position '(0 0 0 1 2 3) 0)

(define get-mod-column
  (λ (state pos)
    (replace-position (get-column (cdr state) pos '()) (find-next-position (get-column (cdr state) pos '()) 0) (car state))))

(define modify-board
  (λ (board col pos result)
    (if (empty? col)
        (reverse result)
        (modify-board (cdr board) (cdr col) pos (cons (replace-position (car board) pos (car col)) result)))))
      
(define apply-action
  (λ (state action)
    ;done
    (if (equal? RED (get-player state))
        (cons YELLOW (modify-board (get-board state) (get-mod-column state action) action '()))
        (cons RED (modify-board (get-board state) (get-mod-column state action) action '())))))

;(apply-action state-test 1)
state-test
(replace-position '(0 0 1 2 1 2 2) 1 3)
;(find-next-position 0 (get-column (cdr state) action '()))
;(find-next-position 0 (get-column (cdr state) action '()))
(define (atom? x ) (not (or (pair? x) (null? x))))

(define apply-actions
  (λ (state actions)
    ;done
    (if (empty? actions)
        state
        (if (atom? actions)
            (apply-action state (list->num (list actions))) 
            (apply-actions (apply-action state (car actions)) (cdr actions))))))

(define transpose
  (λ (m)
    (apply map list m)))

(define (mirror matrix)
  (map reverse matrix))
  
(define search-line
  (λ (line count elem)
    (if (equal? count 4)
        #t
        (if (empty? line)
            #f
            (if (equal? (car line) elem)
                (search-line (cdr line) (+ count 1) elem)
                (search-line (cdr line) 0 elem))))))

(define make-column
  (λ (board a b result)
    (if (or (equal? a -1) (equal? b -1))
        result
        (make-column board (- a 1) (- b 1) (cons (get-disc board (cons a b)) result)))))

(define diag-help-height
  (λ (board a b result)
    (if (equal? a (get-width board))
        result
        (diag-help-height board (+ a 1) b (cons (make-column board a b '()) result)))))

(define check-all-lines
  (λ (board elem result)
    (if (empty? board)
        result
        (check-all-lines (cdr board) elem (or result (search-line (car board) 0 elem))))))

;(diag-help-height (get-board state-test) 3 (- (get-height (get-board state-test)) 1) '())
;(diag-help-width (get-board state-test) (- (get-width (get-board state-test)) 1) 3 '())

(define diag-help-width
  (λ (board a b result)
    (if (equal? b (- (get-height board) 1))
        result
        (diag-help-width board a (+ b 1) (cons (make-column board a b '()) result)))))

(define make-list-of-diag
  (λ (board)
    (append (diag-help-height board 3 (- (get-height board) 1) '())
            (diag-help-width board (- (get-width board) 1) 3 '()))))

(define is-board-full
  (λ (board)
    (if (member 0 (car board))
        #f
        #t)))

;; Task 1 c) - Verificarea stării finale
(define is-game-over?
  (λ (state)
    ;done
    (if (or (check-all-lines (get-board state) RED #f)
            (or (check-all-lines (transpose (get-board state)) RED #f)
                (or (check-all-lines (make-list-of-diag (get-board state)) RED #f) (check-all-lines (make-list-of-diag (mirror (get-board state))) RED #f))))
        RED
        (if (or (check-all-lines (get-board state) YELLOW #f)
                (or (check-all-lines (transpose (get-board state)) YELLOW #f)
                    (or (check-all-lines (make-list-of-diag (get-board state)) YELLOW #f) (check-all-lines (make-list-of-diag (mirror (get-board state))) YELLOW #f))))
            YELLOW
            (if (is-board-full (get-board state))
                3
                #f)))))
;; Task 2 - Euristică simplă
(define select-random-action
  (λ (state rand-gen)
    ;TODO
    (select-element (get-available-actions (get-board state)) (random (length (get-available-actions (get-board state))) rand-gen))))

(define select-element
  (λ (list pos)
    (car (drop list pos))))
      
(define action-helper
  (λ (state list player)
    (if (empty? list)
        -1
        (if (equal? player (is-game-over? (apply-action state (car list))))
            (car list)
            (action-helper state (cdr list) (get-player state))))))

(define player-switch
  (λ (p)
    (if (equal? p RED)
        YELLOW
        RED)))

(define get-action
  (λ (state)
    ;TODO
    (if (not (equal? (action-helper state (get-available-actions (get-board state)) (car state)) -1))
        (action-helper state (get-available-actions (get-board state)) (car state))
        (if (equal? (action-helper (cons (player-switch (car state)) (cdr state)) (get-available-actions (get-board state)) (player-switch (car state))) -1)
            (get-board state)
            (action-helper (cons (player-switch (car state)) (cdr state)) (get-available-actions (get-board state)) (player-switch (car state)))))))
        
            
;(fourth (get-available-actions (get-board state)))
(define AI #t) ;Trebuie modificat în #t după ce se implementează play-game

(define play-game
  (λ (state strategy1 strategy2)
    ;TODO
    (let ((l (play-game-help-1 state strategy1 strategy2 '())))
      (cons (reverse (cdr l)) (car l)))))

  (define play-game-help-1
    (λ (state strategy1 strategy2 result)
      (if (is-game-over? state)
          (cons (is-game-over? state) result)
          (let ((rnd  ((car strategy1) state (cdr strategy1))))
            (play-game-help-2 (apply-action state rnd) strategy1 strategy2 (cons rnd result))))))

  (define play-game-help-2
    (λ (state strategy1 strategy2 result)
      (if (is-game-over? state)
          (cons (is-game-over? state) result)
          (let ((rnd ((car strategy2) state (cdr strategy2))))
            (play-game-help-1 (apply-action state rnd) strategy1 strategy2 (cons rnd result))))))

;; Bonus 
(define evaluate
  (λ (state)
    ;TODO
    'your-code-here))

(define negamax
  (λ (state maxDepth)
    ;TODO
    'your-code-here))
     
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
