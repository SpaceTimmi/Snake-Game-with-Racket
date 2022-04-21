;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |Snake Game With Fruit|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; Snake Game in Beginning Student Language (BSL)
;; Start world with -> (main START-GAME) after runnining the program
;; Movement: Arrow keys

;; =================
;; Constants:
(define WIDTH 600)
(define HEIGHT 400)
(define SCRN-COLOR "navy")
(define SCREEN (rectangle WIDTH HEIGHT "solid" SCRN-COLOR))
(define MOVEMENT-SPEED 10)
(define SNAKE (square MOVEMENT-SPEED "solid" "orange"))
(define APPLE (square MOVEMENT-SPEED "solid" "red"))




;; =================
;; Data definitions:
(define-struct position (x y))
;; position is (make-position Integer[1, WIDTH] Integer[1, HEIGHT])
;; interp. the x and y axis position of the snake pixel

;;Examples:
(define P1 (make-position 20 50))
(define P2 (make-position 20 40))
(define P3 (make-position 20 30))
(define P4 (make-position 20 20))
(define LAUNCH-POSITION (make-position (/ WIDTH 2) (/ HEIGHT 2)))

(define (fn-for-position p)
  (... (position-x p)
       (position-y p)))



;; ListOfPositions is one of:
;; - empty
;; - (cons ListOfPositions empty)

;;Example:
(define LOP1 (cons P1 (cons P2 (cons P3 (cons P4 empty)))))
(define START (cons LAUNCH-POSITION empty))
(define STARTING-POSITION (make-position (/ WIDTH 2) (/ HEIGHT 2)))

(define (fn-for-lop lop)
  (cond [(empty? lop) (...)]
        [else
         (... (first lop)
              (fn-for-lop(rest lop)))]))



(define-struct snake (lop direction apple))
;; Snake is (make-snake (listofpositions) String)
;; interp. the current state of the snake with the previous positions the snake has passed through
;;         and its current direction
;;         direction - direction can be one of four strings:
;;                   - "up"    (constant x position decreasing y position)
;;                   - "down"  (constant x position increasing y position)
;;                   - "right" (constant y position increasing x position)
;;                   - "left"  (constant y position decreasing x position)
;;         apple - this is the apple on-screen that increases the lenght of the snake
(define apple (make-position 100 90))

;;Example:
(define S2 (make-snake LOP1 "down" apple))
(define S3 (make-snake LOP1 "right" apple))
(define S4 (make-snake LOP1 "left" apple))
(define S5 (make-snake LOP1 "up" apple))


(define (fn-for-snake s)
  (... (snake-lop s)
       (snake-direction s)
       (snake-apple s)))

;; Launch
(define START-GAME (make-snake START "right" STARTING-POSITION))


;; =================
;; Functions:

;; Snake -> Snake
;; start the world with (main START-GAME)

(define (main s)
  (big-bang s                    ; Snake
    (on-tick   update-snake)     ; Snake -> Snake
    (stop-when end-game?)        ; Snake -> End
    (to-draw   render-snake)     ; Snake -> Image
    (on-key    move-snake)))     ; Snake KeyEvent -> Snake    


;; Snake -> Snake
;; Add a new snake position to the ListOfPositions depending on the 'direction' given and Remove the last element in the list.


;;(define (update-snake s) s) ;stub
(define (update-snake s)
  (if (contains? (snake-apple s) (snake-lop s))
      (cond [(string=? (snake-direction s) "up")    (make-snake (increase-snake (snake-lop s) "y" (- MOVEMENT-SPEED)) "up" (new-apples (snake-lop s)))]
            [(string=? (snake-direction s) "down")  (make-snake (increase-snake (snake-lop s) "y" MOVEMENT-SPEED) "down" (new-apples (snake-lop s)))]
            [(string=? (snake-direction s) "right") (make-snake (increase-snake (snake-lop s) "x" MOVEMENT-SPEED) "right" (new-apples (snake-lop s)))]
            [(string=? (snake-direction s) "left")  (make-snake (increase-snake (snake-lop s) "x" (- MOVEMENT-SPEED)) "left" (new-apples (snake-lop s)))]
            [else s])
      
      (cond [(string=? (snake-direction s) "up")    (make-snake (add-snake (snake-lop s) "y"  (- MOVEMENT-SPEED))   "up"    (snake-apple s))]
            [(string=? (snake-direction s) "down")  (make-snake (add-snake (snake-lop s) "y"  MOVEMENT-SPEED)       "down"  (snake-apple s))]
            [(string=? (snake-direction s) "right") (make-snake (add-snake (snake-lop s) "x"  MOVEMENT-SPEED)       "right" (snake-apple s))]
            [(string=? (snake-direction s) "left")  (make-snake (add-snake (snake-lop s) "x"  (- MOVEMENT-SPEED))   "left"  (snake-apple s))]
            [else s])))
;; - "up"    (constant x position decreasing y position)
;; - "down"  (constant x position increasing y position)
;; - "right" (constant y position increasing x position)
;; - "left"  (constant y position decreasing x position)



;;(define (add-snake lop axis dir) lop) ;stub
;; adds a new positon to the ListOfPosition and removes the last position from the list, os the length of the snake stays the same
(define (add-snake lop axis dir)
  (cond [(string=? axis "x") (rearrange-snake (cons (make-position (+ (position-x (first lop)) dir)
                                                              (position-y (first lop)))
                                               lop))]
        [(string=? axis "y") (rearrange-snake (cons (make-position (position-x (first lop)) 
                                                              (+ (position-y (first lop)) dir))
                                               lop))]))

;; takes a ListOfPositions and removes the last element in the list.
(define (rearrange-snake lop)
  (reverse (rest (reverse lop))))


;; adds a new position to the ListOfPosition without removing the last position from the list, so the length of the snake increases.
(define (increase-snake lop axis dir)
  (cond [(string=? axis "x") (cons (make-position (+ (position-x (first lop)) dir)
                                                  (position-y (first lop)))
                                    lop)]
        [(string=? axis "y") (cons (make-position (position-x (first lop)) 
                                                  (+ (position-y (first lop)) dir))
                                    lop)]))


;; returns 'true' if apple has been taken by the snake.
(define (contains? apple lop)
  (if (and
       (= (position-x (first lop)) (position-x apple))
       (= (position-y (first lop)) (position-y apple)))
      true
      false))


;; randomly generates a new position for the apple if it is taken
(define (new-apples lop)
  (make-position (format-rand (random WIDTH)) (format-rand (random HEIGHT))))

;; takes the randomly generated number, runs modulo on the number with the MOVEMENT-SPEED as the third argument and subtracts the result 
;; from the originally generated number. I did this so that the size and position of the apple is consistent with the size of the snake (MOVEMENT-SPEED)
;; Example: if the MOVEMENT-SPEED is 10 then the position of the apple should be in 10s ( 10 20 30 40 50 etc).

(define (format-rand random-number)
  (+ MOVEMENT-SPEED (- random-number (modulo random-number MOVEMENT-SPEED))))

  

;; Snake -> Image
;; render the ListOfPositions on the Screen
  
#;
(define (render-snake s) SCREEN) ;stub

;; render-snake takes in Snake and sends the ListOfPostion to display-snake
(define (render-snake s)
  (display-snake (snake-lop s) (snake-apple s) (- (length (snake-lop s)) 2)))

;; display-snake takes in ListOfPositions and displays the x and y positions on-screen
(define (display-snake lop apple len)
  (cond [(empty? lop) (overlay
                       (text (number->string len) 40 "black")
                       (place-image APPLE (position-x apple) (position-y apple) SCREEN))]
        [else
         (place-image SNAKE
                      (position-x (first lop))
                      (position-y (first lop))
                      (display-snake (rest lop) apple len))]))



;; Snake KeyEvent -> Snake
;; on key press change the direction of the snake

(define (move-snake s ke)
  (cond [(key=? ke "up")    (make-snake (snake-lop s)
                                        (validate-input (snake-direction s) "up"    (get-opposite (snake-direction s)))
                                        (snake-apple s))]
        
        [(key=? ke "down")  (make-snake (snake-lop s)
                                        (validate-input (snake-direction s) "down"  (get-opposite (snake-direction s)))
                                        (snake-apple s))]
        
        [(key=? ke "right") (make-snake (snake-lop s)
                                        (validate-input (snake-direction s) "right" (get-opposite (snake-direction s)))
                                        (snake-apple s))]
        
        [(key=? ke "left")  (make-snake (snake-lop s)
                                        (validate-input (snake-direction s) "left"  (get-opposite (snake-direction s)))
                                        (snake-apple s))]
        [else s]))


;;prev-dir -> previous direction
;;new-dir  -> new direction
;;opp-dir  -> opposite direction (of previous direction. eg if prev-dir is "right" then opp-dir is "left")
;; validate-input makes sure the new-dir does not result in a 180 turn of the snake
(define (validate-input prev-dir new-dir opp-dir)
  (if (string=? new-dir opp-dir)
      prev-dir
      new-dir))


;; get-opposite returns the opposite of a given direction eg. if given "right" it will return "left"
(define (get-opposite direction)
  (cond [(string=? direction "right") "left"]
        [(string=? direction "left")  "right"]
        [(string=? direction "up")    "down"]
        [(string=? direction "down")  "up"]))
      


;; Snake -> End
;; Checks if the snake has crossed the boundaries of the screen or if the snake hits itself and then ends the game
(define (end-game? s)
  (or
   (boundary-check? (snake-lop s))
   (snake-hits-itself? (snake-lop s))))

;; Checks if the snake hits the boundary
(define (boundary-check? lop)
  (if (or
       (> (position-x (first lop)) WIDTH)
       (< (position-x (first lop)) 1)
       (> (position-y (first lop)) HEIGHT)
       (< (position-y (first lop)) 1))
      true
      false))

;; Checks if the snake hits itself
(define (snake-hits-itself? lop)
  (verify-hit? (first lop) (rest lop)))

(define (verify-hit? head lop)
  (cond [(empty? lop) false]
        [else
         (if (and
              (= (position-x head) (position-x (first lop)))
              (= (position-y head) (position-y (first lop))))
             true
             (verify-hit? head (rest lop)))]))

;; Start world with -> (main START-GAME) after runnining the program
;; Movement: Arrow keys
