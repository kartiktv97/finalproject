;;Kartik Thooppal Vasu
;;Final project animation
(require "world-cs1102.rkt")
(require 2htdp/image)

;;a velocity is
;;  -(make-velocity number number)
(define-struct velocity (x y))

;;a posn is
;;  -(make-posn number number)

;;a figure is either
;;  -(make-circle string posn velocity string number string string)
;;  -(make-rectangle string posn velocity number number string string)
(define-struct gcircle (name cposn cvelocity radius type color))
(define-struct grectangle (name rposn rvelocity width height type color))

;;a canvas is
;;  -(make-canvas list-of-figures)
(define-struct canvas (list-of-figures))

;;a command is either:
;;  -(make-init-canvas canvas) --> done
;;  -(make-jumprandom string) --> done
;;  -(make-jumpto string posn) --> done
;;  -(make-addgraphics figure) --> done
;;  -(make-deletefigure string) --> done
;;  -(make-until string string -> boolean) list[cmd] list[cmd])
;;  -(make-changevelocity string velocity)
;;  -(make-collide-cond ((posn posn -> boolean) list[cmd]list[cmd]))
(define-struct init-canvas (canvas))
(define-struct addgraphics (figure))
(define-struct jumprandom (name))
(define-struct jumpto (name posn))
(define-struct deletefigure (name))
(define-struct until (something-happens not-happened happened))
(define-struct change-velocity (name newvelocity))
(define-struct collide-cond (collide? collided not-collided))

;;an animation is a list of commands
;;  -(make-animation list[cmd])
(define-struct animation (cmds))

;;----------------------------------------------------------------
;;                         EXAMPLE ANIMATIONS
;;----------------------------------------------------------------
;;Animation 1
(define animation1
  (let ([red-circle
         (make-gcircle "redcircle"
                      (make-posn 50 100)
                      (make-velocity 2 2)
                      10
                      "solid"
                      "red")]
        [blue-rectangle
         (make-grectangle "bluerectangle"
                         (make-posn 300 250)
                         (make-velocity 0 0)
                         50
                         200
                         "solid"
                         "blue")])
    (make-animation
     (list (make-init-canvas (make-canvas (list red-circle
                                                blue-rectangle)))
           (make-collide-cond (lambda (posn1 posn2) (and (= (posn-x posn1) (posn-x posn2))
                                                         (= (posn-y posn2) (posn-y posn2))))
                              (list (make-deletefigure "bluerectangle")
                                    (make-change-velocity "redcircle" (make-velocity -2 -2)))
                              empty)
                                                                  
      ))))

;;Animation 2
(define animation2
  (let ([purple-circle
         (make-gcircle "purplecircle"
                      (make-posn 100 150)
                      (make-velocity 0 0)
                      20
                      "solid"
                      "purple")])
    (make-animation
     (list (make-init-canvas (make-canvas (list purple-circle)))
           (make-jumprandom "purplecircle")
           ))))

;;Animation 3
(define animation3
  (let ([orange-circle
         (make-gcircle "orangecircle"
                      (make-posn 125 100)
                      (make-velocity 0 5)
                      10
                      "solid"
                      "orange")]
        [green-rectangle
         (make-grectangle "greenrectangle"
                         (make-posn 250 400)
                         (make-velocity 0 0)
                         300
                         25
                         "solid"
                         "green")]
        [red-rectangle
         (make-grectangle "redrectangle"
                         (make-posn 100 10)
                         (make-velocity 0 0)
                         50
                         100
                         "solid"
                         "red")])
    (make-animation
     (list (make-init-canvas (make-canvas (list orange-circle
                                                green-rectangle)))
           (make-collide-cond (lambda (posn1 posn2) (and (= (posn-x posn1) (posn-x posn2))
                                                         (= (posn-y posn2) (posn-y posn2))))
                              (list (make-change-velocity "orangecircle" (make-velocity 5 0))
                                    (make-addgraphics red-rectangle))
                              empty)
           
           (make-collide-cond (lambda (posn1 posn2) (and (= (posn-x posn1) (posn-x posn2))
                                                         (= (posn-y posn2) (posn-y posn2))))
                              (list (make-jumprandom "orangecircle"))
                              empty)))))
                                    

;;Animation 4
(define animation4
  (let ([black-circle
         (make-gcircle "blackcircle"
                      (make-posn 300 100)
                      (make-velocity 0 5)
                      10
                      "solid"
                      "black")])
    (make-animation
     (list (make-init-canvas (make-canvas (list black-circle)))
           ))))

;;------------------------------------------------------------------
;;                             INTERPRETER
;;------------------------------------------------------------------
(define WIDTH 500)
(define HEIGHT 500)
(define init-world true)
(define rate 1/28)


(define current-canvas empty)

;;run-animation: animation -> void
;;executes the commands in animation
(define (run-animation animat)
  (run-cmdlist (animation-cmds animat)))

;;run-cmdlist: cmdlist -> void
;;executes each command in the command list
(define (run-cmdlist cmdlist)
  (for-each run-cmd cmdlist))

;;run-cmd: cmd -> void
;;executes the command that is passed as an argument
(define (run-cmd cmd)
  (begin (cond
    [(init-canvas? cmd)(draw-canvas (init-canvas-canvas cmd))]
    [(addgraphics? cmd)(draw-canvas (add-figure (addgraphics-figure cmd)))]
    [(jumprandom? cmd)(draw-canvas (jump-random  (jumprandom-name cmd) (canvas-list-of-figures current-canvas)))]
    [(jumpto? cmd)(draw-canvas (jump-to (jumpto-name cmd) (jumpto-posn cmd) (canvas-list-of-figures current-canvas)))]
    [(deletefigure? cmd)(draw-canvas (delete-figure (deletefigure-name cmd) (canvas-list-of-figures current-canvas)))])
         (loopcanvas current-canvas)))

;;loopcanvas draws an update canvas every 0.25 seconds
(define (loopcanvas acanvas)
  (begin (sleep/yield 0.25) 
         (draw-canvas (update-canvas current-canvas))
         (loopcanvas current-canvas)))

;;draw-canvas: canvas -> scene
;;updates the frame after initializing it with the figures
(define (draw-canvas acanvas)
  (begin
    (set! current-canvas acanvas)
    (update-frame (draw-figures (canvas-list-of-figures acanvas)))))

;;draw-figures: list of figures -> scene
;;takes a list of figures to draw 
(define (draw-figures alof)
  (cond
    [(empty? alof) (empty-scene WIDTH HEIGHT)]
    [(cons? alof) (place-image (draw-figure-image (first alof))
                               (draw-figure-posn (first alof) posn-x)
                               (draw-figure-posn (first alof) posn-y)
                               (draw-figures (rest alof)))]))

;;draw-figure-image: figure -> image
;;takes a figure to return an image
(define (draw-figure-image afigure)
  (cond
    [(gcircle? afigure)(circle (gcircle-radius afigure)
                              (gcircle-type afigure)
                              (gcircle-color afigure))]
    [(grectangle? afigure) (rectangle (grectangle-width afigure)
                                     (grectangle-height afigure)
                                     (grectangle-type afigure)
                                     (grectangle-color afigure))]))

;;draw-figure-posn: figure function -> number
;;returns the specified position
(define (draw-figure-posn afigure parameter)
  (cond
    [(gcircle? afigure)(parameter (gcircle-cposn afigure))]
    [(grectangle? afigure) (parameter (grectangle-rposn afigure))]))

;;add-figure: figure -> canvas
;;updates the frame with the new scene
(define (add-figure afigure)
  (begin
    (make-canvas (cons afigure (canvas-list-of-figures current-canvas)))
    ))
               
;;jump-random: string -> canvas
;;gives the figure with the given name a random position and returns the new canvas
(define (jump-random figurename alof)
  (cond
    [(empty? alof) empty]
    [(gcircle? (first alof))
     (cond
       [(string=? (gcircle-name (first alof)) figurename)
        (make-canvas (cons (make-gcircle (gcircle-name(first alof))
                                                      (make-posn (random 500) (random 500))
                                                      (gcircle-cvelocity(first alof))
                                                      (gcircle-radius(first alof))
                                                      (gcircle-type(first alof))
                                                      (gcircle-color(first alof)))
                           (rest alof)))]
       [else (make-canvas (append (list (first alof))
                                  (canvas-list-of-figures (jump-random figurename (rest alof)))))])]
    [(grectangle? (first alof))
     (cond
       [(string=? (grectangle-name(first alof)) figurename)
        (make-canvas (cons (make-grectangle (grectangle-name(first alof))
                                                         (make-posn (random 500) (random 500))
                                                         (grectangle-rvelocity(first alof))
                                                         (grectangle-width(first alof))
                                                         (grectangle-height(first alof))
                                                         (grectangle-type(first alof))
                                                         (grectangle-color(first alof)))
                                        (rest alof)))]
       [else (make-canvas (cons (first alof)
                                (canvas-list-of-figures (jump-random figurename (rest alof)))))])]))

;;jump-to: string posn -> canvas
;;takes the name of a figure and a position to move the figure to and returns a new canvas
(define (jump-to figurename aposn alof)
  (cond
    [(empty? alof) empty]
    [(gcircle? (first alof))
     (cond
       [(string=? (gcircle-name (first alof)) figurename)
        (make-canvas (cons (make-gcircle (gcircle-name(first alof))
                                         aposn
                                         (gcircle-cvelocity(first alof))
                                         (gcircle-radius(first alof))
                                         (gcircle-type(first alof))
                                         (gcircle-color(first alof)))
                           (rest alof)))]
       [else (make-canvas (append (list (first alof))
                                  (canvas-list-of-figures (jump-to figurename (rest alof)))))])]
    [(grectangle? (first alof))
     (cond
       [(string=? (grectangle-name(first alof)) figurename)
        (make-canvas (cons (make-grectangle (grectangle-name(first alof))
                                            aposn
                                            (grectangle-rvelocity(first alof))
                                            (grectangle-width(first alof))
                                            (grectangle-height(first alof))
                                            (grectangle-type(first alof))
                                            (grectangle-color(first alof)))
                                        (rest alof)))]
       [else (make-canvas (cons (first alof)
                                (canvas-list-of-figures (jump-to figurename (rest alof)))))])]))

;;delete-figure: string list-of-figures-> canvas
;;takes the name and the list of figures and returns a canvas after deleting the figure with the given name
(define (delete-figure figurename alof)
  (cond
    [(empty? alof) empty]
    [(gcircle? (first alof))
     (cond 
       [(string=? (gcircle-name(first alof)) figurename)
        (make-canvas (rest alof))]
       [else (make-canvas (cons (first alof)
                                (canvas-list-of-figures (delete-figure figurename (rest alof)))))])]
    [(grectangle? (first alof))
     (cond
       [(string=? (grectangle-name(first alof)) figurename)
        (make-canvas (rest alof))]
       [else (make-canvas (cons (first alof)
                                (canvas-list-of-figures (delete-figre figurename (rest alof)))))])]))

;;update-canvas: canvas -> canvas
;;updates canvas by changing the position of the figures by given velocity
(define (update-canvas acanvas)
  (make-canvas (change-positions (canvas-list-of-figures acanvas))))

;;------------------------------------------------------------
;;                        FOR VELOCITY
;;------------------------------------------------------------
;;change-positions: list of figures -> list-of-figures
(define (change-positions alof)
  (cond
    [(empty? alof) empty]
    [(gcircle? (first alof))(cons (change-position-circle (first alof)) (change-positions (rest alof)))]
    [(grectangle? (first alof))(cons (change-position-rectangle (first alof)) (change-positions (rest alof)))]))

;;change-position-circle: circle -> circle
(define (change-position-circle acircle)
  (make-gcircle (gcircle-name acircle)
                (make-posn (+ (velocity-x (gcircle-cvelocity acircle)) (posn-x (gcircle-cposn acircle)))
                           (+ (velocity-y (gcircle-cvelocity acircle)) (posn-y (gcircle-cposn acircle))))
                (gcircle-cvelocity acircle)
                (gcircle-radius acircle)
                (gcircle-type acircle)
                (gcircle-color acircle)))

;;change-position-rectangle: rectangle -> rectangle
(define (change-position-rectangle arectangle)
  (make-grectangle (grectangle-name arectangle)
                   (make-posn (+ (velocity-x (grectangle-rvelocity arectangle)) (posn-x (grectangle-rposn arectangle)))
                              (+ (velocity-y (grectangle-rvelocity arectangle)) (posn-y (grectangle-rposn arectangle))))
                   (grectangle-rvelocity arectangle)
                   (grectangle-width arectangle)
                   (grectangle-height arectangle)
                   (grectangle-type arectangle)
                   (grectangle-color arectangle)))
;;----------------------------------------------------------------------

;;collide? : string string -> boolean
(define (collide? figurename1 figurename2)
  (let ([figure1 (first (filter (lambda (afigure)
                                  (check-name afigure figurename1)) (canvas-list-of-figures acanvas)))]
        [figure2 (first (filter (lambda (afigure)
                                  (check-name afigure figurename1)) (canvas-list-of-figures acanvas)))])
    (check-posn-close figure1 figure2)))

;;check-name figure string -> boolean
(define (check-name afigure figurename)
  (cond
    [(and (gcircle? afigure) (string=? (gcircle-name afigure) figurename))]
    [(and (grectangle? afigure) (string=? (grectangle-name afigure) figurename))]
    [else false]))

;;check-posn: figure1 figure2 -> boolean
(define (check-posn afigure1 afigure2)
  (cond
    [(gcircle? afigure1)
     (cond
       [(gcircle? afigure2)
        (compare-posn-circles (gcircle-cposn afigure1)
                              (gcircle-cposn afigure2))]
       [(grectangle? afigure2)
        (compare-posn-mixed (gcircle-cposn afigure1) 
                            (grectangle-rposn afigure2) 
                            (grectangle-width afigure2) 
                            (grectangle-height afigure2))])]
    [(grectangle? afigure1)
     (cond
       [(gcircle? afigure2)
        (compare-posn-mixed (gcircle-cposn afigure2) 
                            (grectangle-rposn afigure1)
                            (grectangle-width afigure1)
                            (grectangle-height afigure1))]
       [(grectangle? afigure2)
        (compare-posn-rectangles (grectangle-rposn afigure1) 
                                 (grectangle-rposn afigure2))])]))

;;compare-posn-circles: posn posn -> boolean
(define (compare-posn-circles posn1 posn2)
  (and
   (< (- (posn-x posn1) (posn-x posn2)) 5)
   (< (- (posn-y posn2) (posn-y posn2)) 5)))

;;compare-posn-rectangles: posn posn -> boolean

;;compare-posn-mixed: posn posn -> boolean
    
  

(big-bang WIDTH HEIGHT rate init-world)                                              
                  
        
         






