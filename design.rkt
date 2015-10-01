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
;;  -(make-stopwhen string string) 
;;  -(make-changevelocity string velocity)
;;  -(make-collide-cond ((posn posn -> boolean) list[cmd]list[cmd]))
(define-struct init-canvas (canvas))
(define-struct addgraphics (figure))
(define-struct jumprandom (name))
(define-struct jumpto (name posn))
(define-struct deletefigure (name))
(define-struct stopwhen (name collidewith))
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
           (make-stopwhen "redcircle" "left-edge")                                                          
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
           (make-stopwhen "purplecircle" "top-edge")))))

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
           (make-stopwhen "blackcircle" "bottom-edge")))))

;;------------------------------------------------------------------
;;                             INTERPRETER
;;------------------------------------------------------------------
(define WIDTH 500)
(define HEIGHT 500)
(define init-world true)
(define rate 1/28)

(define current-canvas-scene true)
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
  (cond
    [(init-canvas? cmd)(draw-canvas (init-canvas-canvas cmd))]
    [(addgraphics? cmd)(add-figure (addgraphics-figure cmd))]
    [(jumprandom? cmd)(draw-canvas (jump-random  (jumprandom-name cmd) (canvas-list-of-figures current-canvas)))]
    [(jumpto? cmd)(draw-canvas (jump-to (jumpto-name cmd) (jumpto-posn cmd) (canvas-list-of-figures current-canvas)))]
    [(deletefigure? cmd)(draw-canvas (delete-figure (deletefigure-name cmd) (canvas-list-of-figures current-canvas)))]))

;;draw-canvas: canvas -> scene
;;updates the frame after initializing it with the figures
(define (draw-canvas acanvas)
  (begin
    (set! current-canvas acanvas)
    (set! current-canvas-scene (draw-figures (canvas-list-of-figures acanvas)))
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

;;add-figure: figure -> void
;;updates the frame with the new scene
(define (add-figure afigure)
  (begin
    (set! current-canvas (make-canvas (cons afigure (canvas-list-of-figures current-canvas))))
    (set! current-canvas-scene (draw-new-graphic afigure))
    (update-frame (draw-new-graphic afigure))))
               
;;draw-new-graphic: figure -> scene
;;adds a new graphic to the scene
(define (draw-new-graphic afigure)
  (place-image (draw-figure-image afigure)
               (draw-figure-posn afigure posn-x)
               (draw-figure-posn afigure posn-y)
               current-canvas-scene))

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
               


                            
























(big-bang WIDTH HEIGHT rate init-world)                                              
                  
        
         






