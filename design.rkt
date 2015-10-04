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
;;  -(make-circle string posn velocity string number string string boolean)
;;  -(make-rectangle string posn velocity number number string string boolean)
(define-struct gcircle (name cposn cvelocity radius type color jumprandom?))
(define-struct grectangle (name rposn rvelocity width height type color jumprandom?))

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
(define-struct until (something-happens name1 name2 not-happened happened))
(define-struct change-velocity (name newvelocity))
(define-struct collide-cond (collide? collided not-collided))

;;an animation is a list of commands
;;  -(make-animation list[cmd])
(define-struct animation (cmds))

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
    [(until? cmd)
     (begin (sleep/yield 0.25)
            (draw-canvas (update-canvas current-canvas))
            (cond
              [(not((until-something-happens cmd) (until-name1 cmd) (until-name2 cmd) current-canvas))
               (cond
                 [(empty? (until-not-happened cmd))(run-cmd cmd)]
                 [(begin (run-cmd cmd)(run-cmdlist (until-not-happened cmd)))])]
              [else (run-cmdlist (until-happened cmd))]))]
    
    [(change-velocity? cmd)(draw-canvas
                             (changevelocity (change-velocity-name cmd) 
                                             (change-velocity-newvelocity cmd) 
                                             (canvas-list-of-figures current-canvas)))]
    
    [(init-canvas? cmd)(draw-canvas (init-canvas-canvas cmd))]
    [(addgraphics? cmd)(draw-canvas (add-figure (addgraphics-figure cmd)))]
    [(jumprandom? cmd)(draw-canvas (jump-random  (jumprandom-name cmd) (canvas-list-of-figures current-canvas)))]
    [(jumpto? cmd)(draw-canvas (jump-to (jumpto-name cmd) (jumpto-posn cmd) (canvas-list-of-figures current-canvas)))]
    [(deletefigure? cmd)(draw-canvas (delete-figure (deletefigure-name cmd) (canvas-list-of-figures current-canvas)))])))

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
                                                      (gcircle-cposn (first alof))
                                                      (gcircle-cvelocity(first alof))
                                                      (gcircle-radius(first alof))
                                                      (gcircle-type(first alof))
                                                      (gcircle-color(first alof))
                                                      true)
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
                                        true))]
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
                (cond [(gcircle-jumprandom? acircle) (make-posn (random 500) (random 500))]
                      [else
                  (make-posn (+ (velocity-x (gcircle-cvelocity acircle)) (posn-x (gcircle-cposn acircle)))
                           (+ (velocity-y (gcircle-cvelocity acircle)) (posn-y (gcircle-cposn acircle))))])
                (gcircle-cvelocity acircle)
                (gcircle-radius acircle)
                (gcircle-type acircle)
                (gcircle-color acircle)
                (gcircle-jumprandom? acircle)))

;;change-position-rectangle: rectangle -> rectangle
(define (change-position-rectangle arectangle)
  (make-grectangle (grectangle-name arectangle)
                   (make-posn (+ (velocity-x (grectangle-rvelocity arectangle)) (posn-x (grectangle-rposn arectangle)))
                              (+ (velocity-y (grectangle-rvelocity arectangle)) (posn-y (grectangle-rposn arectangle))))
                   (grectangle-rvelocity arectangle)
                   (grectangle-width arectangle)
                   (grectangle-height arectangle)
                   (grectangle-type arectangle)
                   (grectangle-color arectangle)
                   (grectangle-jumprandom? arectangle)))
;;----------------------------------------------------------------------

;;collide? : string string -> boolean
(define (collide? figurename1 figurename2 acanvas)
  
  (let 
      ([figure1 (first (filter (lambda (afigure)
                                  (check-name afigure figurename1)) (canvas-list-of-figures acanvas)))]
        [figure2 (first (filter (lambda (afigure)
                                  (check-name afigure figurename2)) (canvas-list-of-figures acanvas)))])
      (check-posn figure1 figure2)))

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
   (< (abs (- (posn-x posn1) (posn-x posn2))) 15)
   (< (abs (- (posn-y posn1) (posn-y posn2))) 15)))

;;compare-posn-rectangles: posn posn -> boolean

;;compare-posn-mixed: posn posn number number-> boolean
(define (compare-posn-mixed posnc posnr width height)
   (or  (and (< (abs (- (posn-y posnc) (+ (posn-y posnr) (/ height 2)))) 5)
             (and (<= (posn-x posnc) (+ (posn-x posnr) (/ width 2))) 
                  (>= (posn-x posnc) (- (posn-x posnr) (/ width 2)))))
        (and (< (abs (- (posn-y posnc) (- (posn-y posnr) (/ height 2)))) 5)
             (and (<= (posn-x posnc) (+ (posn-x posnr) (/ width 2))) 
                  (>= (posn-x posnc) (- (posn-x posnr) (/ width 2)))))
        (and (< (abs (- (posn-x posnc) (- (posn-x posnr) (/ width 2)))) 5)
             (and (<= (posn-y posnc) (+ (posn-y posnr) (/ height 2))) 
                  (>= (posn-y posnc) (- (posn-y posnr) (/ height 2)))))
        (and (< (abs (- (posn-x posnc) (+ (posn-x posnr) (/ width 2)))) 5)
             (and (<= (posn-y posnc) (+ (posn-y posnr) (/ height 2))) 
                  (>= (posn-y posnc) (- (posn-y posnr) (/ height 2)))))))
     
;;collide-with-edge?: string string -> boolean  
(define (collide-with-edge? figurename edgename acanvas)
    (let ([figure1 (first (filter (lambda (afigure)
                                  (check-name afigure figurename)) (canvas-list-of-figures acanvas)))])
      (edge-collide figure1 edgename)))

;;edge-collide: figure string -> boolean
(define (edge-collide afigure edgename)
  (cond 
    [(gcircle? afigure)(edge-collide-circle afigure edgename)]
    [(grectangle? afigure)(edge-collide-rectangle afigure edgename)]))

(define (edge-collide-circle afigure edgename)
  (cond 
    [(string=? "left-edge" edgename)(< (abs (- (posn-x (gcircle-cposn afigure)) 0)) 10)]
    [(string=? "right-edge" edgename)(< (abs (- (posn-x (gcircle-cposn afigure)) 500)) 10)]
    [(string=? "top-edge" edgename)(< (abs (- (posn-y (gcircle-cposn afigure)) 0)) 10)]
    [(string=? "bottom-edge" edgename)(< (abs (- (posn-y (gcircle-cposn afigure)) 500)) 10)]))

(define (edge-collide-rectangle afigure edgename)
  (cond 
    [(string=? "left-edge" edgename)(< (abs (- (posn-x (grectangle-rposn afigure)) 0)) 10)]
    [(string=? "right-edge" edgename)(< (abs (- (posn-x (grectangle-rposn afigure)) 500)) 10)]
    [(string=? "top-edge" edgename)(< (abs (- (posn-y (grectangle-rposn afigure)) 0)) 10)]
    [(string=? "bottom-edge" edgename)(< (abs (- (posn-y (grectangle-cposn afigure)) 500)) 10)]))

;;changevelocity: string velocity listoffigures -> canvas
(define (changevelocity figurename avelocity alof)
  (cond 
    [(empty? alof) empty]
    [(gcircle? (first alof))
     (cond
       [(string=? (gcircle-name (first alof)) figurename)
        (make-canvas (cons (make-gcircle (gcircle-name(first alof))
                                         (gcircle-cposn (first alof))
                                         avelocity
                                         (gcircle-radius(first alof))
                                         (gcircle-type(first alof))
                                         (gcircle-color(first alof))
                                         (gcircle-jumprandom?(first alof)))
                           (rest alof)))]
       [else (make-canvas (append (list (first alof))
                                  (canvas-list-of-figures (changevelocity figurename avelocity (rest alof)))))])]
       
    [(grectangle? (first alof))
     (cond
       [(string=? (grectangle-name(first alof)) figurename)
        (make-canvas (cons (make-grectangle (grectangle-name(first alof))
                                                         (grectangle-rposn (first alof))
                                                         avelocity
                                                         (grectangle-width(first alof))
                                                         (grectangle-height(first alof))
                                                         (grectangle-type(first alof))
                                                         (grectangle-color(first alof))
                                                         (grectangle-jumprandom?(first alof)))
                                        (rest alof)))]
       [else (make-canvas (append (list (first alof))
                                  (canvas-list-of-figures (changevelocity figurename avelocity (rest alof)))))])]))

(big-bang WIDTH HEIGHT rate init-world)   

;;Animation 1
(define animation1
  (let ([red-circle
         (make-gcircle "redcircle"
                      (make-posn 50 100)
                      (make-velocity 5 5)
                      10
                      "solid"
                      "red" 
                      false)]
        [blue-rectangle
         (make-grectangle "bluerectangle"
                         (make-posn 300 250)
                         (make-velocity 0 0)
                         50
                         200
                         "solid"
                         "blue" 
                         false)])
    (make-animation
     (list (make-init-canvas (make-canvas (list red-circle
                                                blue-rectangle)))
           (make-until collide? "redcircle" "bluerectangle" empty (list 
                                                                   (make-deletefigure "bluerectangle")
                                                                   (make-change-velocity "redcircle" (make-velocity -5 2))
                                                                   (make-until collide-with-edge? "redcircle" "left-edge" empty empty)
                                                                   )))
      )))

;;Animation 2
(define animation2
  (let ([purple-circle
         (make-gcircle "purplecircle"
                      (make-posn 100 150)
                      (make-velocity 0 0)
                      20
                      "solid"
                      "purple"
                      true)])
    (make-animation
     (list (make-init-canvas (make-canvas (list purple-circle)))
           (make-until collide-with-edge? "purplecircle" "top-edge" (list (make-jumprandom "purplecircle")) empty))
           )))
           
;;Animation 3
(define animation3
  (let ([orange-circle
         (make-gcircle "orangecircle"
                      (make-posn 125 100)
                      (make-velocity 0 5)
                      10
                      "solid"
                      "orange"
                      false)]
        [green-rectangle
         (make-grectangle "greenrectangle"
                         (make-posn 250 400)
                         (make-velocity 0 0)
                         300
                         25
                         "solid"
                         "green"
                         false)]
        [red-rectangle
         (make-grectangle "redrectangle"
                         (make-posn 400 400)
                         (make-velocity 0 0)
                         50
                         100
                         "solid"
                         "red"
                         false)])
    (make-animation
     (list (make-init-canvas (make-canvas (list orange-circle
                                                green-rectangle)))
           (make-until collide? "orangecircle" 
                                 "greenrectangle" 
                                 empty
                                 (list (make-addgraphics red-rectangle)
                                       (make-change-velocity "orangecircle" (make-velocity 5 0))
                                       (make-until collide? "orangecircle" "redrectangle" empty (list (make-jumprandom "orangecircle")
                                                                                                      (make-deletefigure "redrectangle")))))))))                 

;;Animation 4
(define animation4
  (let ([black-circle
         (make-gcircle "blackcircle"
                      (make-posn 300 100)
                      (make-velocity 0 5)
                      10
                      "solid"
                      "black"
                      false)]
        [redcircle
         (make-gcircle "redcircle"
                       (make-posn 300 400)
                       (make-velocity 0 0)
                       10
                       "solid"
                       "red"
                       false)])
    (make-animation
     (list (make-init-canvas (make-canvas (list black-circle redcircle)))
           (make-until collide? "blackcircle" "redcircle"  empty 
                       (list (make-addgraphics (make-gcircle "yellowcircle"
                                                             (make-posn 200 400) 
                                                             (make-velocity 0 4) 
                                                             10 
                                                             "solid" 
                                                             "yellow" 
                                                             false))
                             (make-jumprandom "yellowcircle")
                             (make-until collide? "blackcircle" "yellowcircle" empty
                                         empty)))))))