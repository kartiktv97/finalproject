;;Kartik Thooppal Vasu

#|
PROJECT REPORT
1. An animation in the proposed language is defined similarly to object definitions in C. The definition for an animation is done
as follows:
(animation animation-name {vars : [varname (definition)]
                           (animate (command1)
                                    (command2)...)})
Where the keywords are:
   a. animation
   b. vars : (please note that var requires a space before and after the ":"
   c. animate

The different commands that a programmer has access to are:
   a. velocity : (velocity x y) -> where x and y denote the rate of change of the position of the figure.
      Example: (velocity 2 4)

   b. position : (position x y) -> where x and y denote the x and y positions of the figure.
      Example: (position 250 250)

   c. circle: (circle circlename position velocity radius type color)
      Example: (circle orangecircle (position 125 100) (velocity 0 15) 10 "solid" "orange")

   d. rectangle: (rectangle rectanglename position velocity width height type color)
      Example: (rectangle purplerectangle (position 250 250) (velocity 15 15) 30 50 "solid" "purple")

   e. canvas: (canvas figure1 figure2 ...) -> used to make a canvas
      Example: (canvas (circle orangecircle (position 125 100) (velocity 0 15) 10 "solid" "orange")
                       (rectangle purplerectangle (position 250 250) (velocity 15 15) 30 50 "solid" "purple"))

   f. initialize: (initialize (canvas figure1 ...) -> used to initialize a canvas on the animation window
      Example: (initialize (canvas (circle orangecircle (position 125 100) (velocity 0 15) 10 "solid" "orange")
                       (rectangle purplerectangle (position 250 250) (velocity 15 15) 30 50 "solid" "purple")))
 
   g. delete: (delete figurename) -> used to delete a figure from the animation window
      Example: (delete orangecircle)

   h. change-velocity-to: (change-velocity-to figurename velocity) -> used to change the velocity of a given figure
      Example: (change-velocity-to redrectangle (velocity 5 4))

   i. addgraphics: (addgraphics figure) -> adds a figure to the given canvas
      Example: (addgraphics (circle orangecircle (position 125 100) (velocity 0 15) 10 "solid" "orange"))

   j. until: (until (name1 collideswith name2){cmd1 cmd2...}) -> performs the given commands in the curly paratheses until the two figures collide
      !!IMPORTANT!! If no commands except movement of the figures must take place, include moveanimation within parantheses as in the below example
      Example: (until (collide? redcircle collideswith bluerectangle) 
               {moveanimation})

   k. jumprandom: (jumprandom figurename) -> makes the figure jump to a random position on the canvas
      Example: (jumprandom redcircle)

   l. animate: (animate cmd1 ...) -> used to define the list of actions to be performed during the animation
      Example: (animate (initialize (canvas redcircle purplecircle)))

   m. animation: (animation animation-name {vars : [fname figuredefinition]
                                            (animate cmd1 cmd2...)})         -> used to define an animation and assign an animation with a name
      Example: (animation animation5 {vars : [blue-circle (circle bluecircle (position 250 250) (velocity 5 0) 10 "solid" "blue")]
                                             [raindrop (circle redcircle (position 30 30) (velocity 0 55) 20 "solid" "red")]
                                     (animate (initialize (canvas blue-circle raindrop))
                                              (until (collide-with-edge? bluecircle collideswith right-edge)
                                                     {(addgraphics raindrop)}))})

2. All the required functionalities as mentioned on the project webpage are working in the current submitted version of the project.
Added functionality includes the "jumpto" function that allows the programmer to specify a new location for a given figure to  jump to.

3. The following are the changes that were made in the design after the project design submission:
   a. Changed the name of the circle and rectangle structures from "circle" and "rectangle" 
   "gcircle" and "grectangle" respectively.

   b. Changed the identifiers for each figure from type "string" to type "symbol"

   c. Removed the commands "stop-when" and "collide-cond" and replaced with "until". I realised that I was trying to stick
     to the example slideshow program that was done in class a little too much by trying to add a collide-cond function. Also,
     the "until" command is more intuitive and understandable to the novice programmer.

4. I could've made the position checking for the collision simpler by just including a single function that compares the centers.
   However, collision with a rectangle, has a slightly more complicated set of checks that need to be done. Also, the animation does
   not look very accurate if we check for a circle and rectangle collision but it checks only if the centers of the two figures are 
   close. The code structure could have been made a little more edit-friendly to a person who has no prior knowledge of the program 
   requirements. 

Note: Animation 5 has the repeated block of commands (two addgraphics)
|#

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
;;  -(make-circle symbol posn velocity number string string boolean)
;;  -(make-rectangle symbol posn velocity number number string string boolean)
(define-struct gcircle (name cposn cvelocity radius type color jumprandom?))
(define-struct grectangle (name rposn rvelocity width height type color jumprandom?))

;;a canvas is
;;  -(make-canvas list-of-figures)
(define-struct canvas (list-of-figures))

;;a command is either:
;;  -(make-init-canvas canvas) 
;;  -(make-jumprandom symbol) 
;;  -(make-jumpto symbol posn) 
;;  -(make-addgraphics figure) 
;;  -(make-deletefigure symbol) 
;;  -(make-until (symbol symbol -> boolean) list[cmd] list[cmd])
;;  -(make-changevelocity symbol velocity)
(define-struct init-canvas (canvas))
(define-struct addgraphics (figure))
(define-struct jumprandom (name))
(define-struct jumpto (name posn))
(define-struct deletefigure (name))
(define-struct until (something-happens name1 name2 not-happened))
(define-struct change-velocity (name newvelocity))

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
              [(not ((until-something-happens cmd) (until-name1 cmd) (until-name2 cmd) current-canvas))
               (cond
                 [(empty? (until-not-happened cmd))(run-cmd cmd)]
                 [(begin (run-cmdlist (until-not-happened cmd))
                         (run-cmd cmd))])]))]    
    [(change-velocity? cmd)(draw-canvas
                             (changevelocity (change-velocity-name cmd) 
                                             (change-velocity-newvelocity cmd) 
                                             (canvas-list-of-figures current-canvas)))]    
    [(init-canvas? cmd)(draw-canvas (init-canvas-canvas cmd))]
    [(addgraphics? cmd)(draw-canvas (add-figure (addgraphics-figure cmd)))]
    [(jumprandom? cmd)(draw-canvas (update-canvas (jump-random  (jumprandom-name cmd) (canvas-list-of-figures current-canvas))))]
    [(jumpto? cmd)(draw-canvas (jump-to (jumpto-name cmd) (jumpto-posn cmd) (canvas-list-of-figures current-canvas)))]
    [(deletefigure? cmd)(draw-canvas (delete-figure (deletefigure-name cmd) (canvas-list-of-figures current-canvas)))])))

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
  (cond
    [(empty? current-canvas)(make-canvas(list afigure))]
    [else (make-canvas (cons afigure (canvas-list-of-figures current-canvas)))]))
               
;;jump-random: symbol list[figure]-> canvas
;;gives the figure with the given name a random position and returns the new canvas
(define (jump-random figurename alof)
  (cond
    [(empty? alof) empty]
    [(gcircle? (first alof))
     (cond
       [(symbol=? (gcircle-name (first alof)) figurename)
        (make-canvas (cons (jump-random-circle (first alof))(rest alof)))]
       [else (make-canvas (append (list (first alof))(canvas-list-of-figures (jump-random figurename (rest alof)))))])]
    [(grectangle? (first alof))
     (cond
       [(symbol=? (grectangle-name (first alof)) figurename)
        (make-canvas (cons (jump-random-rectangle (first alof))(rest alof)))]
       [else (make-canvas (append (list (first alof))(canvas-list-of-figures (jump-random figurename (rest alof)))))])]))

;;jump-random-circle: gcircle -> gcircle
;;makes the jumprandom 'true'
(define (jump-random-circle acircle)
(make-gcircle (gcircle-name acircle)
              (gcircle-cposn acircle)
              (gcircle-cvelocity acircle)
              (gcircle-radius acircle)
              (gcircle-type acircle)
              (gcircle-color acircle)
              true))

;;jump-random-rectangle: grectangle -> grectangle
;;makes the jumprandom 'true'
(define (jump-random-rectangle arectangle)
  (make-grectangle (grectangle-name arectangle)
                   (grectangle-rposn arectangle)
                   (grectangle-rvelocity arectangle)
                   (grectangle-width arectangle)
                   (grectangle-height arectangle)
                   (grectangle-type arectangle)
                   (grectangle-color arectangle)
                   true))

;;jump-to: symbol posn -> canvas
;;takes the name of a figure and a position to move the figure to and returns a new canvas
(define (jump-to figurename aposn alof)
  (cond
    [(empty? alof) empty]
    [(gcircle? (first alof))
     (cond
       [(symbol=? (gcircle-name (first alof)) figurename)
        (make-canvas (cons (make-gcircle (gcircle-name(first alof))
                                         aposn
                                         (gcircle-cvelocity(first alof))
                                         (gcircle-radius(first alof))
                                         (gcircle-type(first alof))
                                         (gcircle-color(first alof))
                                         (gcircle-jumprandom? (first alof)))
                           (rest alof)))]
       [else (make-canvas (append (list (first alof))
                                  (canvas-list-of-figures (jump-to figurename aposn (rest alof)))))])]
    [(grectangle? (first alof))
     (cond
       [(symbol=? (grectangle-name(first alof)) figurename)
        (make-canvas (cons (make-grectangle (grectangle-name(first alof))
                                            aposn
                                            (grectangle-rvelocity(first alof))
                                            (grectangle-width(first alof))
                                            (grectangle-height(first alof))
                                            (grectangle-type(first alof))
                                            (grectangle-color(first alof))
                                            (grectangle-jumprandom?(first alof)))
                                        (rest alof)))]
       [else (make-canvas (append (list (first alof))
                                (canvas-list-of-figures (jump-to figurename  aposn (rest alof)))))])]))

;;delete-figure: symbol list-of-figures-> canvas
;;takes the name and the list of figures and returns a canvas after deleting the figure with the given name
(define (delete-figure figurename alof)
  (make-canvas 
   (filter 
    (lambda (afigure)(cond [(gcircle? afigure) (not(symbol=? (gcircle-name afigure) figurename))]
                           [(grectangle? afigure) (not(symbol=? (grectangle-name afigure) figurename))])) 
    alof)))
 
;;update-canvas: canvas -> canvas
;;updates canvas by changing the position of the figures by given velocity
(define (update-canvas acanvas)
  (make-canvas (change-positions (canvas-list-of-figures acanvas))))

;;------------------------------------------------------------
;;                        FOR VELOCITY
;;------------------------------------------------------------
;;change-positions: list[figure] -> list[figure]
(define (change-positions alof)
  (map (lambda (afigure) (cond [(gcircle? afigure)(change-position-circle afigure)]
                               [(grectangle? afigure)(change-position-rectangle afigure)])) alof))

;;change-position-circle: circle -> circle
;;changes the position of the circle by the given velocity
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
;;changes the position of the rectangle by the given velocity
(define (change-position-rectangle arectangle)
  (make-grectangle (grectangle-name arectangle)
                   (cond [(grectangle-jumprandom? arectangle) (make-posn (random 500) (random 500))]
                         [else
                          (make-posn (+ (velocity-x (grectangle-rvelocity arectangle)) (posn-x (grectangle-rposn arectangle)))
                                     (+ (velocity-y (grectangle-rvelocity arectangle)) (posn-y (grectangle-rposn arectangle))))])
                   (grectangle-rvelocity arectangle)
                   (grectangle-width arectangle)
                   (grectangle-height arectangle)
                   (grectangle-type arectangle)
                   (grectangle-color arectangle)
                   (grectangle-jumprandom? arectangle)))
;;------------------------------------------------------------------------
;;                             COLLIDE CHECKS
;;-------------------------------------------------------------------------
;;collide? : symbol symbol -> boolean
;;checks if the given  figurenames collide
(define (collide? figurename1 figurename2 acanvas)
  (let 
      ([figure1 (first (filter (lambda (afigure)
                                 (check-name afigure figurename1)) (canvas-list-of-figures acanvas)))]
       [figure2 (first (filter (lambda (afigure)
                                 (check-name afigure figurename2)) (canvas-list-of-figures acanvas)))])
    (check-posn figure1 figure2)))

;;check-name figure symbol -> boolean
;;used to check if the name of the figure is the given figure name
(define (check-name afigure figurename)
  (cond
    [(and (gcircle? afigure) (symbol=? (gcircle-name afigure) figurename))]
    [(and (grectangle? afigure) (symbol=? (grectangle-name afigure) figurename))]
    [else false]))

;;check-posn: figure1 figure2 -> boolean
;;checks the positions of the two given figures
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
                                 (grectangle-rposn afigure2)
                                 (grectangle-width afigure1)
                                 (grectangle-height afigure1)
                                 (grectangle-width afigure2)
                                 (grectangle-height afigure2))])]))

;;compare-posn-circles: posn posn -> boolean
;;compares the positions of the two circles
(define (compare-posn-circles posn1 posn2)
  (and
   (< (abs (- (posn-x posn1) (posn-x posn2))) 15)
   (< (abs (- (posn-y posn1) (posn-y posn2))) 15)))

;;compare-posn-rectangles: posn posn number number number number-> boolean
(define (compare-posn-rectangles posn1 posn2 width1 height1 width2 height2)
  (or (and (< (abs (- (posn-x posn1) (posn-x posn2))) 10)
           (< (abs (- (posn-y posn2) (posn-y posn1))) 10))))

;;compare-posn-mixed: posn posn number number-> boolean
;;compares the positions of the circle and the rectangle
(define (compare-posn-mixed posnc posnr width height)
   (or  (and (< (abs (- (posn-y posnc) (+ (posn-y posnr) (/ height 2)))) 10)
             (and (<= (posn-x posnc) (+ (posn-x posnr) (/ width 2))) 
                  (>= (posn-x posnc) (- (posn-x posnr) (/ width 2)))))
        (and (< (abs (- (posn-y posnc) (- (posn-y posnr) (/ height 2)))) 10)
             (and (<= (posn-x posnc) (+ (posn-x posnr) (/ width 2))) 
                  (>= (posn-x posnc) (- (posn-x posnr) (/ width 2)))))
        (and (< (abs (- (posn-x posnc) (- (posn-x posnr) (/ width 2)))) 10)
             (and (<= (posn-y posnc) (+ (posn-y posnr) (/ height 2))) 
                  (>= (posn-y posnc) (- (posn-y posnr) (/ height 2)))))
        (and (< (abs (- (posn-x posnc) (+ (posn-x posnr) (/ width 2)))) 10)
             (and (<= (posn-y posnc) (+ (posn-y posnr) (/ height 2))) 
                  (>= (posn-y posnc) (- (posn-y posnr) (/ height 2)))))))
     
;;collide-with-edge?: symbol symbol -> boolean  
;;checks if the figure with the given name has collided with the edge
(define (collide-with-edge? figurename edgename acanvas)
    (let ([figure1 (first (filter (lambda (afigure)
                                  (check-name afigure figurename)) (canvas-list-of-figures acanvas)))])
      (edge-collide figure1 edgename)))

;;edge-collide: figure aymbol -> boolean
;;checks if the figure collides with the given edge
(define (edge-collide afigure edgename)
  (cond 
    [(gcircle? afigure)(edge-collide-circle afigure edgename)]
    [(grectangle? afigure)(edge-collide-rectangle afigure edgename)]))

;;edge-collide-circle: figure symbol -> boolean
;;determines whether the given circle has collided with the given edge
(define (edge-collide-circle afigure edgename)
  (cond 
    [(symbol=? 'left-edge edgename)(< (abs (- (posn-x (gcircle-cposn afigure)) 0)) 15)]
    [(symbol=? 'right-edge edgename)(< (abs (- (posn-x (gcircle-cposn afigure)) 500)) 15)]
    [(symbol=? 'top-edge edgename)(< (abs (- (posn-y (gcircle-cposn afigure)) 0)) 15)]
    [(symbol=? 'bottom-edge edgename)(< (abs (- (posn-y (gcircle-cposn afigure)) 500)) 15)]))

;;edge-collide-rectangle: figure symbol -> boolean
;;determines whether the given rectangle has collided with the given edge
(define (edge-collide-rectangle afigure edgename)
  (cond 
    [(symbol=? 'left-edge edgename)(< (abs (- (posn-x (grectangle-rposn afigure)) 0)) 15)]
    [(symbol=? 'right-edge edgename)(< (abs (- (posn-x (grectangle-rposn afigure)) 500)) 15)]
    [(symbol=? 'top-edge edgename)(< (abs (- (posn-y (grectangle-rposn afigure)) 0)) 15)]
    [(symbol=? 'bottom-edge edgename)(< (abs (- (posn-y (grectangle-cposn afigure)) 500)) 15)]))

;;changevelocity: symbol velocity listoffigures -> canvas
;;changes the figure with the given name to the velocity
(define (changevelocity figurename avelocity alof)
  (cond 
    [(empty? alof) empty]
    [(gcircle? (first alof))
     (cond
       [(symbol=? (gcircle-name (first alof)) figurename)
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
       [(symbol=? (grectangle-name(first alof)) figurename)
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

;;-------------------------------------------------------------------
;;                             MACROS
;;-------------------------------------------------------------------
(define-syntax velocity
  (syntax-rules()
    [(velocity x y)
     (make-velocity x y)]))

(define-syntax position
  (syntax-rules()
    [(position x y)
     (make-posn x y)]))

(define-syntax circle
  (syntax-rules()
    [(circle name position velocity radius type color)
     (make-gcircle 'name position velocity radius type color false)]))

(define-syntax rectangle
  (syntax-rules()
    [(rectangle name position velocity width height type color)
     (make-grectangle 'name position velocity width height type color false)]))

(define-syntax canvas
  (syntax-rules()
    [(canvas fig1 ...)
     (make-canvas (list fig1 ...))]))

(define-syntax initialize
  (syntax-rules()
    [(initialize canvas)
     (make-init-canvas canvas)]))

(define-syntax animate
  (syntax-rules()
    [(animate cmd1 ...)
     (make-animation (list cmd1 ...))]))

(define-syntax delete
  (syntax-rules()
    [(delete name)
     (make-deletefigure 'name)]))

(define-syntax change-velocity-to
  (syntax-rules()
    [(change-velocity-to name velocity)
     (make-change-velocity 'name velocity)]))

(define-syntax jumprandom
  (syntax-rules()
    [(jumprandom name)
     (make-jumprandom 'name)]))

(define-syntax jumpto
  (syntax-rules()
    [(jumpto name position)
     (make-jumpto 'name position)]))

(define-syntax addgraphics
  (syntax-rules()
    [(addgraphics figure)
     (make-addgraphics figure)]))

(define-syntax until
  (syntax-rules(collideswith)
    [(until (name1 collideswith name2) {cmd1 ...})
      (cond [(or (symbol=? 'name2 'top-edge)(symbol=? 'name2 'left-edge)(symbol=? 'name2 'bottom-edge)(symbol=? 'name2 'right-edge))
             (make-until collide-with-edge? 'name1 'name2 (cond [(symbol? '(first (list cmd1 ...))) empty] [else (list cmd1 ...)]))]
            [else (make-until collide? 'name1 'name2 (cond [(symbol? '(first (list cmd1 ...))) empty] [else (list cmd1 ...)]))])]))            

(define-syntax animation
  (syntax-rules (vars actions)
    [(animation aname {vars : [name figure]...
                 animate})
     (define aname (let ([name figure]...)
                     animate))]))

(define moveanimation false)

;;Animation 1
(animation animation1 {vars : [red-circle (circle redcircle (position 50 100) (velocity 20 20) 10 "solid" "red")]
        [blue-rectangle (rectangle bluerectangle (position 300 250) (velocity 0 0)  50 200 "solid" "blue")]
        (animate (initialize (canvas red-circle blue-rectangle))
             (until (redcircle collideswith bluerectangle) {moveanimation})
             (delete bluerectangle)
             (change-velocity-to redcircle (velocity -20 10))
             (until (redcircle collideswith left-edge) {moveanimation}))})
  

;;Animation 2
(animation animation2 {vars : [purple-circle (circle purplecircle (position 100 150) (velocity 0 0) 20 "solid" "purple")]
                         (animate (initialize (canvas purple-circle)) 
             (until (purplecircle collideswith top-edge)
                    {(jumprandom purplecircle)}))})
           
;;Animation 3
(animation animation3 {vars : [orange-circle (circle orangecircle (position 125 100) (velocity 0 15) 10 "solid" "orange")]
                            [green-rectangle (rectangle greenrectangle (position 250 400) (velocity 0 0) 300 25 "solid" "green")]
                            [red-rectangle (rectangle redrectangle (position 375 350) (velocity 0 0) 50 100 "solid" "red")]
                            (animate (initialize (canvas orange-circle green-rectangle))
                                     (until (orangecircle collideswith greenrectangle) {moveanimation})
                                     (addgraphics red-rectangle)
                                     (change-velocity-to orangecircle (velocity 15 0))
                                     (until (orangecircle collideswith redrectangle) {moveanimation})
                                     (jumprandom orangecircle)
                                     (delete redrectangle))})
                                     

;;Animation 4
(animation animation4 {vars : [black-circle (circle blackcircle (position 300 100) (velocity 0 5) 10 "solid" "black")]
                              [redcircle (circle redcircle (position 300 400) (velocity 0 0) 10 "solid" "red")]
                              (animate (initialize (canvas black-circle redcircle))
             (until (blackcircle collideswith redcircle) {moveanimation})
             (addgraphics (circle yellowcircle (position 200 400) (velocity 0 4) 10 "solid" "yellow"))
             (jumprandom yellowcircle)
             (until (blackcircle collideswith yellowcircle) {moveanimation}))})


;;Animation 5
(animation animation5 {vars : [blue-circle (circle bluecircle (position 250 250) (velocity 5 0) 10 "solid" "blue")]
                               [raindrop (circle redcircle (position 30 30) (velocity 0 55) 20 "solid" "red")]
                               [greenrectangle (rectangle greenrectangle (position 100 100) (velocity 55 0) 10 5 "solid" "green")]
                        (animate (initialize (canvas blue-circle raindrop))
                                 (until (bluecircle collideswith right-edge)
                                        {(addgraphics raindrop)
                                         (addgraphics greenrectangle)})
                                 (jumpto bluecircle (position 250 250)))})



                         
                                     
                                     
                                                                                             
