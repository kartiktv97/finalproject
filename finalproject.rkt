;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname finalproject) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;;Final project animation

;;a velocity is
;;  -(make-velocity number number)
(define-struct velocity (x y))

;;a posn is
;;  -(make-posn number number)

;;a figure is either
;;  -(make-circle string posn velocity string number string)
;;  -(make-rectangle string posn velocity number number string string)
(define-struct circle (name cposn cvelocity radius type color))
(define-struct rectangle (name rposn rvelocity width height type color))

;;a stop-if is:
;;  -(make-stop-if hit)
;; where hit is a list of strings containing these: 
;;  -"right-edge"
;;  -"left-edge"
;;  -"top-edge"
;;  -"bottom-edge"
;; OR
;;  -name of figure (string)
(define-struct stop-if (hit))

;;a window is:
;;  -(make-window number number)
(define-struct window (height width))

;;a command is either:
;;  -(make-displaywindow window)
;;  -(make-displayfigure figure)
;;  -(make-stop-if string)
(define-struct displayfigure (figure))
(define-struct displaywindow (window))

;;an animation is a list of commands
;;  -(make-animation list[cmd])
(define-struct animation (cmds))








